# Impute missing data for individuals with CRP and IL-6 blood data

# ----------------------------------------------------------------------------------------
# We used multiple imputation using chained equations (using the "mice" package in R) to replace missing data for covariates
# and outcome variables for individuals with exposure data (CRP (age 9), IL-6 (age 9), DNAm measured (age 7), genetics (birth)).

# Predictive mean matching was used for continuous variables and logisitic regression for binary categorical variables.

# Covariates were selected that predicted either missingness of variable to be imputed or associated with variable to be imputed. And had no less than 40% missing data. This reduces bias towards "missing not at random" (NMAR) and provides more accurate estimates.

# ----------------------------------------------------------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv") 

variables <- c("Sex", 
               "Maternal_education" = "Maternal.education.at.birth",  #  coded as binary
               "IL6"  = "IL.6..age.9.",
               "CRP" = "CRP..age.9.", 
               "BMI_age9", 
               "BMI_age7",
               "BMI_age12", # continuous score
               paste0("mfq_t0", 1:9), 
               "mfq_t10", "mfq_t11",
               "ple_t01", "ple_t02", "ple_t03", "ple_t04", "ple_t05", "ple_t06", 
               "ple_t07", "ple_t08", "ple_t09",
               "dep_episodes",
               "PLE_total",
             "SDQ_emotional_7", # continuous score
             "SDQ_emotional_9", # continuous score
             "SDQ_emotional_11", # continuous score
             "SDQ_emotional_13", # continuous score
             "DV_anxiety_dis_13", # coded as binary
             "DV_depressive_dis_13", # coded as binary
             "DV_any_anxiety_7", # coded as binary
             "DV_MDD_7", # coded as binary
             "DV_anxiety_dis_10",  # coded as binary
             "ever_depression_diagnosis_22", # coded as binary
             "WEMWBS_23", # coded as binary
             "dep_bin_17", #  coded as binary
             "mild_dep_dis_24", # coded as binary
             "IMD_score", # continuous score
             "maternal_EDPS", # code as binary
             "paternal_EDPS" # code as binary
              )
# ----------------------------------------------------------------------------------------
# Participant sex : female = 1; male = 0 -------------------------------------------------
# Maternal education at birth: 0 = CSE/Olevel/Vocational; 1= Alevel/Degree ---------------
# ----------------------------------------------------------------------------------------

# Check there are no negative values which should be coded as missing (ie. NA)
data %>%
  dplyr::select(variables) %>%
  filter_all(any_vars(. < 0)) # 0 rows :-)

# Get categorical variables we want to factorise
catColsLogical <- sapply(1:ncol(data), function(x) length(unique(data[,x])) == 3)
catCols <- colnames(data)[catColsLogical] ;catCols

# Get continuous varaibles we want to z-scale
conCols <- colnames(data)[!catColsLogical][-1] ;conCols
# remove CRP and IL-6 from this, as we z-scale their log transformed variables
conCols <-conCols[-c(grep("CRP", conCols), grep("IL", conCols), 
                  grep("dep_episodes", conCols), grep("PLE_total", conCols), 
                  grep("mfq", conCols) )] ;conCols

# Subset data to those with CRP or IL-6 bloods data, factorise binary variables, z-scale continuous
#########################################################
# This is where we split the script by CRP or IL-6
#########################################################
if(marker == "CRP"){
  dataSub <- data %>%
    mutate_at(catCols, as.factor) %>%
    mutate_at(conCols, scale) %>%
    mutate_at(conCols, as.numeric) %>%
    dplyr::select(variables) %>%
    filter(CRP < 10) %>%
    mutate(CRP_log = log(CRP + 1), IL6_log = log(IL6 + 1)) %>%
    mutate_at(c("CRP_log", "IL6_log"), scale) %>%
    mutate_at(c("CRP_log", "IL6_log"), as.numeric) %>%
    filter(!is.na(CRP)) %>%
    dplyr::select(-c(CRP, IL6))
}
if(marker == "IL6"){
  dataSub <- data %>%
    mutate_at(catCols, as.factor) %>%
    mutate_at(conCols, scale) %>%
    mutate_at(conCols, as.numeric) %>%
    dplyr::select(variables) %>%
    filter(CRP < 10) %>%
    mutate(CRP_log = log(CRP + 1), IL6_log = log(IL6 + 1)) %>%
    mutate_at(c("CRP_log", "IL6_log"), scale) %>%
    mutate_at(c("CRP_log", "IL6_log"), as.numeric) %>%
    filter(!is.na(IL6)) %>%
    dplyr::select(-c(CRP, IL6))
}



########################################################################################
# Exploring what's missing in our data:
########################################################################################
# Check number of NAs for each variable:
summary(dataSub)


# --------------------------------------------------------------------------------------
# Imputing BMI:
dataSub %>% 
  filter(is.na(BMI_age9)) %>%
  filter(!is.na(BMI_age12)) %>%
  nrow() 
# 54 participants have BMI missing at age 9
# 45 participants have BMI missing at age 9 but not at age 12
# Therefore we can impute 45 participants BMI meaning only 9 participants have missing BMI.

# Combined age 7 and 12 to impute for age 9:
dataSub %>% 
  filter(is.na(BMI_age9)) %>%
  filter(!is.na(BMI_age7) | !is.na(BMI_age12)) %>%
  nrow() 
# All participants with missing BMI at age 9 can be imputed with BMI at age 7 and 12

# --------------------------------------------------------------------------------------
# Which other auxiliary variables have <= 40% missing:
propNA <- round(colSums(is.na(dataSub))/nrow(dataSub), 3) ;propNA
colnames(dataSub)[propNA > 0.40]
# "ever_depression_diagnosis_22", "dep_bin_17", "mild_dep_dis_24", "paternal_EDPS"
# should not be considered as auxiliary variables

# # WEMWBS has no missing data
# sum(dataSubQC$WEMWBS_23 == 0)
# sum(dataSubQC$WEMWBS_23 == 1)
# sum(is.na(dataSubQC$WEMWBS_23))
# sum(is.na(data$WEMWBS_23))

dataSubQC <- dataSub %>%
              dplyr::select(-c("ever_depression_diagnosis_22", "dep_bin_17", 
                            "mild_dep_dis_24", "paternal_EDPS", "DV_MDD_7"))
# "DV_MDD_7" caused problems later on when running regression analysis as complete cases did not have any 1 for DV_MDD_7


########################################################################################
# Which auxiliary variables predict missingness or variable we want to impute
########################################################################################
# Loop through each variable we want to impute to see which auxillary variables predict that variable or missingness.

auxVars <- c("BMI_age7",  "BMI_age12",
             "SDQ_emotional_7", "SDQ_emotional_9", 
             "SDQ_emotional_11", "SDQ_emotional_13", "DV_anxiety_dis_13", 
             "DV_depressive_dis_13", "DV_any_anxiety_7",  "DV_anxiety_dis_10", 
             "WEMWBS_23", "IMD_score", "maternal_EDPS"
             )

imputeVars <- c("Maternal_education", "BMI_age9", 
                "mfq_t01", "mfq_t02", "mfq_t03", "mfq_t04", "mfq_t05", 
                "mfq_t06", "mfq_t07", "mfq_t08", "mfq_t09", "mfq_t10", "mfq_t11"
                )

getVars <- function(imputeVars){
    newAuxVars <- lapply(imputeVars, function(outcome){
      lapply(auxVars, function(predictor){
      if( class(dataSubQC[,outcome]) == "factor" ){
        family <- "binomial"
      }else{
        family <- "gaussian"
      }
      formula <- paste0(outcome, " ~ ", predictor)
      fit <- glm(formula = formula, data = dataSubQC, family = family)
      sig <- summary(fit)$coefficients %>%
              as.data.frame() %>%
              slice(-1) %>%
              filter(.[[4]] < 0.05) %>%
              rownames() 
      if(is_empty(sig)){
        sig <- ""
      }
      sig
      }) %>% unlist() %>% unique()
    }) 
return(newAuxVars)
}

newAuxVars <-getVars(imputeVars) ; newAuxVars
newAuxVars <- newAuxVars %>% unlist() %>% unique()
newAuxVars <- newAuxVars[!newAuxVars %in% ""]
length(auxVars) == length(newAuxVars) 
# All auxiliary variables predict a variable we want to impute

# Check PLEs separately:
newAuxVars <- getVars(imputeVars = c("ple_t01", "ple_t02", "ple_t03", "ple_t04", "ple_t05", "ple_t06", "ple_t07", "ple_t08", "ple_t09"))
newAuxVars # At least one variable predicts PLE at each time point


# ------------------------------------------------------------------------------------
# Do they also predict missingness?
getVarsMiss <- function(imputeVars){
newAuxVars <- lapply(imputeVars, function(outcome){
  lapply(auxVars, function(predictor){
    formula <- paste0("is.na(", outcome, ") ~ ", predictor)
    fit <- glm(formula = formula, data = dataSubQC, family = "binomial")
    sig <- summary(fit)$coefficients %>%
      as.data.frame() %>%
      slice(-1) %>%
      filter(.[[4]] < 0.05) %>%
      rownames() 
    if(is_empty(sig)){
      sig <- ""
    }
    sig
  }) %>% unlist() %>% unique()
}) 
}
newAuxVars <- getVarsMiss(imputeVars)
newAuxVars <- newAuxVars %>% unlist() %>% unique()
newAuxVars <- newAuxVars[!newAuxVars %in% ""]
newAuxVars # 10/13 auxiliary variables also predict missingness of a variable we want to impute

# Check PLEs separately:
newAuxVars <- getVarsMiss(imputeVars = c("ple_t01", "ple_t02", "ple_t03", "ple_t04", "ple_t05", "ple_t06", "ple_t07", "ple_t08", "ple_t09"))
newAuxVars # At least 2 variables predicts missingness of PLE at each time point

########################################################################################
# Correlation between variables
########################################################################################
library(corrplot)
corSub <- dplyr::select(dataSubQC, where(is.numeric))
M <- cor(corSub, use = "pairwise.complete.obs")
corrplot(M)
########################################################################################
# List of variables:
colnames(dataSubQC)

# [1] "Sex"                  "Maternal_education"              "BMI_age9"            
# [5] "BMI_age7"             "BMI_age12"            "mfq_t01"              "mfq_t02"             
# [9] "mfq_t03"              "mfq_t04"              "mfq_t05"              "mfq_t06"             
# [13] "mfq_t07"              "mfq_t08"              "mfq_t09"              "mfq_t10"             
# [17] "mfq_t11"              "ple_t01"              "ple_t02"              "ple_t03"             
# [21] "ple_t04"              "ple_t05"              "ple_t06"              "ple_t07"             
# [25] "ple_t08"              "ple_t09"              "dep_episodes"         "PLE_total"           
# [29] "SDQ_emotional_7"      "SDQ_emotional_9"      "SDQ_emotional_11"     "SDQ_emotional_13"    
# [33] "DV_anxiety_dis_13"    "DV_depressive_dis_13" "DV_any_anxiety_7"     "DV_anxiety_dis_10"   
# [37] "WEMWBS_23"            "IMD_score"            "maternal_EDPS"        "CRP_log"             
# [41] "IL6_log" 

########################################################################################
# Running the imputation: 
########################################################################################
# A small number of imputations is fine when no. of missing data is small.
# But a larger M makes the imputation more reproducible when using a different seed.
# M should be >= 100 * fraction of missing information (FMI).
# It's also possible to do a trial and error approach to see if your estimates differ
# after running the imputation 2-3 times with the same M.

# first get default predictor matrix
predMat <- make.predictorMatrix(dataSubQC)
# Rows are variables to be imputed, cols are variables used to impute.

# We don't want to impute missing data for CRP, IL-6 or sex
# if we don't want to impute their missing data then they cannot be used as predictors, 
# as there cannot be missing data in the predictors used for imputation.
# Unsure if this is ok, as all variables used in the substantive model are meant to be included in the imputation model
# otherwise I think it biases the estimates towards the null.
# However, if I do use these below variables as predictors BUT i also don't want to impute them
# then I can't impute variables for the points these data is missing... so this is not what we want.
# The below is the only work around I found for this.

#predMat[c("CRP_log","IL6_log",  "Sex", "dep_episodes", "PLE_total"),] <- 0
#predMat[,c("CRP_log","IL6_log", "Sex", "dep_episodes", "PLE_total")] <- 0
predMat

# Also set the method to empty for these variables so they are not imputed, changing predMat alone is not enough.
meth <- make.method(dataSubQC)
#meth[c("CRP_log","IL6_log", "Sex", "dep_episodes", "PLE_total")] <- ""
meth

start_time <- Sys.time()
ModImps <- mice(dataSubQC, m=nImputations, predictorMatrix=predMat, seed=8232446, printFlag=FALSE, method = meth)
end_time <- Sys.time()
end_time - start_time

# Inspect the imputed datasets object
names(ModImps)

# Non-imputed data:
head(ModImps$data) 

# Imputed data:
# each column is an imputation dataset & each row is a participant that had their data imputed:
ModImps$imp$Maternal_education %>% head()
ModImps$imp$Sex %>% head()

ModImps$loggedEvents # tells you which predictors (if any) were removed from the model

# --------------------------------------------------------------------------------------
# Calculate total number of depressive episodes in each imputed dataset by altering the ModImps object

ImpsLong <- complete(ModImps, action = 'long', include = TRUE)

# Let's see the first few imputed data for one participant
ImpsLong %>%
  filter(.id == 1) %>%
  head()
# here it is clear that .imp 0 refers to the non-imputed dataset, and 1 - n are all the imputed datasets.
# for variables that are NA in .imp 0 rows, they are not NA in the other .imp rows. Showing that the data has
# been imputed.

# ---------------
# Impute total depressive episodes
ImpsLong$dep_episodes <- rowSums(dplyr::select(ImpsLong, c(mfq_t01:mfq_t11)) >= 11, na.rm = T) 
# some of those 0s should be NA though for people that don't have data for ANY appointments (same as what we did in the other script, this should really only affect the row where .imp is 0 ie. non-imputed dataset)
ImpsLong$dep_episodes[rowSums(is.na(dplyr::select(ImpsLong, c(mfq_t01:mfq_t11)))) == 11] <- NA

# Non-imputed dataset still contains NA:
ImpsLong %>%
  filter(.imp == 0) %>%
  dplyr::pull(dep_episodes)

# 1st imputed dataset does not contain NA:
ImpsLong %>%
  filter(.imp == 1) %>%
  dplyr::pull(dep_episodes)

# ---------------
# Calculate total number of PLEs 

# Impute total PLES:
timePoint <- paste0("ple_t0", 1:9)

# Change TRUE/FALSE to 1/0:
ImpsLong <- ImpsLong %>%
  mutate_at(timePoint, ~ as.numeric(.) -1 ) 

ImpsLong$PLE_total <- rowSums(dplyr::select(ImpsLong, timePoint), na.rm = T)

# some of those 0s should be NA though for people that didn't attend ANY appointments (just for non-imputed dataset)
ImpsLong$PLE_total[ rowSums(is.na( dplyr::select(ImpsLong, timePoint) )) == 9 ] <- NA

# Non-imputed dataset still contains NA:
ImpsLong %>%
  filter(.imp == 0) %>%
  dplyr::pull(PLE_total)

# 1st imputed dataset does not contain NA:
ImpsLong %>%
  filter(.imp == 1) %>%
  dplyr::pull(PLE_total)

# ---------------
# Turn long dataframe back into mids object
ModImpsTotal <- as.mids(ImpsLong)

# --------------------------------------------------------------------------------------
# Run the regression on each imputed dataset

MIfit <- with(data = ModImpsTotal, exp = glm.nb(dep_episodes ~ IL6_log + Sex) )
pooled <- pool(MIfit)

# Compare estimates with complete case analysis:
CCfit <- glm.nb(formula = dep_episodes ~ IL6_log + Sex, data = dataSubQC)
summary(CCfit)
summary(pooled)

# compare with non-edited imputed datasets for depressive episodes (should be the same as CC)
test <- with(data = ModImps, exp = glm.nb(dep_episodes ~ IL6_log + Sex) )
pooledTest <- pool(test)
# summary(pooledTest)

MIfit <- with(data = ModImpsTotal, exp = glm.nb(PLE_total ~ IL6_log + Sex) )
pooled <- pool(MIfit)

# Compare estimates with complete case analysis:
CCfit <- glm.nb(formula = PLE_total ~ IL6_log + Sex, data = dataSubQC)
summary(CCfit)
summary(pooled)

# compare with non-edited imputed datasets for PLEs (should be the same as CC)
test <- with(data = ModImps, exp = glm.nb(PLE_total ~ IL6_log + Sex) )
pooledTest <- pool(test)
summary(pooledTest)


########################################################################################
# Turn the above into a nice function that runs all the models and stores results in a table
########################################################################################

# This works:
MIfit <- with(data = ModImpsTotal, expr = glm.nb(dep_episodes ~ IL6_log + Sex) )

# This doesn't work:
#form <- "dep_episodes ~ IL6_log + Sex"
#MIfit <- with(data = ModImpsTotal, expr = glm.nb(form) )

# Instead of using with change ModImpsTotal into a long format and run regression on each imputed dataset using lapply

# First check if a list is the correct object we want:
MIfit$analyses %>% class()

pool(MIfit)
pool(MIfit$analyses)

summary(pool(MIfit))
summary(pool(MIfit$analyses))
# Yes a list is good.

# --------------------------------------------------------------------------------------

# Convert mice object into a long data frame
ImpsLongTotal <- complete(ModImpsTotal, action = 'long', include = TRUE)

# Split this into a list of dataframes, each item in the list is an imputed dataframe
ImpsLongTotalList <- split(ImpsLongTotal, ImpsLongTotal$.imp)

# Remove first dataframe in list as this is the non-imputed dataset
lapply(ImpsLongTotalList, head)

ImpsLongTotalList$`0` # this is the non-imputed dataset
ImpsLongTotalList <- ImpsLongTotalList[-1]

names(ImpsLongTotalList) # names go from 1 - n, where n is the total number of imputations
ImpsLongTotalList[1] %>% head() # the first item in the list is now the first imputed data set

# -----------------
summary(pool(MIfit))

MIfit <- lapply(ImpsLongTotalList, function(imputedData) {
            form <- "dep_episodes ~ IL6_log + Sex"
            glm.nb(formula = form, data = imputedData)
          })

summary(pool(MIfit)) # this gives us the same result as using the "with" function

# -----------------
# Now let's loop over different outcome variables, exposure and covariates in the model

outcomes <- c("dep_episodes", "PLE_total")
if(marker == "CRP"){
  exposures <- "CRP_log"
}
if(marker == "IL6"){
  exposures <- "IL6_log"
}
covariates <- c("Sex", "Sex + Maternal_education + BMI_age9")
modelName <- c("Base_Model", "Fully_Adjusted")

MIfit <- 
lapply(outcomes, function(outcome){
  exposuresList <- lapply(exposures, function(exposure){
    covarsList <- lapply(covariates, function(covariate){
      lapply(ImpsLongTotalList, function(imputedData) {
        form <- paste0(outcome, " ~  + ", exposure, " + ", covariate)
        glm.nb(formula = form, data = imputedData)  
      })
    })
    names(covarsList) <- modelName
    return(covarsList)
  })
  names(exposuresList) <- paste0(exposures, names(exposuresList))
  return(exposuresList)
}) 
names(MIfit) <- outcomes

MIfit <- MIfit %>% do.call(c, .)  %>% do.call(c, .) # this returns a list of imputed results

lapply(MIfit, function(x){
  summary(pool(x)) 
})

# Combine the results into a table in the same format we did for the complete case analysis
# ie. base and fully adjusted models separately
# col names: Exposure	Outcome	Covariates	Sample_Size	Standardised Beta	CI_lower	CI_upper	SE	P-value	Significant_covariates

results <- lapply(1:length(MIfit), function(i){
              results <- summary(pool(MIfit[[i]])) 
              
              nMissing <- length(MIfit[[i]][[1]]$na.action) 
              nTotal <- nrow(ImpsLongTotalList[[1]])
              n <- nTotal - nMissing 

              name <- names(MIfit)[i] %>% str_split(., "\\.") %>% unlist()
              
              results <- results %>%
                filter(term == name[2]) %>%
                mutate(Exposure = term, Outcome = name[1], Covariates = name[3], Sample_Size = n) %>%
                dplyr::select(c(Exposure, Outcome, Covariates, Sample_Size, Standardised_Beta = estimate,
                         SE = `std.error`, `P-value` = `p.value`))

}) %>% do.call(rbind, .)



if(marker == "CRP"){
write.csv(results, "Imputation/Output/imputed_results_bloods_CRP.csv", row.names = F, quote = F)
}
if(marker == "IL-6"){
  write.csv(results, "Imputation/Output/imputed_results_bloods_IL6.csv", row.names = F, quote = F)
}

