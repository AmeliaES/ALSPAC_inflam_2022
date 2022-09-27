# Impute missing data for PRS data set

########################################################################################
# Get the data:
########################################################################################
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# ----------------------------------------------------------------------------
# Load in PRSs
PRSs <- list(read.table(paste0("PRS/Output/CRP_SBayesR.all_score"), header = T),
             read.table(paste0("PRS/Output/IL6_SBayesR.all_score"), header = T))

for(i in 1:length(PRSs)){
  colnames(PRSs[[i]])[1] <- "Subject" 
}

PRSs <- Reduce(function(x, y) merge(x, y, all = FALSE, by = "Subject"), PRSs)
PRSs <- PRSs %>%
  dplyr::select(c(Subject,
                  CRP_SBayesR = Pt_1.x, IL6_SBayesR = Pt_1.y))

# Format subject to same as in data
stri_sub(PRSs$Subject, -1, -2) <- "_"

# ----------------------------------------------------------------------------
# Merge PRSs into data
data <- merge(data, PRSs, by = "Subject")
head(data)
nrow(data)
# ----------------------------------------------------------------------------

variables <- c("Sex", 
               "Maternal_education" = "Maternal.education.at.birth",  #  coded as binary
                "IL6_SBayesR", "CRP_SBayesR",
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
               "paternal_EDPS", # code as binary
               paste0("pc", 1:10)
)

# Get categorical variables we want to factorise
catColsLogical <- sapply(1:ncol(data), function(x) length(unique(data[,x])) == 3)
catCols <- colnames(data)[catColsLogical] ;catCols

# Get continuous varaibles we want to z-scale
conCols <- colnames(data)[!catColsLogical][-1] ;conCols
# remove continuous variables we don't want to z-scale:
conCols <-conCols[-c(grep("dep_episodes", conCols), grep("PLE_total", conCols), 
                     grep("mfq", conCols) )] ;conCols

# Subset data to those with CRP and IL-6 bloods data, factorise binary variables, z-scale continuous
dataSub <- data %>%
  mutate_at(catCols, as.factor) %>%
  mutate_at(conCols, scale) %>%
  mutate_at(conCols, as.numeric) %>%
  dplyr::select(variables) %>%
  filter(!is.na(IL6_SBayesR) | !is.na(CRP_SBayesR)) 

head(dataSub)

# Remove variables we removed in the inflam bloods subset:
dataSubQC <- dataSub %>%
  dplyr::select(-c("ever_depression_diagnosis_22", "dep_bin_17", 
                   "mild_dep_dis_24", "paternal_EDPS", "DV_MDD_7"))

# Check number of NAs for each variable:
summary(dataSubQC)


########################################################################################
# Running the imputation: 
########################################################################################

# We don't want to impute missing data for CRP, IL-6 or sex
predMat <- make.predictorMatrix(dataSubQC)
# predMat[c("CRP_SBayesR","IL6_SBayesR"),] <- 0
# predMat[,c("CRP_SBayesR","IL6_SBayesR")] <- 0

meth <- make.method(dataSubQC)
# meth[c("CRP_SBayesR","IL6_SBayesR")] <- ""

start_time <- Sys.time()
ModImps <- mice(dataSubQC, m=nImputations, predictorMatrix=predMat, seed=8232446, printFlag=FALSE, method = meth)
end_time <- Sys.time()
end_time - start_time

# --------------------------------------------------------------------------------------
# Calculate total number of depressive episodes in each imputed dataset by altering the ModImps object

ImpsLong <- complete(ModImps, action = 'long', include = TRUE)

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

########################################################################################
# Run the regression
########################################################################################

# Convert mice object into a long data frame
ImpsLongTotal <- complete(ModImpsTotal, action = 'long', include = TRUE)

# Split this into a list of dataframes, each item in the list is an imputed dataframe
ImpsLongTotalList <- split(ImpsLongTotal, ImpsLongTotal$.imp)

# Remove first dataframe in list as this is the non-imputed dataset
ImpsLongTotalList <- ImpsLongTotalList[-1]

# -----------------
# Now let's loop over different outcome variables, exposure and covariates in the model

outcomes <- c("dep_episodes", "PLE_total")
exposures <- c("IL6_SBayesR", "CRP_SBayesR")
covariates <- c("Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10", 
                "Sex + Maternal_education + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10")
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

results

write.csv(results, "Imputation/Output/imputed_results_PRS.csv", row.names = F, quote = F)

