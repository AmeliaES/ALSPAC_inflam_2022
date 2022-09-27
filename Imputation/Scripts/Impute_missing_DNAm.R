# Impute missing data for PRS data set

########################################################################################
# Get the data:
########################################################################################
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# ----------------------------------------------------------------------------
# Merge DNAm scores at age 0, 7, 15, 17 (fill in NA for empty rows)
DNAm <- list(read.csv("DNAm/Output/DNAm_scores_age_0.csv", header = T),
             read.csv("DNAm/Output/DNAm_scores_age_7.csv", header = T))
# Merge these files into one data frame by the common column "Subject"
DNAm <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Subject"), DNAm)
nrow(DNAm) # 1017
colnames(DNAm) <- c("Subject","CRP_scores_0", "IL6_scores_0", "IL6_Gadd_scores_0",
                    "CRP_scores_7", "IL6_scores_7", "IL6_Gadd_scores_7")

data <- merge(data, DNAm, by = "Subject")
head(data)
nrow(data) # 998

# ----------------------------------------------------------------------------
# Estimated cell counts at each time point
cell_counts <- read.csv("DNAm/Output/cell_counts.csv", header = T) %>%
  filter(!str_detect(Subject, "M")) %>%
  filter(is.na(!str_detect(duplicate.rm, "Remove")))
# Scale cell counts
cell_counts <- cell_counts %>%
  mutate_at(c("Bcell", "CD4T", "CD8T", "Gran", "Mono"), scale)

# Cell counts at birth
cell_counts_birth <- cell_counts %>%
  filter(time_point == "cord") %>%
  dplyr::select(c(Subject, Bcell_birth = Bcell, CD4T_birth = CD4T, CD8T_birth = CD8T, 
                  Gran_birth = Gran, Mono_birth = Mono, NK_birth = NK))

# Cell counts at age 7
cell_counts_F07 <- cell_counts %>%
  filter(time_point == "F7") %>%
  dplyr::select(c(Subject, Bcell_F07 = Bcell, CD4T_F07 = CD4T, CD8T_F07 = CD8T, 
                  Gran_F07 = Gran, Mono_F07 = Mono, NK_F07 = NK))

data <- merge(merge(data, cell_counts_birth, by = "Subject"), 
              cell_counts_F07, by = "Subject")

nrow(data) # 877

# ----------------------------------------------------------------------------
# Include 10 DNAm PCs

PCs_0 <- read.csv("DNAm/Output/DNAm_PCs_age_0.csv")
PCs_7 <- read.csv("DNAm/Output/DNAm_PCs_age_7.csv")

PCs <- merge(PCs_0, PCs_7, by = "Subject", suffix = c("_0", "_7"))

data <- merge(data, PCs, by = "Subject")

# ----------------------------------------------------------------------------

variables <- c("Sex", 
               "Maternal_education" = "Maternal.education.at.birth",  #  coded as binary
               "CRP_scores_0", "IL6_scores_0",
               "CRP_scores_7", "IL6_scores_7", 
               "Bcell_F07", "CD4T_F07","CD8T_F07","Gran_F07" , "Mono_F07" , "NK_F07",
               "Bcell_birth", "CD4T_birth","CD8T_birth","Gran_birth" , "Mono_birth" , "NK_birth",
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
               "paternal_EDPS", # code as binary
               paste0("PC", 1:10, "_0"),
               paste0("PC", 1:10, "_7")
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
  filter(!is.na(CRP_scores_0) | !is.na(CRP_scores_7) | !is.na(IL6_scores_0) | !is.na(IL6_scores_7)) 

head(dataSub)

# Remove variables we removed in the inflam bloods subset:
dataSubQC <- dataSub %>%
  dplyr::select(-c("ever_depression_diagnosis_22", "dep_bin_17", 
                   "mild_dep_dis_24", "paternal_EDPS", "DV_MDD_7"))

########################################################################################
# Running the imputation: 
########################################################################################

# We don't want to impute missing data for CRP, IL-6 or sex
predMat <- make.predictorMatrix(dataSubQC)
# predMat[c("CRP_scores_0", "IL6_scores_0",
#          "CRP_scores_7", "IL6_scores_7", 
# "Bcell_F07", "CD4T_F07","CD8T_F07","Gran_F07" , "Mono_F07" , "NK_F07",
#    "Bcell_birth", "CD4T_birth","CD8T_birth","Gran_birth" , "Mono_birth" , "NK_birth",  "Sex", "dep_episodes", "PLE_total"),] <- 0
# predMat[,c("CRP_scores_0", "IL6_scores_0",
#         "CRP_scores_7", "IL6_scores_7", 
# "Bcell_F07", "CD4T_F07","CD8T_F07","Gran_F07" , "Mono_F07" , "NK_F07",
# "Bcell_birth", "CD4T_birth","CD8T_birth","Gran_birth" , "Mono_birth" , "NK_birth", "Sex", "dep_episodes", "PLE_total")] <- 0

predMat[,c(
"Bcell_F07", "CD4T_F07","CD8T_F07","Gran_F07" , "Mono_F07" , "NK_F07",
"Bcell_birth", "CD4T_birth","CD8T_birth","Gran_birth" , "Mono_birth" , "NK_birth")] <- 0

meth <- make.method(dataSubQC)
# meth[c("CRP_scores_0", "IL6_scores_0",
#        "CRP_scores_7", "IL6_scores_7", 
# "Bcell_F07", "CD4T_F07","CD8T_F07","Gran_F07" , "Mono_F07" , "NK_F07",
# "Bcell_birth", "CD4T_birth","CD8T_birth","Gran_birth" , "Mono_birth" , "NK_birth",  "Sex", "dep_episodes", "PLE_total")] <- ""

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

imputedRegression <- function(exposures, covariates){
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
}

outcomes <- c("dep_episodes", "PLE_total")
modelName <- c("Base_Model", "Fully_Adjusted")

exposures <- c("CRP_scores_0", "IL6_scores_0")
covariates <- c("Sex+ PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0",
 "Sex + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0 + Maternal_education + Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth")

results_birth <- imputedRegression(exposures, covariates)

exposures <- c("CRP_scores_7", "IL6_scores_7")
covariates <- c("Sex+ PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7", 
"Sex + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7 + BMI_age7 + Maternal_education + Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07")

results_F07 <- imputedRegression(exposures, covariates)

results <- rbind(results_birth, results_F07)

results

write.csv(results, "Imputation/Output/imputed_results_DNAm.csv", row.names = F, quote = F)






