# Associations of CRP and IL-6 DNA methylation scores (age 7) with number of depressive episodes

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)

library("ggpubr")
library(MASS)

# ----------------------------------------------------------------------------
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
# Include 10 PCs

PCs_0 <- read.csv("DNAm/Output/DNAm_PCs_age_0.csv")
PCs_7 <- read.csv("DNAm/Output/DNAm_PCs_age_7.csv")

PCs <- merge(PCs_0, PCs_7, by = "Subject", suffix = c("_0", "_7"))

data <- merge(data, PCs, by = "Subject")

# ----------------------------------------------------------------------------
# z-scale and factorise vars
dataTrans <- data %>%
  rename(CRP = CRP..age.9.) %>%
  rename(IL6 = IL.6..age.9.) %>%
  mutate(CRP_log = log(CRP+1)) %>%
  mutate(IL6_log = log(IL6+1)) %>%
  mutate_at(c("Sex", "Maternal.social.class.at.birth", "Maternal.education.at.birth"), as.factor) %>%
  mutate_at(c("BMI_age7", 
              "CRP_scores_0", "IL6_scores_0", "IL6_Gadd_scores_0",
              "CRP_scores_7", "IL6_scores_7", "IL6_Gadd_scores_7", 
              "CRP", "IL6", "CRP_log", "IL6_log",
              "PC1_0", "PC2_0",  "PC3_0", "PC4_0",  "PC5_0" , "PC6_0" , "PC7_0" , "PC8_0" , "PC9_0" , "PC10_0",
              "PC1_7" , "PC2_7" , "PC3_7" , "PC4_7" , "PC5_7" , "PC6_7" , "PC7_7" , "PC8_7" , "PC9_7" , "PC10_7",
              "Bcell_birth" , "CD4T_birth" , "CD8T_birth" , "Gran_birth" , "Mono_birth" , "NK_birth",
              "Bcell_F07" , "CD4T_F07" , "CD8T_F07" , "Gran_F07" , "Mono_F07" ,"NK_F07"), scale)

# Split dataTrans by sex 
dataTransMale <- dataTrans %>%
  filter(Sex == 0)

dataTransFemale <- dataTrans %>%
  filter(Sex == 1)

# ----------------------------------------------------------------------------
# Negative binomial regression
# Can be used for over-dispersed count data

# Base Model
# birth
CRP_nb_nocovars_0 <- glm.nb(dep_episodes ~ CRP_scores_0 + Sex + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,  data = dataTrans)
IL6_nb_nocovars_0 <- glm.nb(dep_episodes ~ IL6_scores_0 + Sex + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTrans)
# age 7
CRP_nb_nocovars_7 <- glm.nb(dep_episodes ~ CRP_scores_7 + Sex + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,  data = dataTrans)
IL6_nb_nocovars_7 <- glm.nb(dep_episodes ~ IL6_scores_7 + Sex + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7, data = dataTrans)

# Fully Adjusted
# birth
CRP_nb_0 <- glm.nb(dep_episodes ~ CRP_scores_0 + Sex + Maternal.education.at.birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                 data = dataTrans)
IL6_nb_0 <- glm.nb(dep_episodes ~ IL6_scores_0 + Sex + Maternal.education.at.birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                 data = dataTrans)
# age 7
CRP_nb_7 <- glm.nb(dep_episodes ~ CRP_scores_7 + Sex + BMI_age7 + Maternal.education.at.birth + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                 data = dataTrans)
IL6_nb_7 <- glm.nb(dep_episodes ~ IL6_scores_7 +Sex +  BMI_age7 +   Maternal.education.at.birth + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                 data = dataTrans)

# Fully Adjusted (with cell estimates)
# birth
CRP_nb_0_cc <- glm.nb(dep_episodes ~ CRP_scores_0 + Sex + Maternal.education.at.birth + Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                   data = dataTrans)
IL6_nb_0_cc <- glm.nb(dep_episodes ~ IL6_scores_0 + Sex + Maternal.education.at.birth +  Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                   data = dataTrans)
# age 7
CRP_nb_7_cc <- glm.nb(dep_episodes ~ CRP_scores_7 + Sex + BMI_age7 + Maternal.education.at.birth + Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                   data = dataTrans)
IL6_nb_7_cc <- glm.nb(dep_episodes ~ IL6_scores_7 +Sex +  BMI_age7 +   Maternal.education.at.birth +  Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                   data = dataTrans)

# ----------------------------------------------------------------------------
# Save the results in Output
options(scipen=4)

result_func <- function(model, covariates, exposure){
  results <- t(as.data.frame( summary(model)$coefficients[2,] ))
  lower <- confint(model, level = 0.95)[2,1]
  upper <- confint(model, level = 0.95)[2,2]
  results <- cbind(data.frame("Exposure" = exposure),
                   data.frame("Outcome" = "Depression episodes"),
                   data.frame("Covariates" = covariates),
                   data.frame("Sample_Size" = nrow(dataTrans) - length(summary(model)$na.action) ),
                   results)
  row.names(results) <- NULL
  results <- results %>%
    mutate(CI_upper = upper, CI_lower = lower) %>%
    dplyr::select(c(1:4, "Standardised Beta" = 5, CI_lower, CI_upper, "SE" = 6, "P-value" = 8 ))
  # Add col of significant covariates
  covars <- as.data.frame(summary(model)$coefficients) %>%
    filter(`Pr(>|z|)` < 0.05)
  if(nrow(covars) > 1){
    vars <- rownames(covars)[!rownames(covars) %in% rownames(summary(model)$coefficients)[1:2] ]
    results$Significant_covariates <- paste0(vars, collapse = "; ")
  }else{
    results$Significant_covariates <- " "
  }
  results
}

results <- do.call( rbind, 
                    list(result_func(IL6_nb_nocovars_0, "Base Model", "DNAm IL-6 score (birth)"),
                         result_func(CRP_nb_nocovars_0, "Base Model", "DNAm CRP score (birth)"),
                         result_func(IL6_nb_nocovars_7, "Base Model", "DNAm IL-6 score (7 years)"),
                         result_func(CRP_nb_nocovars_7, "Base Model", "DNAm CRP score (7 years)")
                         )
                    )
results
write.csv(results, "Regression_analysis/Output/DNAm_dep_base.csv", row.names = F)

results <- do.call( rbind, 
                    list(result_func(IL6_nb_0, "Fully Adjusted", "DNAm IL-6 score (birth)"),
                         result_func(CRP_nb_0, "Fully Adjusted", "DNAm CRP score (birth)"),
                         result_func(IL6_nb_7, "Fully Adjusted", "DNAm IL-6 score (7 years)"),
                         result_func(CRP_nb_7, "Fully Adjusted", "DNAm CRP score (7 years)")
                    )
)
results
write.csv(results, "Regression_analysis/Output/DNAm_dep_fully_adjusted.csv", row.names = F)

results <- do.call( rbind, 
                    list(result_func(IL6_nb_0_cc, "Fully Adjusted (with cell estimates)", "DNAm IL-6 score (birth)"),
                         result_func(CRP_nb_0_cc, "Fully Adjusted (with cell estimates)", "DNAm CRP score (birth)"),
                         result_func(IL6_nb_7_cc, "Fully Adjusted (with cell estimates)", "DNAm IL-6 score (7 years)"),
                         result_func(CRP_nb_7_cc, "Fully Adjusted (with cell estimates)", "DNAm CRP score (7 years)")
                    )
)
results
write.csv(results, "Regression_analysis/Output/DNAm_dep_fully_adjusted_cc.csv", row.names = F)

# ----------------------------------------------------------------------------
# Split by sex
result_func_sex <- function(model, covariates, exposure, sex, dataFrame){
  results <- t(as.data.frame( summary(model)$coefficients[2,] ))
  lower <- confint(model, level = 0.95)[2,1]
  upper <- confint(model, level = 0.95)[2,2]
  results <- cbind(data.frame("Sex" = sex),
                   data.frame("Exposure" = exposure),
                   data.frame("Outcome" = "Depression episodes"),
                   data.frame("Covariates" = covariates),
                   data.frame("Sample_Size" = nrow(dataFrame) - length(summary(model)$na.action) ),
                   results)
  row.names(results) <- NULL
  results <- results %>%
    mutate(CI_upper = upper, CI_lower = lower) %>%
    dplyr::select(c(1:5, "Standardised Beta" = 6, CI_lower, CI_upper, "SE" = 7, "P-value" = 9 ))
  # Add col of significant covariates
  covars <- as.data.frame(summary(model)$coefficients) %>%
    filter(`Pr(>|z|)` < 0.05)
  if(nrow(covars) > 1){
    vars <- rownames(covars)[!rownames(covars) %in% rownames(summary(model)$coefficients)[1:2] ]
    results$Significant_covariates <- paste0(vars, collapse = "; ")
  }else{
    results$Significant_covariates <- " "
  }
  results
}

# Males

# age birth
# Base Model
IL6_nb_male_nocovars_0 <- glm.nb(dep_episodes ~ IL6_scores_0 + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTransMale)
CRP_nb_male_nocovars_0 <- glm.nb(dep_episodes ~ CRP_scores_0 + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTransMale)
# Fully Adjusted
CRP_nb_male_0 <- glm.nb(dep_episodes ~ CRP_scores_0  + Maternal.education.at.birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTransMale)
IL6_nb_male_0 <- glm.nb(dep_episodes ~ IL6_scores_0  + Maternal.education.at.birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTransMale)
# Fully Adjusted (with cell counts)
CRP_nb_male_0_cc <- glm.nb(dep_episodes ~ CRP_scores_0  + Maternal.education.at.birth + Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTransMale)
IL6_nb_male_0_cc <- glm.nb(dep_episodes ~ IL6_scores_0  + Maternal.education.at.birth + Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTransMale)

# age 7
# Base Model
IL6_nb_male_nocovars_7 <- glm.nb(dep_episodes ~ IL6_scores_7 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7, data = dataTransMale)
CRP_nb_male_nocovars_7 <- glm.nb(dep_episodes ~ CRP_scores_7 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7, data = dataTransMale)
# Fully Adjusted
CRP_nb_male_7 <- glm.nb(dep_episodes ~ CRP_scores_7  + BMI_age7 + Maternal.education.at.birth + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                   data = dataTransMale)
IL6_nb_male_7 <- glm.nb(dep_episodes ~ IL6_scores_7 +  BMI_age7 +   Maternal.education.at.birth + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                   data = dataTransMale)
# Fully Adjusted (with cell counts)
CRP_nb_male_7_cc <- glm.nb(dep_episodes ~ CRP_scores_7  + BMI_age7 + Maternal.education.at.birth + Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                        data = dataTransMale)
IL6_nb_male_7_cc <- glm.nb(dep_episodes ~ IL6_scores_7 +  BMI_age7 +   Maternal.education.at.birth +  Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07+ PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                        data = dataTransMale)

#---------
# Females

# age birth
# Base Model
IL6_nb_female_nocovars_0 <- glm.nb(dep_episodes ~ IL6_scores_0 + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,data = dataTransFemale)

CRP_nb_female_nocovars_0 <- glm.nb(dep_episodes ~ CRP_scores_0 + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,  data = dataTransFemale)
# Fully Adjusted
CRP_nb_female_0 <- glm.nb(dep_episodes ~ CRP_scores_0  + Maternal.education.at.birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                        data = dataTransFemale)
IL6_nb_female_0 <- glm.nb(dep_episodes ~ IL6_scores_0  +   Maternal.education.at.birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                        data = dataTransFemale)
# Fully Adjusted (with cell counts)
CRP_nb_female_0_cc <- glm.nb(dep_episodes ~ CRP_scores_0  + Maternal.education.at.birth + Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                          data = dataTransFemale)
IL6_nb_female_0_cc <- glm.nb(dep_episodes ~ IL6_scores_0  +   Maternal.education.at.birth +  Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                          data = dataTransFemale)

# age 7
# Base Model
IL6_nb_female_nocovars_7 <- glm.nb(dep_episodes ~ IL6_scores_7 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,data = dataTransFemale)

CRP_nb_female_nocovars_7 <- glm.nb(dep_episodes ~ CRP_scores_7 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,  data = dataTransFemale)
# Fully Adjusted
CRP_nb_female_7 <- glm.nb(dep_episodes ~ CRP_scores_7 + BMI_age7 + Maternal.education.at.birth + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                        data = dataTransFemale)
IL6_nb_female_7 <- glm.nb(dep_episodes ~ IL6_scores_7 +  BMI_age7 +   Maternal.education.at.birth + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                        data = dataTransFemale)
# Fully Adjusted (with cell counts)
CRP_nb_female_7_cc <- glm.nb(dep_episodes ~ CRP_scores_7 + BMI_age7 + Maternal.education.at.birth + Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                          data = dataTransFemale)
IL6_nb_female_7_cc <- glm.nb(dep_episodes ~ IL6_scores_7 +  BMI_age7 +   Maternal.education.at.birth +  Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                          data = dataTransFemale)


results_sex <- do.call( rbind, 
list(result_func_sex(IL6_nb_male_nocovars_0, "Base Model", "DNAm IL-6 score (birth)", "MALE", dataTransMale),
result_func_sex(CRP_nb_male_nocovars_0, "Base Model", "DNAm CRP score (birth)", "MALE", dataTransMale),

result_func_sex(IL6_nb_female_nocovars_0, "Base Model", "DNAm IL-6 score (birth)", "FEMALE", dataTransFemale),
result_func_sex(CRP_nb_female_nocovars_0, "Base Model", "DNAm CRP score (birth)", "FEMALE", dataTransFemale),

result_func_sex(IL6_nb_male_nocovars_7, "Base Model", "DNAm IL-6 score (7 years)", "MALE", dataTransMale),
result_func_sex(CRP_nb_male_nocovars_7, "Base Model", "DNAm CRP score (7 years)", "MALE", dataTransMale),

result_func_sex(IL6_nb_female_nocovars_7, "Base Model", "DNAm IL-6 score (7 years)", "FEMALE", dataTransFemale),
result_func_sex(CRP_nb_female_nocovars_7, "Base Model", "DNAm CRP score (7 years)", "FEMALE", dataTransFemale))
                  )

results_sex
write.csv(results_sex, "Regression_analysis/Output/DNAm_dep_sex_split_base.csv", row.names = F)

results_sex <- do.call( rbind, 
                        list(result_func_sex(IL6_nb_male_0, "Fully Adjusted", "DNAm IL-6 score (birth)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_male_0, "Fully Adjusted", "DNAm CRP score (birth)", "MALE", dataTransMale),
                             
                             result_func_sex(IL6_nb_female_0, "Fully Adjusted", "DNAm IL-6 score (birth)", "FEMALE", dataTransFemale),
                             result_func_sex(CRP_nb_female_0, "Fully Adjusted", "DNAm CRP score (birth)", "FEMALE", dataTransFemale),
                             
                             result_func_sex(IL6_nb_male_7, "Fully Adjusted", "DNAm IL-6 score (7 years)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_male_7, "Fully Adjusted", "DNAm CRP score (7 years)", "MALE", dataTransMale),
                             
                             result_func_sex(IL6_nb_female_7, "Fully Adjusted", "DNAm IL-6 score (7 years)", "FEMALE", dataTransFemale),
                             result_func_sex(CRP_nb_female_7, "Fully Adjusted", "DNAm CRP score (7 years)", "FEMALE", dataTransFemale))
)

results_sex
write.csv(results_sex, "Regression_analysis/Output/DNAm_dep_sex_split_fully_adjusted.csv", row.names = F)

results_sex <- do.call( rbind, 
                        list(result_func_sex(IL6_nb_male_0_cc, "Fully Adjusted (with cell estimates)", "DNAm IL-6 score (birth)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_male_0_cc, "Fully Adjusted (with cell estimates)", "DNAm CRP score (birth)", "MALE", dataTransMale),
                             
                             result_func_sex(IL6_nb_female_0_cc, "Fully Adjusted (with cell estimates)", "DNAm IL-6 score (birth)", "FEMALE", dataTransFemale),
                             result_func_sex(CRP_nb_female_0_cc, "Fully Adjusted (with cell estimates)", "DNAm CRP score (birth)", "FEMALE", dataTransFemale),
                             
                             result_func_sex(IL6_nb_male_7_cc, "Fully Adjusted (with cell estimates)", "DNAm IL-6 score (7 years)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_male_7_cc, "Fully Adjusted (with cell estimates)", "DNAm CRP score (7 years)", "MALE", dataTransMale),
                             
                             result_func_sex(IL6_nb_female_7_cc, "Fully Adjusted (with cell estimates)", "DNAm IL-6 score (7 years)", "FEMALE", dataTransFemale),
                             result_func_sex(CRP_nb_female_7_cc, "Fully Adjusted (with cell estimates)", "DNAm CRP score (7 years)", "FEMALE", dataTransFemale))
)

results_sex
write.csv(results_sex, "Regression_analysis/Output/DNAm_dep_sex_split_fully_adjusted_cc.csv", row.names = F)

