# Associations of CRP and IL-6 in blood age 9 with number of PEs


setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
library(MASS)
# ----------------------------------------------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# log transform and factorise vars
dataTrans <- data %>%
  rename(CRP = CRP..age.9.) %>%
  filter(CRP < 10) %>%
  rename(IL6 = IL.6..age.9.) %>%
  mutate(CRP_quartile = ntile(CRP, 4)) %>% # Create quartiles for inflammatory markers
  mutate(IL6_quartile = ntile(IL6, 4)) %>%
  mutate(CRP_log = log(CRP+1)) %>%
  mutate(IL6_log = log(IL6+1)) %>%
  mutate_at(c("Sex", "IL6_quartile", "CRP_quartile", "Maternal.social.class.at.birth",  "Maternal.education.at.birth"), as.factor)

# Z-scale continuous data
dataTrans <- dataTrans %>%
  mutate_at(c("CRP", "IL6", "IL6_log", "CRP_log", "BMI_age9"), scale)  # z-scale continuous variables

# Split dataTrans by sex 
dataTransMale <- dataTrans %>%
  filter(Sex == 0)

dataTransFemale <- dataTrans %>%
  filter(Sex == 1)

hist(dataTransMale$PLE_total)
hist(dataTransFemale$PLE_total)
# ----------------------------------------------------------------------------
# Base Model
IL6_nb_nocovars <- glm.nb(PLE_total ~ IL6_log + Sex ,
                          data = dataTrans)

CRP_nb_nocovars <- glm.nb(PLE_total ~ CRP_log + Sex ,
                          data = dataTrans)
# Fully Adjusted
IL6_nb <- glm.nb(PLE_total ~ IL6_log +
                   Sex + 
                   BMI_age9 +  
                   Maternal.education.at.birth ,
                 data = dataTrans)

CRP_nb <- glm.nb(PLE_total ~ CRP_log +
                   Sex + 
                   BMI_age9 +  
                   Maternal.education.at.birth ,
                 data = dataTrans)


# ----------------------------------------------------------------------------
# Split by sex:
# Males

# Base Model
IL6_nb_male_nocovars <- glm.nb(PLE_total ~ IL6_log ,
                               data = dataTransMale)

CRP_nb_male_nocovars <- glm.nb(PLE_total ~ CRP_log ,
                               data = dataTransMale)
# Fully Adjusted
IL6_nb_male <- glm.nb(PLE_total ~ IL6_log +             
                        BMI_age9 +  
                        Maternal.education.at.birth,
                      data = dataTransMale)

CRP_nb_male <- glm.nb(PLE_total ~ CRP_log +             
                        BMI_age9 +  
                        Maternal.education.at.birth,
                      data = dataTransMale)
# ------------------------
# Females
# Base Model
IL6_nb_female_nocovars <- glm.nb(PLE_total ~ IL6_log ,
                                 data = dataTransFemale)

CRP_nb_female_nocovars <- glm.nb(PLE_total ~ CRP_log  ,
                                 data = dataTransFemale)
# Fully Adjusted
IL6_nb_female <- glm.nb(PLE_total ~ IL6_log +             
                          BMI_age9 +  
                          Maternal.education.at.birth,
                        data = dataTransFemale)

CRP_nb_female <- glm.nb(PLE_total ~ CRP_log +             
                          BMI_age9 +  
                          Maternal.education.at.birth,
                        data = dataTransFemale)

# ----------------------------------------------------------------------------
# Save the results in Output
options(scipen=4)

result_func <- function(model, covariates, exposure){
  results <- t(as.data.frame( summary(model)$coefficients[2,] ))
  lower <- confint(model, level = 0.95)[2,1]
  upper <- confint(model, level = 0.95)[2,2]
  results <- cbind(data.frame("Exposure" = exposure),
                   data.frame("Outcome" = "PEs"),
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
                    list(result_func(IL6_nb_nocovars, "Base Model", "log(IL-6)"),
                         result_func(CRP_nb_nocovars, "Base Model", "log(CRP)")))
results
write.csv(results, "Regression_analysis/Output/blood_PLE_base.csv", row.names = F)

results <- do.call( rbind, 
                    list(result_func(IL6_nb, "Fully Adjusted", "log(IL-6)"),
                         result_func(CRP_nb, "Fully Adjusted", "log(CRP)")))
results
write.csv(results, "Regression_analysis/Output/blood_PLE_fully_adjusted.csv", row.names = F)

# ----------------------------------------------------------------------------
# Split by sex
result_func_sex <- function(model, covariates, exposure, sex, dataFrame){
  results <- t(as.data.frame( summary(model)$coefficients[2,] ))
  lower <- confint(model, level = 0.95)[2,1]
  upper <- confint(model, level = 0.95)[2,2]
  results <- cbind(data.frame("Sex" = sex),
                   data.frame("Exposure" = exposure),
                   data.frame("Outcome" = "PEs"),
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
# Base Model
IL6_nb_male_nocovars <- glm.nb(PLE_total ~ IL6_log, data = dataTransMale)
CRP_nb_male_nocovars <- glm.nb(PLE_total ~ CRP_log , data = dataTransMale)
# Fully Adjusted
CRP_nb_male <- glm.nb(PLE_total ~ CRP_log  + BMI_age9 + Maternal.education.at.birth,
                      data = dataTransMale)
IL6_nb_male <- glm.nb(PLE_total ~ IL6_log +  BMI_age9 + Maternal.education.at.birth,
                      data = dataTransMale)

# Females
# Base Model
IL6_nb_female_nocovars <- glm.nb(PLE_total ~ IL6_log, data = dataTransFemale)
CRP_nb_female_nocovars <- glm.nb(PLE_total ~ CRP_log, data = dataTransFemale)
# Fully Adjusted
CRP_nb_female <- glm.nb(PLE_total ~ CRP_log  + BMI_age9 + Maternal.education.at.birth,
                        data = dataTransFemale)
IL6_nb_female <- glm.nb(PLE_total ~ IL6_log  +  BMI_age9 + Maternal.education.at.birth,
                        data = dataTransFemale)

results_sex <- do.call( rbind, 
                        list(result_func_sex(IL6_nb_male_nocovars, "Base Model", "log(IL-6)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_male_nocovars, "Base Model", "log(CRP)", "MALE", dataTransMale),
                             result_func_sex(IL6_nb_female_nocovars, "Base Model", "log(IL-6)", "FEMALE", dataTransFemale),
                             result_func_sex(CRP_nb_female_nocovars, "Base Model", "log(CRP)", "FEMALE", dataTransFemale))
)
results_sex
write.csv(results_sex, "Regression_analysis/Output/blood_PLE_sex_split_base.csv", row.names = F)

results_sex <- do.call( rbind, 
                        list(result_func_sex(IL6_nb_male, "Fully Adjusted", "log(IL-6)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_male, "Fully Adjusted", "log(CRP)", "MALE", dataTransMale),
                             result_func_sex(IL6_nb_female, "Fully Adjusted", "log(IL-6)", "FEMALE", dataTransFemale),
                             result_func_sex(CRP_nb_female, "Fully Adjusted", "log(CRP)", "FEMALE", dataTransFemale))
)
results_sex
write.csv(results_sex, "Regression_analysis/Output/blood_PLE_sex_split_fully_adjusted.csv", row.names = F)

# ----------------------------------------------------------------------------


