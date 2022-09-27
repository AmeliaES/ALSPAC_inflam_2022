# Sensitivity analysis removing ppl with infection present

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
library(MASS)
# ----------------------------------------------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")
# ----------------------------------------------------------------------------
# How many ppl with serum CRP and IL-6 data that had an infection in the last week or presently
# Infection_present: yes is 1, no is 2
# Negative values are missing data and should be changed to NA, apart from -2 which means no infection present.

summary(as.factor(data$Infection_days_since))

summary(as.factor(data$Infection_present))

data %>%
  filter(Infection_days_since %in% 0:7) %>%
  filter(Infection_present == 1) %>%
  nrow()
# maximum 678 ppl with an infection that could be removed from our sample (some of these may have high CRP so already excluded, or may have missing data for other variables)

#
data %>%
  filter(Infection_days_since == -2) %>%
  filter(Infection_present == 2) %>%
  nrow()
# Total 6880 max sample with no infection, include this filter for our dataTrans variable

# ----------------------------------------------------------------------------
# Sample sizes with serum CRP or IL-6, with and without infection excluded
data %>%
  filter(!is.na(CRP..age.9.)) %>%
  nrow()

data %>%
  filter(!is.na(CRP..age.9.) | !is.na(IL.6..age.9.)) %>%
  filter(Infection_days_since == -2) %>%
  filter(Infection_present == 2) %>%
  nrow()

5079-4590
# 489, Golam removed 491 from his, so this sounds like the correct number (may have been a couple of drop outs since his papers came out)


# out of this 489, some may have already been removed because they have CRP. How many is this?

data %>%
  filter(!is.na(CRP..age.9.) | !is.na(IL.6..age.9.)) %>%
  filter(Infection_days_since == -2) %>%
  filter(Infection_present == 2) %>%
  filter(CRP..age.9. >= 10) %>%
  nrow()
# 32 of those had high CRP, so had already been excluded in the main analysis

489 - 32
#####################################################################################
# so the sensitivity analysis below removes a further 457 ppl with an infection present in the last 7 days or at time of blood draw.
# 32 ppl with an infection present had already been removed from the main analysis as their CRP was >= 10 mg/L
#####################################################################################
# ----------------------------------------------------------------------------
# log transform and factorise vars
dataTrans <- data %>%
  filter(Infection_days_since == -2) %>%
  filter(Infection_present == 2) %>%
  rename(CRP = CRP..age.9.) %>%
  rename(IL6 = IL.6..age.9.) %>%
  filter(CRP < 10) %>%
  mutate(CRP_quartile = ntile(CRP, 4)) %>% # Create quartiles for inflammatory markers
  mutate(IL6_quartile = ntile(IL6, 4)) %>%
  mutate(CRP_log = log(CRP+1)) %>%
  mutate(IL6_log = log(IL6+1)) %>%
  mutate_at(c("Sex", "IL6_quartile", "CRP_quartile", "Maternal.social.class.at.birth", "Ethnicity", "Maternal.education.at.birth"), as.factor) %>%
  mutate_at(c("CRP", "IL6", "IL6_log", "CRP_log", "BMI_age9"), scale)  # z-scale continuous variables


# ----------------------------------------------------------------------------
# Depressive episodes
# Base model
IL6_dep_nocovars <- glm.nb(dep_episodes ~ IL6_log + Sex,
                           data = dataTrans)

CRP_dep_nocovars <- glm.nb(dep_episodes ~ CRP_log + Sex,
                           data = dataTrans)
# Fully Adjusted
IL6_dep <- glm.nb(dep_episodes ~ IL6_log +
                    Sex + 
                    BMI_age9 +  
                    Maternal.education.at.birth,
                  data = dataTrans)

CRP_dep <- glm.nb(dep_episodes ~ CRP_log +
                    Sex + 
                    BMI_age9 +  
                    Maternal.education.at.birth,
                  data = dataTrans)

# ---------------------------------
# Total PLEs
# Base model
IL6_ple_nocovars <- glm.nb(PLE_total ~ IL6_log + Sex,
                           data = dataTrans)

CRP_ple_nocovars <- glm.nb(PLE_total ~ CRP_log + Sex,
                           data = dataTrans)
# Fully Adjusted
IL6_ple <- glm.nb(PLE_total ~ IL6_log +
                    Sex + 
                    BMI_age9 +  
                    Maternal.education.at.birth,
                  data = dataTrans)

CRP_ple <- glm.nb(PLE_total ~ CRP_log +
                    Sex + 
                    BMI_age9 +  
                    Maternal.education.at.birth,
                  data = dataTrans)

# ----------------------------------------------------------------------------
# Save the results in Output
result_func <- function(model, covariates, exposure, outcome){
  results <- t(as.data.frame( summary(model)$coefficients[2,] ))
  lower <- confint(model, level = 0.95)[2,1]
  upper <- confint(model, level = 0.95)[2,2]
  results <- cbind(data.frame("Exposure" = exposure),
                   data.frame("Outcome" = outcome),
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
                    list(result_func(IL6_dep_nocovars, "Base Model", "log(IL-6)", "Depression episodes"),
                         result_func(CRP_dep_nocovars, "Base Model", "log(CRP)", "Depression episodes"),
                         result_func(IL6_dep, "Fully Adjusted", "log(IL-6)", "Depression episodes"),
                         result_func(CRP_dep, "Fully Adjusted", "log(CRP)", "Depression episodes"),
                         result_func(IL6_ple_nocovars, "Base Model", "log(IL-6)", "PEs"),
                         result_func(CRP_ple_nocovars, "Base Model", "log(CRP)", "PEs"),
                         result_func(IL6_ple, "Fully Adjusted", "log(IL-6)", "PEs"),
                         result_func(CRP_ple, "Fully Adjusted", "log(CRP)", "PEs")))

results
write.csv(results, "Regression_analysis/Output/infection_rm_sensitivity.csv", quote = F, row.names = F)

