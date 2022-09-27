# Associations between PRSs and depression episodes

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)


library(MASS)
# ----------------------------------------------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# ----------------------------------------------------------------------------
# Load in PRSs
PRSs <- list(read.table(paste0("PRS/Output/CRP_SBayesR.all_score"), header = T),
             read.table(paste0("PRS/Output/IL6_SBayesR.all_score"), header = T))

for(i in 1:length(PRSs)){
  colnames(PRSs[[i]])[1] <- "Subject" 
}
for(i in 1:length(PRSs)){
  colnames(PRSs[[i]])[3] <- "SBayesR" 
}
PRSs <- Reduce(function(x, y) merge(x, y, all = FALSE, by = "Subject"), PRSs)
PRSs <- PRSs %>%
  dplyr::select(c(Subject,
                  CRP_SBayesR = SBayesR.x, IL6_SBayesR = SBayesR.y))

# Format subject to same as in data
stri_sub(PRSs$Subject, -1, -2) <- "_"

# ----------------------------------------------------------------------------
# Merge PRSs into data
data <- merge(data, PRSs, by = "Subject")
head(data)
nrow(data)
# ----------------------------------------------------------------------------
# z-scale and factorise vars
dataTrans <- data %>%
  mutate_at(c("Sex", "Maternal.social.class.at.birth", "Maternal.education.at.birth"), as.factor) %>%
  mutate_at(c("BMI_age7", "BMI_age9",  
              "CRP_SBayesR", "IL6_SBayesR", paste("pc", 1:10, sep = "")), scale)
# ----------------------------------------------------------------------------
# Split dataTrans by sex 
dataTransMale <- dataTrans %>%
  filter(Sex == 0)

dataTransFemale <- dataTrans %>%
  filter(Sex == 1)

# ----------------------------------------------------------------------------
# Test for associations between depression episodes and PRSs for CRP and IL-6
##############################################################################
#############         Negative binomial models            ####################
##############################################################################
# Base Model
# CRP 
# SBayesR
CRP_nb_nocovars_SBayesR <- glm.nb(dep_episodes ~ CRP_SBayesR + Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTrans)

# IL-6
# SBayesR
IL6_nb_nocovars_SBayesR <- glm.nb(dep_episodes ~ IL6_SBayesR + Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTrans)

# Fully Adjusted
# CRP 
# SBayesR
CRP_nb_SBayesR <- glm.nb(dep_episodes ~ CRP_SBayesR + Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + Maternal.education.at.birth,  data = dataTrans)

# IL-6
# SBayesR
IL6_nb_SBayesR <- glm.nb(dep_episodes ~ IL6_SBayesR + Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + Maternal.education.at.birth,  data = dataTrans)
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
                    list(result_func(CRP_nb_nocovars_SBayesR, "Base Model", "CRP PRS (SBayesR)"),
                         result_func(IL6_nb_nocovars_SBayesR, "Base Model", "IL-6 PRS (SBayesR)")
                    ))
results
write.csv(results, "Regression_analysis/Output/PRS_dep_base.csv", row.names = F)

results <- do.call( rbind, 
                    list(result_func(CRP_nb_SBayesR, "Fully Adjusted", "CRP PRS (SBayesR)"),
                         result_func(IL6_nb_SBayesR, "Fully Adjusted", "IL-6 PRS (SBayesR)")
                    )
)
results
write.csv(results,"Regression_analysis/Output/PRS_dep_fully_adjusted.csv",row.names = F)

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
##############################################################################
# Base Model
# CRP 
# SBayesR
CRP_nb_male_nocovars_SBayesR <- glm.nb(dep_episodes ~ CRP_SBayesR + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTransMale)

# IL-6
# SBayesR
IL6_nb_male_nocovars_SBayesR <- glm.nb(dep_episodes ~ IL6_SBayesR + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTransMale)

# Fully Adjusted
# CRP 
# SBayesR
CRP_nb_male_SBayesR <- glm.nb(dep_episodes ~ CRP_SBayesR  + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + Maternal.education.at.birth,  data = dataTransMale)

# IL-6 
# SBayesR
IL6_nb_male_SBayesR <- glm.nb(dep_episodes ~ IL6_SBayesR  + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + Maternal.education.at.birth,  data = dataTransMale)

# Females
##############################################################################
# Base Model
# CRP 
# SBayesR
CRP_nb_female_nocovars_SBayesR <- glm.nb(dep_episodes ~ CRP_SBayesR + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTransFemale)

# IL-6
# SBayesR
IL6_nb_female_nocovars_SBayesR <- glm.nb(dep_episodes ~ IL6_SBayesR + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTransFemale)

# Fully Adjusted
# CRP 
# SBayesR
CRP_nb_female_SBayesR <- glm.nb(dep_episodes ~ CRP_SBayesR + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + Maternal.education.at.birth,  data = dataTransFemale)

# IL-6
# SBayesR
IL6_nb_female_SBayesR <- glm.nb(dep_episodes ~ IL6_SBayesR + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + Maternal.education.at.birth,  data = dataTransFemale)

results_sex <- do.call( rbind, 
                        list(
                             result_func_sex(CRP_nb_male_nocovars_SBayesR, "Base Model", "CRP PRS (SBayesR)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_female_nocovars_SBayesR, "Base Model", "CRP PRS (SBayesR)", "FEMALE", dataTransFemale),

                             result_func_sex(IL6_nb_male_nocovars_SBayesR, "Base Model", "IL-6 PRS (SBayesR)", "MALE", dataTransMale),
                             result_func_sex(IL6_nb_female_nocovars_SBayesR, "Base Model", "IL-6 PRS (SBayesR)", "FEMALE", dataTransFemale)
                             
       )
)

results_sex
write.csv(results_sex, "Regression_analysis/Output/PRS_dep_sex_split_base.csv",row.names = F)

results_sex <- do.call( rbind, 
                        list(result_func_sex(CRP_nb_male_SBayesR, "Fully Adjusted", "CRP PRS (SBayesR)", "MALE", dataTransMale),
                             result_func_sex(CRP_nb_female_SBayesR, "Fully Adjusted", "CRP PRS (SBayesR)", "FEMALE", dataTransFemale),
                             result_func_sex(IL6_nb_male_SBayesR, "Fully Adjusted", "IL-6 PRS (SBayesR)", "MALE", dataTransMale),
                             result_func_sex(IL6_nb_female_SBayesR, "Fully Adjusted", "IL-6 PRS (SBayesR)", "FEMALE", dataTransFemale)
                             
                        )
)

results_sex

write.csv(results_sex, "Regression_analysis/Output/PRS_dep_sex_split_fully_adjusted.csv", row.names = F)
