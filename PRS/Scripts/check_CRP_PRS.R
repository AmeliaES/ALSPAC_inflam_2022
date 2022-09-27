# Check CRP PRS and see if it associates with other phenotypes eg. CRP and BMI

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
library(MASS)

# ----------------------------------------------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# ----------------------------------------------------------------------------
# Load in PRSs
PRSs <- list(read.table(paste0("PRS/Output/CRP_SBayesR", ".all_score"), header = T),
             read.table(paste0("PRS/Output/IL6_SBayesR", ".all_score"), header = T))

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
  rename(CRP = CRP..age.9.) %>%
  rename(IL6 = IL.6..age.9.) %>%
  mutate(CRP_log = log(CRP+1)) %>%
  mutate(IL6_log = log(IL6+1)) %>%
  mutate_at(c("Sex", "Maternal.social.class.at.birth", "Ethnicity", "Maternal.education.at.birth"), as.factor) %>%
  mutate_at(c("BMI_age7", "BMI_age9", "CRP", "IL6", "IL6_log", "CRP_log", 
              "CRP_SBayesR", "IL6_SBayesR"), scale)

# ----------------------------------------------------------------------------
outcomes <- c("BMI_age7", "BMI_age9", "CRP", "IL6", "CRP_log", "IL6_log", "PLE_total", "PSYCH_18_definite" ,
              "PSYCH_24_definite", "PLE_24" , "PSYCH_24_disorder" )

results <- lapply(outcomes, function(outcome){
  form <- paste0(outcome, " ~ CRP_SBayesR + Sex + Ethnicity")
  fit <- glm(form,  data = dataTrans)
  summary(fit)
  results <- t(as.data.frame( summary(fit)$coefficients[2,] ))
  lower <- confint(fit, level = 0.95)[2,1]
  upper <- confint(fit, level = 0.95)[2,2]
  results <- cbind(data.frame("Exposure" = "CRP_PRS"),
                   data.frame("Outcome" = outcome),
                   data.frame("CI_lower" = lower),
                   data.frame("CI_upper" = upper),
                   data.frame("Sample_Size" = nrow(dataTrans) - length(summary(fit)$na.action) ),
                   results)
  row.names(results) <- NULL
  results
})

results <- do.call(rbind, results)

results

ggplot(data = results, aes(x = Estimate, y = Outcome)) +
  geom_col(position=position_dodge(0.6), width=0.6) +
  geom_errorbar(aes(xmin=CI_lower, xmax = CI_upper), width = 0.1, position = position_dodge(0.6), colour = "gray60")





