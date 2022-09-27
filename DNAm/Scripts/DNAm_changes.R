# Changes on DNAm scores from birth to age 7 years
# ----------------------------------------------------------------------------

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
# Include 10 DNAm PCs

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
# Differences between birth and age 7 DNAm scores

# How many ppl have DNAm at age 0 and age 7
# 877

png("DNAm/Output/DNAm_age_changes.png", width = 20, height = 10, units = "cm", res = 300)
plot_grid(
ggplot(dataTrans, aes(CRP_scores_0, CRP_scores_7)) +
  geom_point(color = "gray50") +
  geom_smooth(method = "lm", se = FALSE, color = "mediumorchid") +
  ylab("DNAm CRP score (7 years)") +
  xlab("DNAm CRP score (birth)") +
  theme(text = element_text(size = 15))
,

ggplot(dataTrans, aes(IL6_scores_0, IL6_scores_7)) +
  geom_point(color = "gray50") +
  geom_smooth(method = "lm", se = FALSE, color = "mediumorchid") +
  ylab("DNAm IL-6 score (7 years)") +
  xlab("DNAm IL-6 score (birth)") +
  theme(text = element_text(size = 15))
, labels = c("A", "B"))
dev.off()

#-------------------------------
# Run regression and save residuals
fit_CRP <- lm(CRP_scores_7 ~ CRP_scores_0, data = dataTrans)
summary(fit_CRP)
dataTrans$CRP_scores_resid <- resid(fit_CRP)

fit_IL6 <- lm(IL6_scores_7 ~ IL6_scores_0, data = dataTrans)
summary(fit_IL6)
dataTrans$IL6_scores_resid <- resid(fit_IL6)

# Save results
results <- lapply(list(fit_CRP, fit_IL6), function(model){
results <- as.data.frame(t(as.data.frame( summary(model)$coefficients[2,] )))
lower <- confint(model, level = 0.95)[2,1]
upper <- confint(model, level = 0.95)[2,2]
row.names(results) <- NULL
results <- results %>%
  mutate(CI_lower = lower, CI_upper = upper)
}) %>% do.call(rbind,.)

results <- results %>%
  mutate("DNAm score changes" = c("CRP", "IL-6")) %>%
  relocate(`DNAm score changes`, .before = Estimate) %>%
  dplyr::select(-4) %>%
  rename("Standardised Beta" = Estimate,  "SE" = 3, "P-value" = 4 ) %>%
  relocate(CI_lower, .before = SE) %>%
  relocate(CI_upper, .before = SE) 

write.csv(results, "DNAm/Output/DNAm_age_changes.csv", row.names = F, quote = F)

#-------------------------------
# Associations with differences between 2 ages
dataTrans$CRP_scores_dif <- dataTrans$CRP_scores_7 - dataTrans$CRP_scores_0
dataTrans$IL6_scores_dif <- dataTrans$IL6_scores_7 - dataTrans$IL6_scores_0

CRP_nb_nocovars <- glm.nb(PLE_total ~ CRP_scores_dif ,  data = dataTrans)
summary(CRP_nb_nocovars)

IL6_nb_nocovars <- glm.nb(PLE_total ~ IL6_scores_dif ,  data = dataTrans)
summary(IL6_nb_nocovars)


CRP_nb_nocovars <- glm.nb(dep_episodes ~ CRP_scores_dif ,  data = dataTrans)
summary(CRP_nb_nocovars)

IL6_nb_nocovars <- glm.nb(dep_episodes ~ IL6_scores_dif ,  data = dataTrans)
summary(IL6_nb_nocovars)


#-------------------------------
# check associations with residuals
CRP_nb_nocovars <- glm.nb(PLE_total ~ CRP_scores_resid ,  data = dataTrans)
summary(CRP_nb_nocovars)

IL6_nb_nocovars <- glm.nb(PLE_total ~ IL6_scores_resid ,  data = dataTrans)
summary(IL6_nb_nocovars)


CRP_nb_nocovars <- glm.nb(dep_episodes ~ CRP_scores_resid ,  data = dataTrans)
summary(CRP_nb_nocovars)

IL6_nb_nocovars <- glm.nb(dep_episodes ~ IL6_scores_resid ,  data = dataTrans)
summary(IL6_nb_nocovars)

