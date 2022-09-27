# Correlation plot between DNAm scores, bloods, PRSs

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)

# ----------------------------------------------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# Log transform CRP and IL-6
dataTrans <- data %>%
  rename(CRP = CRP..age.9.) %>%
  mutate(CRP = replace(CRP, which(CRP >= 10), NA)) %>%
  rename(IL6 = IL.6..age.9.) %>%
  mutate(log_CRP_9 = log(CRP+1)) %>%
  mutate(log_IL6_9 = log(IL6+1))

# ----------------------------------------------------------------------------
# Include DNAm scores
# Merge DNAm scores at age 0, 7, 15, 17 (fill in NA for empty rows)
DNAm <- list(read.csv("DNAm/Output/DNAm_scores_age_0.csv", header = T),
             read.csv("DNAm/Output/DNAm_scores_age_7.csv", header = T),
             read.csv("DNAm/Output/DNAm_scores_age_15.csv", header = T),
             read.csv("DNAm/Output/DNAm_scores_age_17.csv", header = T))
# Merge these files into one data frame by the common column "Subject"
DNAm <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Subject"), DNAm)
nrow(DNAm) # 1033
colnames(DNAm) <- c("Subject","CRP_scores_0", "IL6_scores_0", "IL6_Gadd_scores_0",
                    "CRP_scores_7", "IL6_scores_7", "IL6_Gadd_scores_7",
                    "CRP_scores_15", "IL6_scores_15", "IL6_Gadd_scores_15",
                    "CRP_scores_17", "IL6_scores_17", "IL6_Gadd_scores_17")

nrow(data)
dataTrans <- left_join(dataTrans, DNAm, by = "Subject")

# ----------------------------------------------------------------------------
# Include PRSs
# Load in PRSs
PRSs <- list(read.table("PRS/Output/CRP_SBayesR.all_score", header = T),
             read.table("PRS/Output/IL6_SBayesR.all_score", header = T))

for(i in 1:length(PRSs)){
  colnames(PRSs[[i]])[1] <- "Subject" 
}
for(i in 1:length(PRSs)){
  colnames(PRSs[[i]])[3] <- "SBayesR" 
}
PRSs <- Reduce(function(x, y) merge(x, y, all = FALSE, by = "Subject"), PRSs)
PRSs <- PRSs %>%
  dplyr::select(c(Subject,CRP_SBayesR = SBayesR.x, IL6_SBayesR = SBayesR.y))

# Format subject to same as in data
stri_sub(PRSs$Subject, -1, -2) <- "_"

dataTrans <- left_join(dataTrans, PRSs, by = "Subject")
nrow(data)
nrow(dataTrans)

# ----------------------------------------------------------------------------
# Pearson correlation 

vars <- list(c("CRP_SBayesR", "log_CRP_9"),
          c("CRP_scores_0", "log_CRP_9"),
          c("CRP_scores_7", "log_CRP_9"),
          c("IL6_SBayesR", "log_IL6_9"),
          c("IL6_scores_0", "log_IL6_9"),
          c("IL6_scores_7", "log_IL6_9"),
          c("IL6_scores_0", "IL6_scores_7"),
          c("CRP_scores_0", "CRP_scores_7"))
sentences <- lapply(vars, function(x){
sub <- dataTrans %>%
  dplyr::select(x) %>%
  na.omit() 
test <- cor.test(sub[,1], sub[,2])
paste0("r = ", round(test$estimate, 3) , ", p = ", round(test$p.value, 4), ", n = ", nrow(sub))
})
names(sentences) <- lapply(vars, function(x) paste(x, collapse = "-"))
sentences

