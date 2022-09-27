# Check correlations between serum CRP and CRP PRSs calculated with SbayesR and C+T
# SBayesR dropped lots of SNPs

setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/")

library(dplyr)
library(stringr)
library(stringi)

# ----------------------------------
# Load in serum data
data <- read.csv("dataSubQCWide.csv")

data %>%
	filter(!is.na(`CRP..age.9.`) | !is.na(`IL.6..age.9.`)) %>%
	nrow()
# -----------------------------------
# Load in PRSs
PRSs <- list(read.table(paste0("PRS/Output/CRP_SBayesR.all_score"), header = T),
             read.table(paste0("PRS/Output/IL6_SBayesR.all_score"), header = T),
             read.table(paste0("PRS/Output/CRP_CT.all_score"), header = T),
             read.table(paste0("PRS/Output/IL6_CT.all_score"), header = T))

for(i in 1:length(PRSs)){
  colnames(PRSs[[i]])[1] <- "Subject" 
}
for(i in 1:2){
  colnames(PRSs[[i]])[3] <- "SBayesR" 
}
PRSs <- Reduce(function(x, y) merge(x, y, all = FALSE, by = "Subject", suffixes = c("_CRP", "_IL6")), PRSs)

# Format subject to same as in data
stri_sub(PRSs$Subject, -1, -2) <- "_"

# ----------------------------------------------------------------------------
# Merge PRSs into data
dataSub <- merge(data, PRSs, by = "Subject")
head(dataSub)
nrow(dataSub)


# -----------------------------------
dataPRSs <- dataSub %>%
				select("CRP" = `CRP..age.9.`,
					"IL6" = `IL.6..age.9.`,
					SBayesR_CRP, SBayesR_IL6,
					"Pt_5e.08_CRP" = Pt_5e.08, 
					Pt_0.05_CRP, 
					Pt_1_CRP,
					Pt_5e.06_IL6,
					Pt_0.05_IL6,
					Pt_1_IL6) %>%
				filter(!is.na(CRP) & !is.na(IL6)) 

nrow(dataPRSs) # 4054

res <- cor(dataPRSs)
round(res, 2)

library(corrplot)
corrplot(res, tl.col = "black", tl.srt = 45)

