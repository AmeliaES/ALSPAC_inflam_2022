# Combine results files into one table

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
library(xlsx)

# -----------------------------------------------
# Run the analysis
source("Regression_analysis/Scripts/blood_dep.R")
source("Regression_analysis/Scripts/blood_PLE.R")
source("Regression_analysis/Scripts/DNAm_dep.R")
source("Regression_analysis/Scripts/DNAm_PLE.R")
source("Regression_analysis/Scripts/PRS_dep.R")
source("Regression_analysis/Scripts/PRS_PLE.R")

# -----------------------------------------------

path <- "Regression_analysis/Output/"

dput(list.files(path, pattern = ".csv"))

# -----------------------------------------------
# Depression outcome
files <- c("blood_dep_base.csv", "DNAm_dep_base.csv", "PRS_dep_base.csv")

all_results <- lapply(files,
                       function(x){
                         read.csv(paste0(path, x), header = T)
                       }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
  filter(!grepl("T;", Exposure)) %>%
  mutate(P_FDR_corrected = ifelse(Covariates == "Base Model",
                                  p.adjust(P.value, method = "fdr"), NA )) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)
all_results
write.csv(all_results, paste0(path, "combined_results_dep_base.csv"), row.names = F)
# -----------------
files <- c("blood_dep_fully_adjusted.csv", "DNAm_dep_fully_adjusted_cc.csv", "PRS_dep_fully_adjusted.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

all_results <- all_results %>%
                  filter(!grepl("T;", Exposure)) %>%
                  mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
                  relocate(P_FDR_corrected, .before = Significant_covariates)
all_results$Significant_covariates[is.na(all_results$Significant_covariates)] <- ""
all_results
write.csv(all_results, paste0(path, "combined_results_dep_fully_adjusted.csv"), row.names = F)

# -----------------------------------------------
# PLE outcome
files <- c("blood_PLE_base.csv", "DNAm_PLE_base.csv", "PRS_PLE_base.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
                    filter(!grepl("T;", Exposure)) %>%
                   mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
                   relocate(P_FDR_corrected, .before = Significant_covariates)

all_results$Significant_covariates[is.na(all_results$Significant_covariates)] <- ""
all_results
write.csv(all_results, paste0(path, "combined_results_PLE_base.csv"), row.names = F)


files <- c("blood_PLE_fully_adjusted.csv", "DNAm_PLE_fully_adjusted_cc.csv", "PRS_PLE_fully_adjusted.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
  filter(!grepl("T;", Exposure)) %>%
  mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)

all_results$Significant_covariates[is.na(all_results$Significant_covariates)] <- ""
all_results
write.csv(all_results, paste0(path, "combined_results_PLE_fully_adjusted.csv"), row.names = F)

# -----------------------------------------------
# Split by sex
# Depression outcome
files <- c("blood_dep_sex_split_base.csv", "DNAm_dep_sex_split_base.csv", "PRS_dep_sex_split_base.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
  filter(!grepl("T;", Exposure)) %>%
  mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)

all_results
write.csv(all_results, paste0(path, "combined_results_dep_sex_split_base.csv"), row.names = F)

files <- c("blood_dep_sex_split_fully_adjusted.csv", "DNAm_dep_sex_split_fully_adjusted_cc.csv", "PRS_dep_sex_split_fully_adjusted.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
  filter(!grepl("T;", Exposure)) %>%
  mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)

all_results
write.csv(all_results, paste0(path, "combined_results_dep_sex_split_fully_adjusted.csv"), row.names = F)


# PLE outcome
files <- c("blood_PLE_sex_split_base.csv", "DNAm_PLE_sex_split_base.csv", "PRS_PLE_sex_split_base.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
  filter(!grepl("T;", Exposure)) %>%
  mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)
all_results
write.csv(all_results, paste0(path, "combined_results_PLE_sex_split_base.csv"), row.names = F)

files <- c("blood_PLE_sex_split_fully_adjusted.csv", "DNAm_PLE_sex_split_fully_adjusted_cc.csv", "PRS_PLE_sex_split_fully_adjusted.csv")

all_results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)

# Include an adjusted p-value col
all_results <- all_results %>%
  filter(!grepl("T;", Exposure)) %>%
  mutate(P_FDR_corrected = p.adjust(P.value, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)
all_results
write.csv(all_results, paste0(path, "combined_results_PLE_sex_split_fully_adjusted.csv"), row.names = F)


# -----------------------------------------------
# Plot the results
source("Regression_analysis/Scripts/plot_results.R")

# -----------------------------------------------
# Run PLE sensitivity
source("Regression_analysis/Scripts/PLE_sensitivity.R")

# -----------------------------------------------
# Results to paste into the manuscript
summary <- list(read.csv("Regression_analysis/Output/combined_results_dep_base.csv") %>%
  filter(P_FDR_corrected < 0.05),

read.csv("Regression_analysis/Output/combined_results_dep_fully_adjusted.csv") %>%
  filter(`P.value` < 0.05),


read.csv("Regression_analysis/Output/combined_results_PLE_base.csv") %>%
  filter(`P.value` < 0.05 | str_detect(Exposure, "CRP score \\(7")),



read.csv("Regression_analysis/Output/combined_results_PLE_fully_adjusted.csv") %>%
  filter(`P.value` < 0.05)) %>% do.call(rbind, .)



# make into a summary table
summary <- summary %>%
  mutate(Covariates = str_replace(Covariates, " \\(with cell estimates\\)", "")) %>%
  mutate_at(c("Standardised.Beta", "CI_lower", "CI_upper", "P.value", "P_FDR_corrected"), ~ round(as.numeric(.),3)) %>%
  mutate(CIs = paste0(CI_lower, "-", CI_upper)) %>%
  dplyr::select(c(Exposure, Outcome, Covariates, `Standardised Beta` = `Standardised.Beta`,
                  `95% CI` = CIs, `P (uncorrected)` = `P.value`, `P (FDR corrected)` = P_FDR_corrected, `Sample Size` = Sample_Size)) %>%
  arrange(desc(Exposure))


file <- paste0("Manuscript/Tables/Tab_5.xlsx")
system(paste0("rm ", file))
write.xlsx(summary, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = TRUE)

wb <- loadWorkbook(file)
sheets <- getSheets(wb)
autoSizeColumn(sheets[[1]], colIndex = 1:ncol(summary))
saveWorkbook(wb, file)







