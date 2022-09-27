# Create a table of sample sizes used in each model,
# to compare CC and imputation models

# ----------------------------------------------------------------------------
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
# ----------------------------------------------------------------------------
paths <- c("Regression_analysis/Output/", "Imputation/Output/")

getN <- function(path){
files <- c("combined_results_dep_base.csv",
           "combined_results_dep_fully_adjusted.csv",
           "combined_results_PLE_base.csv",
           "combined_results_PLE_fully_adjusted.csv")

results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T) %>%
                        #filter(!Covariates %in% c("Fully Adjusted (with cell estimates)" , "Fully_Adjusted_with_cell_estimates")) %>%
                        dplyr::select(Exposure, Sample_Size)
                      })
# Get sample sizes:
results <- results %>%
  reduce(left_join, by = "Exposure") %>%
  rename("Depression (Base)" = 2, "Depression (Fully Adjusted)" = 3, "PLE (Base)" = 4, "PLE (Fully Adjusted)" = 5)
return(results)
}

SampleN <- lapply(paths, getN)
SampleN[[2]] <- SampleN[[2]] %>%
                  dplyr::select(1:2) %>%
                  rename("Missing Imputed" = 2)

SampleN <- SampleN %>%
            reduce(left_join, by = "Exposure")

SampleN

write.csv(SampleN, "Imputation/Output/Sample_sizes.csv", row.names = F, quote = F)


