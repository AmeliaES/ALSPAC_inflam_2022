# Create a table of sample sizes used in each model,
# to compare CC and imputation models

# ----------------------------------------------------------------------------
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
# ----------------------------------------------------------------------------
paths <- c("Regression_analysis/Output/", "Imputation/Output/")

getBetaSE <- function(path){
  files <- c("combined_results_dep_base.csv",
             "combined_results_dep_fully_adjusted.csv",
             "combined_results_PLE_base.csv",
             "combined_results_PLE_fully_adjusted.csv")
  
  results <- lapply(files,
                    function(x){
                      df <- read.csv(paste0(path, x), header = T) %>%
                        filter(!Covariates %in% c("Fully Adjusted (with cell estimates)" , "Fully_Adjusted_with_cell_estimates")) %>%
                        dplyr::select(c(1,5,8))
                      
                      df$Beta_SE <- paste0( round(df[[2]], 3) , " (", round(df[[3]], 3), ")" )
                      dplyr::select(df, c(1,4))
                    })
  
  results <- results %>%
    reduce(left_join, by = "Exposure") %>%
    rename("Depression (Base)" = 2, "Depression (Fully Adjusted)" = 3, "PLE (Base)" = 4, "PLE (Fully Adjusted)" = 5)
  return(results)
}

Betas <- lapply(paths, getBetaSE)
Betas

write.csv(Betas[[1]], "Imputation/Output/Betas_SE_CC.csv", row.names = F, quote = F)
write.csv(Betas[[2]], "Imputation/Output/Betas_SE_imputed.csv", row.names = F, quote = F)


