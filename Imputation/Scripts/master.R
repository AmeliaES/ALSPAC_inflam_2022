# Run imputation analysis

# ----------------------------------------------------------------------------
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)

library(cowplot)
library(mice)
library(MASS)

nImputations <- 100

marker <- "CRP"
source("Imputation/Scripts/Impute_missing_bloods.R")
# marker <- "IL6"
# source("Imputation/Scripts/Impute_missing_bloods.R")
# source("Imputation/Scripts/Impute_missing_PRS.R")
# source("Imputation/Scripts/Impute_missing_DNAm.R")

# ----------------------------------------------------------------------------
# Read in the results, combine depression and PLEs into 2 separate files, do FDR correction on base and FA models separately.

all_results <- lapply(c("imputed_results_bloods_IL6.csv","imputed_results_bloods_CRP.csv", "imputed_results_DNAm.csv", "imputed_results_PRS.csv"),
                  function(x)
                  read.csv(paste0("Imputation/Output/", x), header = T) )
all_results <- all_results %>%
                do.call(rbind, .) %>%
                mutate(CI_upper = Standardised_Beta + (1.96*SE),
                       CI_lower = Standardised_Beta - (1.96*SE)) %>%
                relocate(CI_lower, .after = Standardised_Beta) %>%
                relocate(CI_upper, .after = CI_lower) 

all_results$Exposure <- 
  ifelse(all_results$Exposure == "IL6_log", "log(IL-6)",
  ifelse(all_results$Exposure == "CRP_log", "log(CRP)",
  ifelse(all_results$Exposure == "CRP_scores_0", "DNAm CRP score (birth)",
  ifelse(all_results$Exposure == "IL6_scores_0", "DNAm IL-6 score (birth)",         
  ifelse(all_results$Exposure == "CRP_scores_7", "DNAm CRP score (7 years)",
  ifelse(all_results$Exposure == "IL6_scores_7", "DNAm IL-6 score (7 years)",       
  ifelse(all_results$Exposure == "CRP_SBayesR", "CRP PRS (SBayesR)",
  ifelse(all_results$Exposure == "IL6_SBayesR", "IL-6 PRS (SBayesR)", NA))))))))

all_results %>% head()

# Change "PLEs" to "PEs"
all_results <- all_results %>%
  mutate(Outcome = replace(Outcome, str_detect(all_results$Outcome, "PLE"), "PE_total"))


# ----------------------------------------------------------------------------
# Depression outcome - base model

# Include an adjusted p-value col
results <- all_results %>%
  filter(Covariates == "Base_Model") %>%
  filter(Outcome == "dep_episodes") %>%
  mutate(P_FDR_corrected =  p.adjust(P.value, method = "fdr")) 
results

write.csv(results, "Imputation/Output/combined_results_dep_base.csv", row.names = F, quote = F)

# -----------------
# Depression outcome - fully adjusted models

# Include an adjusted p-value col
results <- all_results %>%
  filter(Covariates != "Base_Model") %>%
  filter(Outcome == "dep_episodes") %>%
  mutate(P_FDR_corrected =  p.adjust(P.value, method = "fdr")) 
results

write.csv(results, "Imputation/Output/combined_results_dep_fully_adjusted.csv", row.names = F, quote = F)

# -----------------------------------------------
# PLE outcome - base model

# Include an adjusted p-value col
results <- all_results %>%
  filter(Covariates == "Base_Model") %>%
  filter(Outcome == "PE_total") %>%
  mutate(P_FDR_corrected =  p.adjust(P.value, method = "fdr")) 
results

write.csv(results, "Imputation/Output/combined_results_PLE_base.csv", row.names = F, quote = F)

# -----------------
# PLE - fully adjusted models

# Include an adjusted p-value col
results <- all_results %>%
  filter(Covariates != "Base_Model") %>%
  filter(Outcome == "PE_total") %>%
  mutate(P_FDR_corrected =  p.adjust(P.value, method = "fdr")) 
results

write.csv(results, "Imputation/Output/combined_results_PLE_fully_adjusted.csv", row.names = F, quote = F)

# ----------------------------------------------------------------------------
# Plot the figure

plotFunc <- function(files){
  path <- "Imputation/Output/"
  results <- lapply(files,
                    function(x){
                      read.csv(paste0(path, x), header = T)
                    }) %>% do.call(rbind, .)
  # Add colour for each bar
  results <- results %>%
    mutate(`Inflammatory Marker` = 
            ifelse( str_detect(Exposure, "log") & Covariates == "Base_Model" ,"Serum (base model)", 
            ifelse( str_detect(Exposure, "log") & Covariates == "Fully_Adjusted" ,"Serum (fully adjusted)", 
                             
            ifelse( str_detect(Exposure, "SBayesR") & Covariates == "Base_Model" ,"PRS (base model)", 
            ifelse( str_detect(Exposure, "SBayesR") & Covariates == "Fully_Adjusted" ,"PRS (fully adjusted)", 
                                             
            ifelse( str_detect(Exposure, "DNAm") & Covariates == "Base_Model" ,"DNAm score (base model)", 
            ifelse( str_detect(Exposure, "DNAm") & Covariates == "Fully_Adjusted" ,"DNAm score (fully adjusted)", NA ))))))   )
  
  # Add order of legend labels
  results$`Inflammatory Marker` <- factor(results$`Inflammatory Marker`, levels = c("Serum (fully adjusted)","Serum (base model)","DNAm score (fully adjusted)", "DNAm score (base model)", "PRS (fully adjusted)", "PRS (base model)"
  ))
  results
  # Add order of y-axis labels
  results$Exposure <- factor(results$Exposure, levels = 
                               rev(c("log(IL-6)", "log(CRP)", "DNAm IL-6 score (birth)","DNAm IL-6 score (7 years)",
                                     "DNAm CRP score (birth)",  "DNAm CRP score (7 years)", 
                                     "CRP PRS (SBayesR)",
                                     "IL-6 PRS (SBayesR)"
                               )))
  results
  # Add significant * to plot
  p <- "P_FDR_corrected"
  results$sig_pos_FDR <- ""
  if(any(results$Standardised_Beta > 0 & pull(results, p) < 0.05)){
    results[results$Standardised_Beta > 0 & pull(results, p) < 0.05,]$sig_pos_FDR <- "*"
  }
  if(any(results$Standardised_Beta > 0 & pull(results, p) < 0.01)){
    results[results$Standardised_Beta > 0 & pull(results, p) < 0.01,]$sig_pos_FDR <- "**"
  }
  if(any(results$Standardised_Beta > 0 & pull(results, p) < 0.001)){
    results[results$Standardised_Beta > 0 & pull(results, p) < 0.001,]$sig_pos_FDR <- "***"
  }
  
  p <- "P.value"
  results$sig_pos_uncorrected <- ""
  if(any(results$Standardised_Beta > 0 & pull(results, p) < 0.05)){
    results[results$Standardised_Beta > 0 & pull(results, p) < 0.05,]$sig_pos_uncorrected <- "*"
  }
  if(any(results$Standardised_Beta > 0 & pull(results, p) < 0.01)){
    results[results$Standardised_Beta > 0 & pull(results, p) < 0.01,]$sig_pos_uncorrected <- "**"
  }
  if(any(results$Standardised_Beta > 0 & pull(results, p) < 0.001)){
    results[results$Standardised_Beta > 0 & pull(results, p) < 0.001,]$sig_pos_uncorrected <- "***"
  }
  results$sig_pos_uncorrected[results$sig_pos_FDR != ""] <- ""
  
  p <- ggplot(data = results, aes(x = Standardised_Beta, y = Exposure, fill = `Inflammatory Marker`)) +
    geom_col(position=position_dodge(0.6), width=0.6) +
    geom_text(aes(x = Standardised_Beta, y = Exposure, label = sig_pos_FDR),
              position = position_dodge(1), vjust = 1, size = 5, hjust = -1, colour = "red") +
    geom_text(aes(x = Standardised_Beta, y = Exposure, label = sig_pos_uncorrected),
              position = position_dodge(1), vjust = 1, size = 5, hjust = -1, colour = "blue") +
    geom_errorbar(aes(xmin=CI_lower, xmax = CI_upper), width = 0.1, position = position_dodge(0.6), colour = "gray60")+
    xlab('Effect Size (β)') +
    ylab('') +
    theme(strip.text.y = element_text(face = "bold", size=8, angle = 270)) +
    theme(panel.spacing.y=unit(1, "lines")) + 
    theme(plot.margin = unit(c(1,0,1,-1), "mm")) +
    scale_fill_manual(
      values = c("Serum (base model)"    = "#b7de54",
                 "Serum (fully adjusted)"    = "#63782e",
                 "DNAm score (base model)" = "mediumpurple1",
                 "DNAm score (fully adjusted)" = "mediumorchid",
                 "PRS (base model)" = "orange",
                 "PRS (fully adjusted)" = "darkorange3")
    ) +
   xlim(-0.4,0.45) +
    theme(text = element_text(size = 15))
  return(p)
}

p1 <- plotFunc(c("combined_results_dep_base.csv", "combined_results_dep_fully_adjusted.csv"))+ 
  theme(legend.position="none")
p2 <- plotFunc(c("combined_results_PLE_base.csv", "combined_results_PLE_fully_adjusted.csv"))+
  theme(axis.text.y = element_blank())

ggarrange(p1, p2, labels = c("A", "B"), nrow = 1, ncol = 2, widths = c(0.85,1))

png("Imputation/Output/combined_results_main.png", width = 27, height = 15, units = 'cm', res = 300)
ggarrange(p1, p2, labels = c("A", "B"), nrow = 1, ncol = 2, widths = c(0.85,1))
dev.off()


# -----------------------------------------------
# Calculate sample sizes and betas
source("Imputation/Scripts/SampleSizes.R")
source("Imputation/Scripts/BetasSEs.R")
# -----------------------------------------------



