# Sensitivity analysis on PLEs at clinic time points

# ----------------------------------------
# Read in libraries and clear working envrironment
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/")

source("Data_prep/Scripts/startup.R")
library(MASS)
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")


# PLE total at consistent clinic time points
data$PLE_total_clinic_consistent <- rowSums(dplyr::select(data, c(ple_t02, ple_t06, ple_t08)), na.rm = T)

# some of those 0s should be NA though for people that didn't attend ANY appointments 
data$PLE_total_clinic_consistent[ rowSums(is.na( dplyr::select(data, c(ple_t02, ple_t06, ple_t08)) )) == 3 ] <- NA

# PLE total at clinic time points (interviewer rated)
data$PLE_total_clinic <- rowSums(dplyr::select(data, c(PSYCH_12_definite, PSYCH_18_definite, PSYCH_24_definite)), na.rm = T)
data$PLE_total_clinic[ rowSums(is.na( dplyr::select(data, c(PSYCH_12_definite, PSYCH_18_definite, PSYCH_24_definite)) )) == 3 ] <- NA

# ----------------------------------------
# Barplots: 1) showing number of PLEs at each time point, and 2) total number of PLEs


summary(as.factor(data$PLE_total_clinic))
# ----------------------------------------
PLE <- list("PSYCH_12_definite", "PSYCH_18_definite", "PSYCH_24_definite")
names(PLE) <- c("age12_", "age18_", "age24_")

PLEdf <- lapply(1:3, function(i){
  df <- data.frame(sum(dplyr::select(data, PLE[[i]]) == 1, na.rm = T),
                   sum(dplyr::select(data, PLE[[i]]) == 0, na.rm = T),
                   sum(is.na(dplyr::select(data, PLE[[i]]))) )
  colnames(df) <- c(paste0(names(PLE[i]), "True"), paste0(names(PLE[i]), "False"), paste0(names(PLE[i]), "Missing"))
  return(df)
}) %>% do.call(cbind, .) %>% t()

PLEdf


############################################
# Conduct sensitivity analysis of total PLEs (interviewer determined) with inflammatory exposures
############################################
# Define function to format results table
result_func <- function(model, covariates, exposure){
  results <- as.data.frame(t(as.data.frame( summary(model)$coefficients[2,] )))
  lower <- results$Estimate - (results$`Std. Error` * 1.96)
  upper <- results$Estimate + (results$`Std. Error` * 1.96)
  lower <- results[1,1] - (results[1,2]*1.96)
  upper <- results[1,1] + (results[1,2]*1.96)
  results <- cbind(data.frame("Exposure" = exposure),
                   data.frame("Outcome" = "PLEs (interviewer rated)"),
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
# ----------------------------------------
# Serum:
# log transform and factorise vars
dataTrans <- data %>%
  rename(CRP = CRP..age.9.) %>%
  rename(IL6 = IL.6..age.9.) %>%
  filter(CRP < 10) %>%
  mutate(CRP_quartile = ntile(CRP, 4)) %>% # Create quartiles for inflammatory markers
  mutate(IL6_quartile = ntile(IL6, 4)) %>%
  mutate(CRP_log = log(CRP+1)) %>%
  mutate(IL6_log = log(IL6+1)) %>%
  mutate_at(c("Sex", "IL6_quartile", "CRP_quartile", "Maternal.social.class.at.birth", "Maternal.education.at.birth"), as.factor)

# Z-scale continuous data
dataTrans <- dataTrans %>%
  mutate_at(c("CRP", "IL6", "IL6_log", "CRP_log", "BMI_age9"), scale)  # z-scale continuous variables

# Run models
serum <- do.call(rbind, 
                 list(
                   result_func(glm.nb(PLE_total_clinic ~ IL6_log + Sex , 
                                      data = dataTrans), "Base Model", "log(IL-6)"),
                   result_func(glm.nb(PLE_total_clinic ~ CRP_log + Sex, 
                                      data = dataTrans), "Base Model", "log(CRP)"),
                   result_func(glm.nb(PLE_total_clinic ~ IL6_log + Sex + BMI_age9 + Maternal.education.at.birth, 
                                      data = dataTrans), "Fully Adjusted", "log(IL-6)"),
                   result_func(glm.nb(PLE_total_clinic ~ CRP_log + Sex + BMI_age9 + Maternal.education.at.birth, 
                                      data = dataTrans), "Fully Adjusted", "log(CRP)")
                 ))
serum

# ----------------------------------------
# DNAm:
# Merge DNAm scores at age 0 and 7 (fill in NA for empty rows)
DNAm <- list(read.csv("DNAm/Output/DNAm_scores_age_0.csv", header = T),
             read.csv("DNAm/Output/DNAm_scores_age_7.csv", header = T))
# Merge these files into one data frame by the common column "Subject"
DNAm <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Subject"), DNAm)
nrow(DNAm) # 1017
colnames(DNAm) <- c("Subject","CRP_scores_0", "IL6_scores_0", "IL6_Gadd_scores_0",
                    "CRP_scores_7", "IL6_scores_7", "IL6_Gadd_scores_7")

dataDNAm <- merge(data, DNAm, by = "Subject")
head(dataDNAm)
nrow(dataDNAm) # 998

# -------------------------
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

dataDNAm <- merge(merge(dataDNAm, cell_counts_birth, by = "Subject"), 
                  cell_counts_F07, by = "Subject")

nrow(dataDNAm) # 877
# ----------------------------------------
# Include 10 PCs

PCs_0 <- read.csv("DNAm/Output/DNAm_PCs_age_0.csv")
PCs_7 <- read.csv("DNAm/Output/DNAm_PCs_age_7.csv")

PCs <- merge(PCs_0, PCs_7, by = "Subject", suffix = c("_0", "_7"))

dataDNAm <- merge(dataDNAm, PCs, by = "Subject")
# ----------------------------------------
# z-scale and factorise vars
dataTrans <- dataDNAm %>%
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

# ------------------
# Run models:
# Base model
# birth
CRP_nb_nocovars_0 <- glm.nb(PLE_total_clinic ~ CRP_scores_0 + Sex + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,  data = dataTrans)
IL6_nb_nocovars_0 <- glm.nb(PLE_total_clinic ~ IL6_scores_0 + Sex + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0, data = dataTrans)
# age 7
CRP_nb_nocovars_7 <- glm.nb(PLE_total_clinic ~ CRP_scores_7 + Sex + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,  data = dataTrans)
IL6_nb_nocovars_7 <- glm.nb(PLE_total_clinic ~ IL6_scores_7 + Sex + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7, data = dataTrans)

# Full Adjusted (including cell estimates)
# birth
CRP_nb_0 <- glm.nb(PLE_total_clinic ~ CRP_scores_0 + Sex + Maternal.education.at.birth + Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth + PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                   data = dataTrans)
IL6_nb_0 <- glm.nb(PLE_total_clinic ~ IL6_scores_0 + Sex + Maternal.education.at.birth +  Bcell_birth + CD4T_birth + CD8T_birth + Gran_birth + Mono_birth + NK_birth+ PC1_0 + PC2_0 + PC3_0 + PC4_0 + PC5_0 + PC6_0 + PC7_0 + PC8_0 + PC9_0 + PC10_0,
                   data = dataTrans)

# age 7
CRP_nb_7 <- glm.nb(PLE_total_clinic ~ CRP_scores_7 + Sex + BMI_age7 + Maternal.education.at.birth + Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07+ PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                   data = dataTrans)
IL6_nb_7 <- glm.nb(PLE_total_clinic ~ IL6_scores_7 +Sex +  BMI_age7 +   Maternal.education.at.birth +  Bcell_F07 + CD4T_F07 + CD8T_F07 + Gran_F07 + Mono_F07 + NK_F07 + PC1_7 + PC2_7 + PC3_7 + PC4_7 + PC5_7 + PC6_7 + PC7_7 + PC8_7 + PC9_7 + PC10_7,
                   data = dataTrans)

DNAm <- do.call( rbind, 
                 list(result_func(IL6_nb_nocovars_0, "Base Model", "DNAm IL-6 score (birth)"),
                      result_func(CRP_nb_nocovars_0, "Base Model", "DNAm CRP score (birth)"),
                      result_func(IL6_nb_nocovars_7, "Base Model", "DNAm IL-6 score (7 years)"),
                      result_func(CRP_nb_nocovars_7, "Base Model", "DNAm CRP score (7 years)"),
                      result_func(IL6_nb_0, "Fully Adjusted", "DNAm IL-6 score (birth)"),
                      result_func(CRP_nb_0, "Fully Adjusted", "DNAm CRP score (birth)"),
                      result_func(IL6_nb_7, "Fully Adjusted", "DNAm IL-6 score (7 years)"),
                      result_func(CRP_nb_7, "Fully Adjusted", "DNAm CRP score (7 years)")
                 ))
DNAm

# ----------------------------------------
# PRSs:
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

# Merge PRSs into data
dataPRS <- merge(data, PRSs, by = "Subject")
head(dataPRS)
nrow(dataPRS)

# z-scale and factorise vars
dataTrans <- dataPRS %>%
  mutate_at(c("Sex", "Maternal.social.class.at.birth", "Maternal.education.at.birth"), as.factor) %>%
  mutate_at(c("BMI_age7", "BMI_age9",  
              "CRP_SBayesR", "IL6_SBayesR"), scale)

# ------------------
# Run models:
# Base Model
# CRP 
# SBayesR
CRP_nb_nocovars_SBayesR <- glm.nb(PLE_total_clinic ~ CRP_SBayesR + Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTrans)

# IL-6
# SBayesR
IL6_nb_nocovars_SBayesR <- glm.nb(PLE_total_clinic ~ IL6_SBayesR + Sex + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTrans)

# Fully Adjusted
# CRP 
# SBayesR
CRP_nb_SBayesR <- glm.nb(PLE_total_clinic ~ CRP_SBayesR + Sex + Maternal.education.at.birth + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTrans)

# IL-6
# SBayesR
IL6_nb_SBayesR <- glm.nb(PLE_total_clinic ~ IL6_SBayesR + Sex + Maternal.education.at.birth + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  data = dataTrans)

PRS <- do.call( rbind, 
                list(result_func(IL6_nb_nocovars_SBayesR, "Base Model", "IL-6 PRS"),
                     result_func(CRP_nb_nocovars_SBayesR, "Base Model", "CRP PRS"),
                     result_func(IL6_nb_SBayesR, "Fully Adjusted", "IL-6 PRS"),
                     result_func(CRP_nb_SBayesR, "Fully Adjusted", "CRP PRS")
                ))
PRS
# ----------------------------------------
# Combine results
base <- do.call(rbind, 
                lapply(list(serum, DNAm, PRS), function(x){
                  filter(x, Covariates %in% "Base Model")
                })) %>%
  mutate(P_FDR_corrected = p.adjust(`P-value`, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)

fullyAd <- do.call(rbind, 
                   lapply(list(serum, DNAm, PRS), function(x){
                     filter(x, Covariates %in% "Fully Adjusted")
                   })) %>%
  mutate(P_FDR_corrected = p.adjust(`P-value`, method = "fdr")) %>%
  relocate(P_FDR_corrected, .before = Significant_covariates)

results <- rbind(base, fullyAd)
results


write.csv(results, "Regression_analysis/Output/PLE_sensitivity.csv", row.names = F, quote = F)
# ----------------------------------------
# Plot results
# Add colour for each bar
results <- results %>%
  mutate(`Inflammatory Marker` = 
           ifelse( str_detect(Exposure, "log") & Covariates == "Base Model" ,"Serum (base model)", 
                   ifelse( str_detect(Exposure, "log") & Covariates == "Fully Adjusted" ,"Serum (fully adjusted)", 
                           
                           ifelse( str_detect(Exposure, "PRS") & Covariates == "Base Model" ,"PRS (base model)", 
                                   ifelse( str_detect(Exposure, "PRS") & Covariates == "Fully Adjusted" ,"PRS (fully adjusted)", 
                                           
                                           ifelse( str_detect(Exposure, "DNAm") & Covariates == "Base Model" ,"DNAm score (base model)", 
                                                   ifelse( str_detect(Exposure, "DNAm") & Covariates == "Fully Adjusted" ,"DNAm score (fully adjusted)", 
                                                           ifelse( str_detect(Exposure, "DNAm") & Covariates == "Fully Adjusted (with cell estimates)" ,"DNAm score (fully adjusted\n- with cell estimates)", "NA"
                                                           )))))))   )
results

# Add order of legend labels
results$`Inflammatory Marker` <- factor(results$`Inflammatory Marker`, 
                                        levels = c("Serum (fully adjusted)","Serum (base model)","DNAm score (fully adjusted)", "DNAm score (base model)", "PRS (fully adjusted)", "PRS (base model)"),
                                        labels = c("Serum (fully adjusted)","Serum (base model)","DNAm score (fully adjusted)", "DNAm score (base model)", "PRS (fully adjusted)", "PRS (base model)"))

# Add order of y-axis labels
results$Exposure <- factor(results$Exposure, levels = 
                             rev(c("log(IL-6)", "log(CRP)", "DNAm IL-6 score (birth)","DNAm IL-6 score (7 years)",
                                   "DNAm CRP score (birth)",  "DNAm CRP score (7 years)", 
                                   "IL-6 PRS","CRP PRS"
                                   
                             )))

# Add significant * to plot
p <- "P_FDR_corrected"
results$sig_pos_FDR <- ""
if(any(pull(results, p) < 0.05)){
  results[pull(results, p) < 0.05,]$sig_pos_FDR <- "*"
}
if(any(pull(results, p) < 0.01)){
  results[pull(results, p) < 0.01,]$sig_pos_FDR <- "**"
}
if(any(pull(results, p) < 0.001)){
  results[pull(results, p) < 0.001,]$sig_pos_FDR <- "***"
}

results$sig_pos_uncorrected <- ""
if(any(pull(results, `P-value`) < 0.05)){
  results[pull(results, `P-value`) < 0.05,]$sig_pos_uncorrected <- "*"
}
if(any(pull(results, `P-value`) < 0.01)){
  results[pull(results, `P-value`) < 0.01,]$sig_pos_uncorrected <- "**"
}
if(any(pull(results, `P-value`) < 0.001)){
  results[pull(results, `P-value`) < 0.001,]$sig_pos_uncorrected <- "***"
}
results$sig_pos_uncorrected[results$sig_pos_FDR != ""] <- ""

results

p <- ggplot(data = results, aes(x = `Standardised Beta`, y = Exposure, fill = `Inflammatory Marker`)) +
  geom_col(position=position_dodge(0.6), width=0.6) +
  geom_text(aes(x = `Standardised Beta`, y = Exposure, label = sig_pos_FDR),
            position = position_dodge(1), vjust = 1, size = 5, hjust = -1, colour = "red") +
  geom_text(aes(x = `Standardised Beta`, y = Exposure, label = sig_pos_uncorrected),
            position = position_dodge(1), vjust = 0.5, size = 5, hjust = 1, colour = "blue") +
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
  # xlim(-0.4,0.45)  +
  theme(text = element_text(size = 15))

p

png("Regression_analysis/Output/PLE_sensitivity.png", width = 20, height = 15, units = 'cm', res = 300)
p
dev.off()


