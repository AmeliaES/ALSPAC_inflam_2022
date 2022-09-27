# Regression analysis of MDD and SCZ PRS with dep episodes and PLEs
# --------------------------------------------------------------------------------
# Approx ~ 200,000 SNPs after clumping for both MDD and SCZ PRSs (see log files in ~/PRS/Output/*.log on Eddie)
# MDD GWAS = Howard et al. 2019
# SCZ GWAS = wave 3 (unpublished)
# distressing PLE GWAS = UKB Legge et al. 2018 
# --------------------------------------------------------------------------------
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/")

# Read in librarys and clear working envrironment
source("Data_prep/Scripts/startup.R")
library(MASS)

# ----------------------------------------------------------------------------
# Read in QC'd long data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# Load in PRSs
phenos <- c("MDD", "SCZ_w3", "any")
files <- paste0("PRS/Output/", phenos, "_SBayesR.all_score")

PRSs <- lapply(files, function(x) read.table(x, header = T))

for(i in 1:length(PRSs)){
  colnames(PRSs[[i]]) <- paste0(phenos[i], "_", colnames(PRSs[[i]]))
  colnames(PRSs[[i]])[1] <- "Subject" 
  PRSs[[i]] <- dplyr::select(PRSs[[i]], -2)
}

lapply(1:length(PRSs), function(i) colnames(PRSs[[i]])[2] <<- paste0(phenos[i], "_SBayesR") )

PRSs <- Reduce(function(x, y) merge(x, y, all = FALSE, by = "Subject"), PRSs)

# Format subject to same as in data
stri_sub(PRSs$Subject, -1, -2) <- "_"

scaleVars <- colnames(PRSs)[-1]

head(PRSs)
# ----------------------------------------------------------------------------
# Merge PRSs into data
data <- merge(data, PRSs, by = "Subject")
head(data)
nrow(data) # 8,804


data$PSYCH_18_definite # 1 = none, 2 = definite, 
data$PSYCH_24_definite # 0 = none, 1 = definite,
data$PSYCH_24_disorder # 0 = none, 1 = definite, 
sum(data$mfq_t01 >= 11, na.rm = T)

# z-scale and factorise vars
dataTrans <- data %>%
  mutate(mfq_t01_bin = data$mfq_t01 >= 11,
         mfq_t02_bin = data$mfq_t02 >= 11,
         mfq_t03_bin = data$mfq_t03 >= 11,
         mfq_t04_bin = data$mfq_t04 >= 11,
         mfq_t05_bin = data$mfq_t05 >= 11,
         mfq_t06_bin = data$mfq_t06 >= 11,
         mfq_t07_bin = data$mfq_t07 >= 11,
         mfq_t08_bin = data$mfq_t08 >= 11,
         mfq_t09_bin = data$mfq_t09 >= 11,
         mfq_t10_bin = data$mfq_t10 >= 11,
         mfq_t11_bin = data$mfq_t11 >= 11) %>%
  mutate(PSYCH_24_disorder = recode(PSYCH_24_disorder, `1`="1", `0`="0")) %>%
  mutate_at(c("Sex", "Ethnicity", "Maternal.education.at.birth",
              "ple_t06", "ple_t08", "ple_t09",
              "PSYCH_12_definite", "PSYCH_18_definite", "PSYCH_24_definite", "PSYCH_24_disorder",
              "mfq_t01_bin", "mfq_t02_bin", "mfq_t03_bin", "mfq_t04_bin","mfq_t05_bin",
              "mfq_t06_bin", "mfq_t07_bin","mfq_t08_bin","mfq_t09_bin","mfq_t10_bin", "mfq_t11_bin"), as.factor) %>%
  mutate_at(c("BMI_age7", "BMI_age9",  scaleVars), scale)



# --------------------------------------------------------------------------------
# Define function to combine results
resultsFunc <- function(outcome, outcomeName, predictor){
  
  # P-value thresholds
  prs <- str_subset(colnames(dataTrans), predictor) %>%
    str_subset("SBayesR", negate = F) 
  
  # Is the outcome categorical or continuous?
  outcomeClass <- dataTrans %>%
                    pull(outcome) %>%
                    class() 
  
  if(outcomeClass == "factor"){
    models <- lapply(c(prs), function(pT) {
        form <- paste0(outcome, " ~ ", pT, "+ Sex")
        return( glm(formula = form, data = dataTrans, family = "binomial") )
    })
  }else{
  models <- lapply(c(prs), function(pT) {
        form <- paste0(outcome, " ~ ", pT, "+ Sex")
        return(glm.nb(formula = form, data = dataTrans) )
  }) 
  }
  
  results <- lapply(models, function(x){
    resultsDF <- summary(x) 
    lower <- confint(x, level = 0.95)[2,1]
    upper <- confint(x, level = 0.95)[2,2]
    resultsDF <-as.data.frame(t(as.data.frame(resultsDF$coefficients[2,])))
    resultsDF$OR <- exp(resultsDF$Estimate)
    resultsDF$upper_CI_OR <- exp(upper)
    resultsDF$lower_CI_OR <- exp(lower)
    resultsDF$upper_CI_beta <- upper
    resultsDF$lower_CI_beta <- lower
    resultsDF$Outcome <- outcomeName
    resultsDF$N <- paste("N =", nrow(dataTrans) - length(summary(x)$na.action))
    return(resultsDF)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  results$PRS <- str_remove_all(prs, "`")
  row.names(results) <- NULL
  return(results)
}

###########################################
# ----------------------------------------
all_results <- 
  lapply(c("MDD", "SCZ", "any"), function(x){
list(
# Regression analysis of PRS with dep episodes
resultsFunc(outcome = "dep_episodes", outcomeName = "Total Deps",  predictor = x),
# ----------------------------------------
# Regression analysis of PRS with PLEs
resultsFunc(outcome = "PLE_total",outcomeName = "Total PLEs",predictor = x),
# ----------------------------------------
# PRS predicting PLE at age 18, 24 and 26 years
resultsFunc(outcome = "ple_t01", outcomeName = "PLEs (11)", predictor = x),
resultsFunc(outcome = "ple_t02", outcomeName = "PLEs (12)", predictor = x),
resultsFunc(outcome = "ple_t03", outcomeName = "PLEs (13)", predictor = x),
resultsFunc(outcome = "ple_t04", outcomeName = "PLEs (14)", predictor = x),
resultsFunc(outcome = "ple_t05", outcomeName = "PLEs (16)", predictor = x),
resultsFunc(outcome = "ple_t06", outcomeName = "PLEs (18)", predictor = x),
resultsFunc(outcome = "ple_t07", outcomeName = "PLEs (21)", predictor = x),
resultsFunc(outcome = "ple_t08", outcomeName = "PLEs (24)", predictor = x),
resultsFunc(outcome = "ple_t09", outcomeName = "PLEs (26)", predictor = x),
# ----------------------------------------
# PRS predicting definite PLEs determined by interviewer at ages 18 and 24 and also psychotic disorder at 24.
resultsFunc(outcome = "PSYCH_12_definite", outcomeName = "int (12)", predictor = x),
resultsFunc(outcome = "PSYCH_18_definite", outcomeName = "int (18)", predictor = x),
resultsFunc(outcome = "PSYCH_24_definite", outcomeName = "int (24)", predictor = x),
# resultsFunc(outcome = "PSYCH_24_disorder", outcomeName = "Disorder(24)", predictor = x),
# ----------------------------------------
# PRS predicting depression at age 18, 25 and 28 years
resultsFunc(outcome = "mfq_t01_bin", outcomeName = "Dep (10)", predictor = x),
resultsFunc(outcome = "mfq_t02_bin",  outcomeName = "Dep (12)", predictor = x),
resultsFunc(outcome = "mfq_t03_bin", outcomeName = "Dep (14)", predictor = x),
resultsFunc(outcome = "mfq_t04_bin",  outcomeName = "Dep (17)", predictor = x),
resultsFunc(outcome = "mfq_t05_bin", outcomeName = "Dep (18)", predictor = x),
resultsFunc(outcome = "mfq_t06_bin",  outcomeName = "Dep (19)", predictor = x),
resultsFunc(outcome = "mfq_t07_bin",  outcomeName = "Dep (22)", predictor = x),
resultsFunc(outcome = "mfq_t08_bin",  outcomeName = "Dep (23)", predictor = x),
resultsFunc(outcome = "mfq_t09_bin",  outcomeName = "Dep (24)", predictor = x),
resultsFunc(outcome = "mfq_t10_bin",  outcomeName = "Dep (26)", predictor = x),
resultsFunc(outcome = "mfq_t11_bin",  outcomeName = "Dep (28)",  predictor = x)
) %>%
      do.call(rbind, .)
    })

# ------------------------------------------------------------------------------------------------
# Plot figure for manuscript - plot ORs and CIs 

results <- do.call(rbind, all_results)

resultsMain <- results %>%
  filter(Outcome %in% c("Total Deps", "Total PLEs"))

head(resultsMain)

resultsMain$PRS <- factor(resultsMain$PRS, levels = rev(c("MDD_SBayesR", "any_SBayesR", "SCZ_w3_SBayesR")), 
                          labels = rev(c("MDD_SBayesR", "any_SBayesR", "SCZ_SBayesR") ))
resultsMain$Outcome <- factor(resultsMain$Outcome, labels = c("Total Deps" = "Total Depressive Episodes", "Total PLEs" ))


p = ggplot(data=resultsMain,
           aes(x = PRS,y = OR, ymin = lower_CI_OR, ymax = upper_CI_OR ))+
  geom_pointrange(aes(col=`PRS`))+
  geom_hline(aes(fill=PRS),yintercept =1, linetype=2)+
  xlab('')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower_CI_OR, ymax=upper_CI_OR,col=PRS),width=0.5,cex=1)+ 
  facet_wrap(~Outcome,strip.position="left",nrow=9,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"))+
  scale_color_manual(values = c( "MDD_SBayesR" = "royalblue3", 
                                 "any_SBayesR" = "indianred2", 
                                 "SCZ_SBayesR" = "hotpink2"),
                     labels = c("MDD_SBayesR" = "MDD",   
                                    "any_SBayesR" = "PLE (any) ", 
                                    "SCZ_SBayesR" = "SCZ"))+
  coord_flip() +
  geom_text(aes(x = 1, y = 1.2, label = N))

p

png("PRS/PRSs_predicting_dep_PLEs_manuscript.png",width = 12, height = 12, units = 'cm', res = 300)
print(p)
dev.off()


# -----------------------------------------------------------------
# Plot other phenotypes:

resultsMain <- results %>%
            filter(!str_detect(Outcome, "alt|Dep"))

resultsMain$PRS <- factor(resultsMain$PRS, levels = rev(c("MDD_SBayesR", "any_SBayesR", "SCZ_w3_SBayesR")), 
                          labels = rev(c("MDD_SBayesR", "any_SBayesR", "SCZ_SBayesR") ))
resultsMain$Outcome <- factor(resultsMain$Outcome)


p = ggplot(data=resultsMain,
           aes(x = PRS,y = OR, ymin = lower_CI_OR, ymax = upper_CI_OR ))+
  geom_pointrange(aes(col=`PRS`))+
  geom_hline(aes(fill=PRS),yintercept =1, linetype=2)+
  xlab('')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower_CI_OR, ymax=upper_CI_OR,col=PRS),width=0.5,cex=1)+ 
  facet_wrap(~Outcome,strip.position="left",nrow=13,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"))+
  scale_color_manual(values = c( "MDD_SBayesR" = "royalblue3", 
                                 "any_SBayesR" = "indianred2", 
                                 "SCZ_SBayesR" = "hotpink2"),
                     labels = c("MDD_SBayesR" = "MDD",   
                                "any_SBayesR" = "PLE (any) ", 
                                "SCZ_SBayesR" = "SCZ"))+
  coord_flip() +
  geom_text(aes(x = 3, y = 0.72, label = N))

png("PRS/PRSs_predicting_PLEs.png",width = 14, height = 23, units = 'cm', res = 300)
print(p)
dev.off()
p
# -----------------------------------------------------------------
# Plot other phenotypes:

resultsMain <- results %>%
  filter(!str_detect(Outcome, "alt|PLE|Disorder"))

resultsMain$PRS <- factor(resultsMain$PRS, levels = rev(c("MDD_SBayesR", "any_SBayesR", "SCZ_w3_SBayesR")), 
                          labels = rev(c("MDD_SBayesR", "any_SBayesR", "SCZ_SBayesR") ))
resultsMain$Outcome <- factor(resultsMain$Outcome)


p = ggplot(data=resultsMain,
           aes(x = PRS,y = OR, ymin = lower_CI_OR, ymax = upper_CI_OR ))+
  geom_pointrange(aes(col=`PRS`))+
  geom_hline(aes(fill=PRS),yintercept =1, linetype=2)+
  xlab('')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower_CI_OR, ymax=upper_CI_OR,col=PRS),width=0.5,cex=1)+ 
  facet_wrap(~Outcome,strip.position="left",nrow=13,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"))+
  scale_color_manual(values = c( "MDD_SBayesR" = "royalblue3", 
                                 "any_SBayesR" = "indianred2", 
                                 "SCZ_SBayesR" = "hotpink2"),
                     labels = c("MDD_SBayesR" = "MDD",   
                                "any_SBayesR" = "PLE (any) ", 
                                "SCZ_SBayesR" = "SCZ"))+
  coord_flip() +
  geom_text(aes(x = 3, y = 0.72, label = N))

png("PRS/PRSs_predicting_dep.png",width = 14, height = 23, units = 'cm', res = 300)
print(p)
dev.off()

# -----------------------------------------------------------------
sum(is.na(dataTrans$ple_t01))
sum(!is.na(dataTrans$ple_t01))
sum(dataTrans$ple_t01, na.rm = T)
sum(!dataTrans$ple_t01, na.rm = T)

nrow(dataTrans)



# Save results of other P-value thresholds as sensitivity analysis

resultsMain <- results %>%
  filter(Outcome %in% c("Total Deps", "Total PLEs")) %>%
  #filter(PRS %in% str_subset(PRS, "SCZ", negate = T) ) %>%
  mutate(P_FDR_corrected = p.adjust(`Pr(>|z|)`, method = "fdr") ) %>%
  dplyr::select(c(PRS, Outcome, OR, lower_CI = lower_CI_OR,
                  upper_CI = upper_CI_OR, P = `Pr(>|z|)`, P_FDR_corrected, N)) %>%
  mutate(PRS = str_replace(PRS, "e.0", "e-0") %>% 
           str_replace_all(., "_", " ") %>%
           str_replace(., " Pt", ": P =")) %>%
  mutate(N = str_replace(N, "N = ", "")) %>%
  mutate(Outcome = str_replace(Outcome, "Total Deps", "Total Depressive Episodes")) %>%
  arrange(Outcome) %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  mutate(P = replace(P, P == 0, "P < 0.0001")) %>%
  mutate(P_FDR_corrected = replace(P_FDR_corrected, P_FDR_corrected == 0, "P < 0.001"))
  
resultsMain
  
write.csv(resultsMain, "PRS/PRS_predict.csv", quote = F, row.names = F)

resultsMain <- read.csv("PRS/PRS_predict.csv")

# See which results were significant
sentences <- lapply(c(1,3,4,6), function(i){
paste0("OR = ", round(resultsMain[i,3],3) , ", 95% CIs = ", round(resultsMain[i,4],3) ,"-", round(resultsMain[i,5],3) , ", FDR corrected P = ", resultsMain[i,7])
})
names(sentences) <- sapply(c(1,3,4,6), function(i) paste(resultsMain[i,1:2], collapse = "; ") )
sentences


