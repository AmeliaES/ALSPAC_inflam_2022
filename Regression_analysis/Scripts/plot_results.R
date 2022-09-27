# Plot betas and CIs for results

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes")
source("Data_prep/Scripts/startup.R")
options(scipen=999)
library(cowplot)
##################################################

plotFunc <- function(files, sex){
path <- "Regression_analysis/Output/"
results <- lapply(files,
                      function(x){
                        read.csv(paste0(path, x), header = T)
                      }) %>% do.call(rbind, .)
results
if(sex == "FEMALE"){
results <-   results %>% 
    filter(Sex == sex)
}
if(sex == "MALE"){
  results <- results %>%
    filter(Sex == sex)
}else{
  results <- results
}
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
                                        levels = c("Serum (fully adjusted)","Serum (base model)","DNAm score (fully adjusted\n- with cell estimates)", "DNAm score (base model)", "PRS (fully adjusted)", "PRS (base model)"),
                                        labels = c("Serum (fully adjusted)","Serum (base model)","DNAm score (fully adjusted)", "DNAm score (base model)", "PRS (fully adjusted)", "PRS (base model)"))

# Add order of y-axis labels
results$Exposure <- factor(results$Exposure, levels = 
  rev(c("log(IL-6)", "log(CRP)", "DNAm IL-6 score (birth)","DNAm IL-6 score (7 years)",
    "DNAm CRP score (birth)",  "DNAm CRP score (7 years)", 
    "CRP PRS (SBayesR)",
     "IL-6 PRS (SBayesR)"
)))

# Add significant * to plot
p <- "P_FDR_corrected"
results$sig_pos_FDR <- ""
if(any(results$Standardised.Beta > 0 & pull(results, p) < 0.05)){
  results[results$Standardised.Beta > 0 & pull(results, p) < 0.05,]$sig_pos_FDR <- "*"
}
if(any(results$Standardised.Beta > 0 & pull(results, p) < 0.01)){
  results[results$Standardised.Beta > 0 & pull(results, p) < 0.01,]$sig_pos_FDR <- "**"
}
if(any(results$Standardised.Beta > 0 & pull(results, p) < 0.001)){
  results[results$Standardised.Beta > 0 & pull(results, p) < 0.001,]$sig_pos_FDR <- "***"
}

p <- "P.value"
results$sig_pos_uncorrected <- ""
if(any(results$Standardised.Beta > 0 & pull(results, p) < 0.05)){
  results[results$Standardised.Beta > 0 & pull(results, p) < 0.05,]$sig_pos_uncorrected <- "*"
}
if(any(results$Standardised.Beta > 0 & pull(results, p) < 0.01)){
  results[results$Standardised.Beta > 0 & pull(results, p) < 0.01,]$sig_pos_uncorrected <- "**"
}
if(any(results$Standardised.Beta > 0 & pull(results, p) < 0.001)){
  results[results$Standardised.Beta > 0 & pull(results, p) < 0.001,]$sig_pos_uncorrected <- "***"
}
results$sig_pos_uncorrected[results$sig_pos_FDR != ""] <- ""

p <- ggplot(data = results, aes(x = Standardised.Beta, y = Exposure, fill = `Inflammatory Marker`)) +
geom_col(position=position_dodge(0.6), width=0.6) +
  geom_text(aes(x = Standardised.Beta, y = Exposure, label = sig_pos_FDR),
            position = position_dodge(1), vjust = 1, size = 5, hjust = -1, colour = "red") +
  geom_text(aes(x = Standardised.Beta, y = Exposure, label = sig_pos_uncorrected),
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
  {if(sex == FALSE) xlim(-0.4,0.5) } +
  {if(sex %in% c("FEMALE", "MALE")) xlim(-0.6,0.56) } +
  theme(text = element_text(size = 15))
return(p)
}

p1 <- plotFunc(c("combined_results_dep_base.csv", "combined_results_dep_fully_adjusted.csv"), sex = FALSE)+ 
      theme(legend.position="none")
p2 <- plotFunc(c("combined_results_PLE_base.csv", "combined_results_PLE_fully_adjusted.csv"), sex = FALSE)+
      theme(axis.text.y = element_blank())

png("Regression_analysis/Output/combined_results_main.png", width = 27, height = 15, units = 'cm', res = 300)
ggarrange(p1, p2, labels = c("A", "B"), nrow = 1, ncol = 2, widths = c(0.85,1))
dev.off()

##################################################
# Split by sex:
p3 <- plotFunc(files = c("combined_results_dep_sex_split_base.csv", "combined_results_dep_sex_split_fully_adjusted.csv"), sex = "FEMALE")+ 
  theme(legend.position="none") +
  ggtitle("")
p4 <- plotFunc(files = c("combined_results_PLE_sex_split_base.csv", "combined_results_PLE_sex_split_fully_adjusted.csv"), sex = "FEMALE")+
  ggtitle("")+ 
  theme(legend.position="none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  theme(plot.margin=unit(c(0,0,0,0.25),"cm"))

p5 <- plotFunc(files = c("combined_results_dep_sex_split_base.csv", "combined_results_dep_sex_split_fully_adjusted.csv"), sex = "MALE")+ 
  theme(legend.position="none") +
  ggtitle("")
p6 <- plotFunc(files = c("combined_results_PLE_sex_split_base.csv", "combined_results_PLE_sex_split_fully_adjusted.csv"), sex = "MALE")+
  theme(legend.position="none") +
  ggtitle("") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  theme(plot.margin=unit(c(0,0,0,0.25),"cm"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)

png("Regression_analysis/Output/combined_results_sex_split.png", width = 30, height = 20, units = 'cm', res = 300)

plot_grid(plot_grid(plot_grid(p3, p4, rel_widths = c(0.47,0.3), labels = c("A", "B")),
                    plot_grid(p5, p6, rel_widths = c(0.47,0.3), labels = c("C", "D")), ncol = 1), mylegend, nrow = 1,
          rel_widths = c(1,0.3))

dev.off()




ggarrange(p1, p2, labels = c("A", "B"), nrow = 1, ncol = 2, widths = c(0.85,1))

plot_grid(plot_grid(plot_grid(p3, p4, rel_widths = c(0.47,0.3), labels = c("A", "B")),
                    plot_grid(p5, p6, rel_widths = c(0.47,0.3), labels = c("C", "D")), ncol = 1), mylegend, nrow = 1,
          rel_widths = c(1,0.3))
