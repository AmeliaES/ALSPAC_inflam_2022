# Create demographic table including exposures, outcomes and covariates
# Separate table for each sample: bloods, DNAm and PRSs

# Get the number of males and females who experienced at least one depressive episode or PLE
# ----------------------------------------
# Sex, Maternal Education at birth,  

# CRP (age 9), IL-6 (age 9), BMI (age 9),
# DNAm scores (birth and age 7) (IL-6 and CRP), BMI (age 7),
# PRS (IL-6 and CRP); SBayesR; no MHC.

# ----------------------------------------
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/Data_prep/")

# Read in librarys and clear working envrironment
source("Scripts/startup.R")
library(gtsummary)
library(gt)
library(forcats)
library("xlsx")
# webshot::install_phantomjs() # /Users/aes/Library/Application Support/PhantomJS


# ----------------------------------------
#### BLOODS ################################
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# make dataset with a few variables to summarize
# individuals with either CRP OR IL-6 serum data (same subset as imputation model)
# Missing categorical values coded as "Missing"

# Make separate tables for CRP vs IL-6 datasets as they had slightly different numbers:


makeSerumTab <- function(marker){
dataSub <- data %>% 
  select(Sex, 
         `Maternal Education` = Maternal.education.at.birth, 
         `BMI (age 9 years)` = BMI_age9,
         `CRP (age 9 years)` = CRP..age.9., 
         `IL-6 (age 9 years)` = IL.6..age.9.,
          `Total Depressive Episodes` = dep_episodes,
         `Total PEs` = PLE_total
        ) %>%
  mutate_at(c("Sex", "Maternal Education", "Total Depressive Episodes", "Total PEs"), as.factor) %>%
  mutate_at(c("Maternal Education", "Total Depressive Episodes", "Total PEs", "Sex"), function(x) fct_explicit_na(x, na_level = "Missing")) 

dataSub$`Maternal Education` <- recode_factor(dataSub$`Maternal Education`, `0` = "CSE/O-level/Vocational", `1` = "A-level/Degree")

dataSub$Sex <- recode_factor(dataSub$Sex, `1` = "Female", `0` = "Male")

# If marker is CRP or IL-6 make different tables:
# For both remove ppl with high CRP
if(marker == "CRP"){
  dataSub <- dataSub %>% filter(!is.na(`CRP (age 9 years)`)) %>%
    filter(`CRP (age 9 years)` < 10) %>%
    dplyr::select(-`IL-6 (age 9 years)`)
}
if(marker == "IL6"){
  dataSub <- dataSub %>% filter(!is.na(`IL-6 (age 9 years)`)) %>%
    filter(`CRP (age 9 years)` < 10) %>%
    dplyr::select(-`CRP (age 9 years)`)
}
if(marker == "CRP_sens"){
  dataSub <- dataSub %>% filter(!is.na(`CRP (age 9 years)`)) %>%
    dplyr::select(-`IL-6 (age 9 years)`)
}



demographic <- tbl_summary(dataSub, by = Sex, statistic = list(all_continuous() ~ "{mean} ({sd})"), missing_text = "Missing") %>%
  modify_header(label = "Variable") 

# Save as csv:
demTab <- demographic %>% as_tibble() %>% as.data.frame()

# Now for the most horrendous code ever to change cells with values <5 to paste "< 5"
# Females:
demTab[,2][as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2])
           & !is.na(as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2]))] <-
  paste0("<5 ", sub("*.\\ ", "", demTab[,2][as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2])
                                             & !is.na(as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2]))]))

demTab[,2][-grep("[(]", demTab[,2])][which(as.numeric(demTab[,2][-grep("[(]", demTab[,2])]) < 5)] <- "<5"

# Males:
demTab[,3][as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3])
           & !is.na(as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3]))] <-
  paste0("<5 ", sub("*.\\ ", "", demTab[,3][as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3])
                                             & !is.na(as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3]))]))

demTab[,3][-grep("[(]", demTab[,3])][which(as.numeric(demTab[,3][-grep("[(]", demTab[,3])]) < 5)] <- "<5"

Nf <- gsub(".*,\ ", "", colnames(demTab)[2])
Nm <- gsub(".*,\ ", "", colnames(demTab)[3])
Nmiss <- gsub(".*,\ ", "", colnames(demTab)[4])
colnames(demTab)[2:4] <- c("Female", "Male", "Missing")
demTab <- rbind(data.frame(Variable = "", Female = Nf, Male = Nm, `Missing` = Nmiss), 
                demTab,
                data.frame(Variable = "n (%); Mean (SD)", Female = "", Male = "", `Missing` = "")) %>%
  mutate_if(is.character, ~replace(., is.na(.), ""))
demTab
write.csv(demTab, paste0("Output/demographicTableBloods_", marker, ".csv"))
file <- paste0("Output/demographicTableBloods_", marker, ".xlsx")
system(paste0("rm ", file))
write.xlsx(demTab, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = TRUE)

wb <- loadWorkbook(file)
sheets <- getSheets(wb)
autoSizeColumn(sheets[[1]], colIndex = 1:ncol(demTab))
saveWorkbook(wb, file)
}

makeSerumTab(marker = "CRP")
# Sensitivity analysis included ppl with CRP >= 10 mg/L, include a demographic table for this too
makeSerumTab(marker = "CRP_sens")
makeSerumTab(marker = "IL6")


# ----------------------------------------
#### DNAm ################################
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# Merge DNAm scores at age 0, 7, 15, 17 (fill in NA for empty rows)
DNAm <- list(read.csv("../DNAm/Output/DNAm_scores_age_0.csv", header = T),
             read.csv("../DNAm/Output/DNAm_scores_age_7.csv", header = T))
# Merge these files into one data frame by the common column "Subject"
DNAm <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Subject"), DNAm)
nrow(DNAm) # 1017
colnames(DNAm) <- c("Subject","CRP_scores_0", "IL6_scores_0", "IL6_Gadd_scores_0",
                    "CRP_scores_7", "IL6_scores_7", "IL6_Gadd_scores_7")

data <- merge(data, DNAm, by = "Subject")

# make dataset with a few variables to summarize
dataSub <- data %>% 
  filter(!is.na(CRP_scores_0) & !is.na(CRP_scores_7)) %>%
  filter(!is.na(IL6_scores_0) & !is.na(IL6_scores_7)) %>%
  mutate_at(c("CRP_scores_0", "CRP_scores_7", "IL6_scores_0", "IL6_scores_7"), scale) %>%
  mutate_at(c("CRP_scores_0", "CRP_scores_7", "IL6_scores_0", "IL6_scores_7"), as.numeric) %>%
  select(Sex, 
         `Maternal Education` = Maternal.education.at.birth,
         `BMI (age 7 years)` = BMI_age7,
        `CRP DNAm score (at birth)` = CRP_scores_0,
        `CRP DNAm score (age 7 years)` = CRP_scores_7,
        `IL-6 DNAm score (at birth)` = IL6_scores_0,
        `IL-6 DNAm score (age 7 years)` = IL6_scores_7,
        `Total Depressive Episodes` = dep_episodes,
        `Total PEs` = PLE_total
  ) %>%
    mutate_at(c("Sex", "Maternal Education" , "Total Depressive Episodes", "Total PEs"), as.factor) %>%
  mutate_at(c("Maternal Education", "Total Depressive Episodes", "Total PEs", "Sex"), function(x) fct_explicit_na(x, na_level = "Missing")) 

dataSub$`Maternal Education` <- recode_factor(dataSub$`Maternal Education`, `0` = "CSE/O-level/Vocational", `1` = "A-level/Degree")

dataSub$Sex <- recode_factor(dataSub$Sex, `1` = "Female", `0` = "Male")

demographic <- tbl_summary(dataSub, by = Sex, statistic = list(all_continuous() ~ "{mean} ({sd})"), missing_text = "Missing") %>%
  modify_header(label = "Variable") 


demographic

demographic %>%
  as_gt() %>%
  gt::gtsave(filename = "Output/demographicTableDNAm.png")


# Save as csv:
demTab <- demographic %>% as_tibble() %>% as.data.frame()

demTab
# Now for the most horrendous code ever to change cells with values <5 to paste "< 5"
# Females:
demTab[,2][as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2])
           & !is.na(as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2]))] <-
  paste0("<5 ", sub("*.\\ ", "", demTab[,2][as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2])
                                            & !is.na(as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2]))]))

demTab[,2][-grep("[(]", demTab[,2])][which(as.numeric(demTab[,2][-grep("[(]", demTab[,2])]) < 5)] <- "<5"

# Males:
demTab[,3][as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3])
           & !is.na(as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3]))] <-
  paste0("<5 ", sub("*.\\ ", "", demTab[,3][as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3])
                                            & !is.na(as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3]))]))

demTab[,3][-grep("[(]", demTab[,3])][which(as.numeric(demTab[,3][-grep("[(]", demTab[,3])]) < 5)] <- "<5"

Nf <- gsub(".*,\ ", "", colnames(demTab)[2])
Nm <- gsub(".*,\ ", "", colnames(demTab)[3])
colnames(demTab)[2:3] <- c("Female", "Male")
demTab <- rbind(data.frame(Variable = "", Female = Nf, Male = Nm), 
                demTab,
                data.frame(Variable = "n (%); Mean (SD)", Female = "", Male = "")) %>%
  mutate_if(is.character, ~replace(., is.na(.), ""))
demTab
write.csv(demTab, "Output/demographicTableDNAm.csv", row.names = F)

file <- paste0("Output/demographicTableDNAm.xlsx")
system(paste0("rm ", file))
write.xlsx(demTab, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = TRUE)

wb <- loadWorkbook(file)
sheets <- getSheets(wb)
autoSizeColumn(sheets[[1]], colIndex = 1:ncol(demTab))
saveWorkbook(wb, file)

# ----------------------------------------
#### PRSs ################################
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# ----------------------------------------------------------------------------
# Load in PRSs
PRSs <- list(read.table("../PRS/Output/CRP_SBayesR.all_score", header = T),
             read.table("../PRS/Output/IL6_SBayesR.all_score", header = T))

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

# ----------------------------------------------------------------------------
# Merge PRSs into data
data <- merge(data, PRSs, by = "Subject")
head(data)
nrow(data) # 8,804


# make dataset with a few variables to summarize
dataSub <- data %>% 
  filter(!is.na(IL6_SBayesR) | !is.na(CRP_SBayesR)) %>%
  select(Sex, 
         `Maternal Education` = Maternal.education.at.birth, 
         `CRP PRS` = CRP_SBayesR,
         `IL-6 PRS` = IL6_SBayesR,
         `Total Depressive Episodes` = dep_episodes,
         `Total PEs` = PLE_total
  ) %>%
  mutate_at(c("Sex", "Maternal Education",  "Total Depressive Episodes", "Total PEs"), as.factor) %>%
  mutate_at(c("Maternal Education", "Total Depressive Episodes", "Total PEs", "Sex"), function(x) fct_explicit_na(x, na_level = "Missing"))

dataSub$`Maternal Education` <- recode_factor(dataSub$`Maternal Education`, `0` = "CSE/O-level/Vocational", `1` = "A-level/Degree")

dataSub$Sex <- recode_factor(dataSub$Sex, `1` = "Female", `0` = "Male")

demographic <- tbl_summary(dataSub, by = Sex, statistic = list(all_continuous() ~ "{mean} ({sd})"), missing_text = "Missing") %>%
  modify_header(label = "Variable")

demographic

demographic %>%
  as_gt() %>%
  gt::gtsave(filename = "Output/demographicTablePRS.png")

# Save as csv:
demTab <- demographic %>% as_tibble() %>% as.data.frame()

demTab
# Now for the most horrendous code ever to change cells with values <5 to paste "< 5"
# Females:
demTab[,2][as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2])
           & !is.na(as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2]))] <-
  paste0("<5 ", sub("*.\\ ", "", demTab[,2][as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2])
                                            & !is.na(as.numeric(sub("\\(.*", "", demTab[,2])) < 5 & grepl("[%]", demTab[,2]))]))

demTab[,2][-grep("[(]", demTab[,2])][which(as.numeric(demTab[,2][-grep("[(]", demTab[,2])]) < 5)] <- "<5"

# Males:
demTab[,3][as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3])
           & !is.na(as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3]))] <-
  paste0("<5 ", sub("*.\\ ", "", demTab[,3][as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3])
                                            & !is.na(as.numeric(sub("\\(.*", "", demTab[,3])) < 5 & grepl("[%]", demTab[,3]))]))

demTab[,3][-grep("[(]", demTab[,3])][which(as.numeric(demTab[,3][-grep("[(]", demTab[,3])]) < 5)] <- "<5"


Nf <- gsub(".*,\ ", "", colnames(demTab)[2])
Nm <- gsub(".*,\ ", "", colnames(demTab)[3])
Nmiss <- gsub(".*,\ ", "", colnames(demTab)[4])
colnames(demTab)[2:4] <- c("Female", "Male", "Missing")
demTab <- rbind(data.frame(Variable = "", Female = Nf, Male = Nm, `Missing` = Nmiss), 
                demTab,
                data.frame(Variable = "n (%); Mean (SD)", Female = "", Male = "", `Missing` = "")) %>%
  mutate_if(is.character, ~replace(., is.na(.), ""))
demTab

write.csv(demTab, "Output/demographicTablePRS.csv", row.names = F)

file <- paste0("Output/demographicTablePRS.xlsx")
system(paste0("rm ", file))
write.xlsx(demTab, file, sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = TRUE)

wb <- loadWorkbook(file)
sheets <- getSheets(wb)
autoSizeColumn(sheets[[1]], colIndex = 1:ncol(demTab))
saveWorkbook(wb, file)
# ----------------------------------------------------------------------------
# Mean age and SD when inflammatory bloods measured

# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# make dataset with a few variables to summarize
dataSub <- data %>% 
  select(Sex, 
         `Maternal Education` = Maternal.education.at.birth, 
         `BMI (age 9 years)` = BMI_age9,
         `CRP (age 9 years)` = CRP..age.9., 
         `IL-6 (age 9 years)` = IL.6..age.9.,
         age_F09
  ) %>%
  mutate_at(c("Sex", "Maternal Education"), as.factor) %>%
  drop_na()

mean(dataSub$age_F09)
sd(dataSub$age_F09)

# ----------------------------------------------------------------------------
# Mean age and SD when DNAm measured

# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# Merge DNAm scores at age 0, 7, 15, 17 (fill in NA for empty rows)
DNAm <- list(read.csv("../DNAm/Output/DNAm_scores_age_0.csv", header = T),
             read.csv("../DNAm/Output/DNAm_scores_age_7.csv", header = T))
# Merge these files into one data frame by the common column "Subject"
DNAm <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Subject"), DNAm)
nrow(DNAm) # 1017
colnames(DNAm) <- c("Subject","CRP_scores_0", "IL6_scores_0", "IL6_Gadd_scores_0",
                    "CRP_scores_7", "IL6_scores_7", "IL6_Gadd_scores_7")
any(!data$Subject %in% DNAm$Subject)
data <- merge(data, DNAm, by = "Subject")

# make dataset with a few variables to summarize
dataSub <- data %>% 
  select(Sex, 
         `Maternal Education` = Maternal.education.at.birth,
         `BMI (age 7 years)` = BMI_age7,
         `CRP DNAm score (at birth)` = CRP_scores_0,
         `CRP DNAm score (age 7 years)` = CRP_scores_7,
         `IL-6 DNAm score (at birth)` = IL6_scores_0,
         `IL-6 DNAm score (age 7 years)` = IL6_scores_7,
         `Total Depressive Episodes` = dep_episodes,
         `Total PEs` = PLE_total,
         age_F07) %>%
  drop_na()
         
mean(dataSub$age_F07)
sd(dataSub$age_F07)

DNAmCols <- dplyr::select(DNAm, -1)
DNAmCols[rowSums(is.na(DNAmCols)) != ncol(DNAmCols), ] %>% nrow()
nrow(DNAm)
nrow(data)
