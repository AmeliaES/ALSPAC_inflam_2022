#  Clean ALSPAC data set 
# ----------------------------------------

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/Data_prep/")

# Read in libraries and clear working envrironment
source("Scripts/startup.R")

# ----------------------------------------
# Read in stata format data (.dta file)
# This takes a wee while... and need to be connected to VPN and Datastore server.
data <- read_dta("/Volumes/ALSPAC/data/B3421_Whalley_04Nov2021.dta")
bkup <- data

## Some useful haven functions:
# Get the labels for the col names
vars <- var_label(data)
# Get labels of values for each variable
val_labels(data)

vars[str_detect(names(vars), "f9sa010")]
vars[str_detect(vars, "Infection")]

bkup$f9sa011
bkup$f9sa010
# ----------------------------------------
# Create subject variable
data <- data %>%
        unite("Subject", c(cidB3421, qlet))

# ----------------------------------------
# Calculate depression scores and total number of depressive episodes
source("Scripts/dep_scores.R")

# ----------------------------------------
# Calculate total number of PLEs
source("Scripts/PLE.R")

# ----------------------------------------
# Subset data frame to variables we need to make it quicker to handle

dataVars <- c("Subject" = "Subject", # Unique pregnancy identifier for Heather Whalley(ACGDEHFB)
              "Sex" = "kz021", # Participant sex
              "Maternal education at birth" = "c645", # Mums highest ed qualification
              "Maternal social class at birth" = "c755", # Social Class - Maternal
              "Ethnicity" = "c804", # Child ethnic background
              "Ethnic_Group" = "c800", # child ethnic group
              "CRP (age 9)" = "CRP_f9", # C-Reactive protein mg/l, Focus@9
              "CRP (age 15)" = "crp_TF3", # C-reactive Protein mg/l, TF3, 15 years
              "CRP (age 17)" = "CRP_TF4", # C-reactive Protein mg/l, TF4, 17 years
              "CRP (age 24)" = "CRP_F24", # C-Reactive Protein mg/L, Focus@24
              "IL-6 (age 9)" = "IL6_f9", # Interleukin 6 pg/ml, Focus@9
              "Infection_present" = "f9sa010",
              "Infection_days_since" = "f9sa011",
               "Neutrophils_F24",
               "Eosinophils_F24",
               "Basophils_F24",
               "Lymphocytes_F24",
               "Monocytes_F24",
              # MFQ scores:
               "mfq_t01", "mfq_t02", "mfq_t03", "mfq_t04", "mfq_t05",
               "mfq_t06", "mfq_t07", "mfq_t08", "mfq_t09", "mfq_t10","mfq_t11",
              # age at appointments for 7 and 9 years:
               "age_F07" = "f7003c",
               "age_F09" = "f9003c",
              # age at attendance (months):                          Appointment Time Point
              "age_t01" = "fd003c", # depression only                  1
              "PLE_11" = "cck991a", # age 11                           2
              "age_t02" = "ff0011a",# PLE and depression (age 12)      3     PLE Clinic
              "PLE_13" = "ccn991a", # age 13                           4
              "age_t03" = "fg0011a",# depression only                  5
              "PLE_14" = "ccr991a", # age 14                           6
              "age_t04"= "ccs9991a",# depression and PLE (age 16)      7
              "age_t05"= "CCXD006",# depression and PLE (age 17/18)    8     PLE Clinic
              "age_t06" = "cct9991a",# depression only                 9
              "age_t07" = "YPA9020",# depression and PLE (age 21)      10
              "age_t08" = "YPB9992",# depression only                  11
              "age_t09" = "YPC2650",# depression only                  12
              "PLE_24" = "FKAR0010", # age 24                          13    PLE Clinic
              "age_t10" = "YPE9660",# depression only                  14
              "PLE_26" = "YPF9520", # age 26                           15
              "age_t11" = "covid4yp_9650", # age in years              16
              "PLE_18" = "FJ003a", # age 18 (F17)
              # Other psychotic like phenotypes:              "PSYCH_12_definite" = "ff5262",
               "PSYCH_24_definite" = "FKPL2010", 
              "PSYCH_24_disorder" = "FKPL2240",
                "PSYCH_18_definite" = "FJPL162",
              # BMI related variables:
               "BMI_age9" = "f9ms026a", # BMI: F9
               "weight_kg_age7" = "f7ms026", # weight age 7 years
               "height_cm_age7" = "f7ms010", # height age 7 years
               "height_cm_age15" = "fh3000", # height TF3
               "weight_kg_age15" = "fh3010",# weight TF3
               "BMI_age17" = "FJMR022a", # BMI F17
              # TRUE or FALSE to whether a PLE occurred:
               "ple_t01", "ple_t02", "ple_t03", "ple_t04", "ple_t05", "ple_t06", 
               "ple_t07", "ple_t08", "ple_t09",
              # Total number of PLE and depressive episode and number of appts attended:
               "PLE_total","PLE_appts",
               "dep_episodes", "dep_appts",
              "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10"
              
           )

# Get auxillary variable names:
auxVars <- c("SDQ_emotional_7" = "kq348c", # continuous score
             "SDQ_emotional_9" = "ku707b", # continuous score
             "SDQ_emotional_11" = "kw6602b", # continuous score
             "SDQ_emotional_13" = "ta7025a", # continuous score
             "DV_anxiety_dis_13" = "tb8618", # coded as binary
             "DV_depressive_dis_13" = "tb8619", # coded as binary
             "DV_any_anxiety_7" = "kr827a", # coded as binary
             "DV_MDD_7" = "kr830", # coded as binary
             "DV_anxiety_dis_10" = "kv8617",  # coded as binary
             "ever_depression_diagnosis_22" = "YPB1233", # coded as binary
             "WEMWBS_23" = "YPC0600", # coded as binary
             "dep_bin_17" = "FJCI1001", #  coded as binary
             "mild_dep_dis_24" = "FKDQ1000", # coded as binary
             "IMD_score" = "jan2014imd2010q5_YP", # continuous score
             "weight_kg_12" = "ff2030", # continuous score
             "height_cm_12" = "ff2000", # continuous score
             "maternal_EDPS" = "e391", # code as binary
             "paternal_EDPS" = "pe290" # code as binary
             )

dataVars <- c(dataVars,  auxVars)

# ----------------------------------------
# Subset data
dataSub <- dataSmfq %>% 
            dplyr::select(all_of(dataVars))

# ----------------------------------------
# Re-code values of variables (this method looses value labels (also indicated in warning message) so I've made a note of the new labels in comments)
# ----------
# Participant sex : female = 1, male = 0
dataSubQC <- dataSub %>%
  mutate(Infection_present = dplyr::recode(Infection_present, `1` = "1", `2` = "2" )) %>%
  mutate(`Sex` = dplyr::recode(`Sex`,
                                    `2`="1",
                                    `1`="0")) %>%
# Binary way of coding Maternal education: 0 = CSE/Olevel/Vocational 1=Alevel/Degree
  mutate(`Maternal education at birth` = dplyr::recode(`Maternal education at birth`,
                                                `5`="1",
                                                `4`="1",
                                                `3`="0",
                                                `2`="0",
                                                `1`="0")) %>%
# Maternal social class at birth: 0 = I, II; 1 = III (non-manual), III (manual), IV, V. 
  mutate(`Maternal social class at birth` = dplyr::recode(`Maternal social class at birth`,
                                                   `6`="1",
                                                   `5`="1",
                                                   `4`="1",
                                                   `3`="1",
                                                   `2`="0",
                                                   `1`="0")) %>%
# Ethnicity: (0 = "white" 1 = "non-white")
  mutate(`Ethnicity` = dplyr::recode(`Ethnicity`,
                              `1`="0",
                              `2`="1")) %>%
# CRP
# Change negative values which indicate missing to NA
  mutate(`CRP (age 9)` = replace(`CRP (age 9)`, which(`CRP (age 9)` < 0), NA)) %>%
# IL-6
# Change negative values which indicate missing to NA
  mutate(`IL-6 (age 9)` = replace(`IL-6 (age 9)`, which(`IL-6 (age 9)` < 0), NA)) %>%
# Remove negative codings from age variables
  mutate_at(vars(c("age_t01", "age_t02", "age_t03", "age_t04", "age_t05",
           "age_t06", "age_t07", "age_t08", "age_t09", "age_t10", "age_t11", "age_F07",
           "PLE_11", # age 11
           "PLE_13", # age 13
           "PLE_14", # age 14
           "PLE_18", # age 18 (F17)
           "PLE_24", # age 24
           "PLE_26")), # age 26)),
           funs(replace(., which(. < 0), NA))) %>%
# Convert age from months to years
  mutate_at(vars(c("age_t01", "age_t02", "age_t03", "age_t04", "age_t05",
                   "age_t06", "age_t07", "age_t08", "age_t09", "age_t10", "age_F07","age_F09",
                 "PLE_11", # age 11
                 "PLE_13", # age 13
                 "PLE_14", # age 14
                 "PLE_18", # age 18 (F17)
                 "PLE_24", # age 24
                 "PLE_26")), # age 26
            funs(./12)) %>%
  mutate("age_t11" = as.numeric(age_t11)) %>%
# Code negative values as NA for weight and height of age 7 years & calculate BMI
  mutate(BMI_age9 = replace(BMI_age9, which(BMI_age9 < 0), NA)) %>%
  mutate(`height_cm_age7` = replace(`height_cm_age7`, which(`height_cm_age7` < 0), NA)) %>%
  mutate(`weight_kg_age7` = replace(`weight_kg_age7`, which(`weight_kg_age7` < 0), NA)) %>%
  mutate(BMI_age7 = weight_kg_age7/((height_cm_age7)/100)^2 ) %>%
  mutate(`height_cm_age15` = replace(`height_cm_age15`, which(`height_cm_age15` < 0), NA)) %>%
  mutate(`weight_kg_age15` = replace(`weight_kg_age15`, which(`weight_kg_age15` < 0), NA)) %>%
  mutate(BMI_age15 = weight_kg_age15/((height_cm_age15)/100)^2 ) %>%
  mutate(`BMI_age17` = replace(`BMI_age17`, which(`BMI_age17` < 0), NA)) %>%
# Code negative values as NA for CRP and WBC counts
  mutate(`CRP (age 9)` = replace(`CRP (age 9)`, which(`CRP (age 9)` < 0), NA)) %>%
  mutate(`CRP (age 15)` = replace(`CRP (age 15)`, which(`CRP (age 15)` < 0), NA)) %>%
  mutate(`CRP (age 17)` = replace(`CRP (age 17)`, which(`CRP (age 17)` < 0), NA)) %>%
  mutate(`CRP (age 24)` = replace(`CRP (age 24)`, which(`CRP (age 24)` < 0), NA)) %>%
  mutate(`Neutrophils_F24` = replace(`Neutrophils_F24`, which(`Neutrophils_F24` < 0), NA)) %>%
  mutate(`Eosinophils_F24` = replace(`Eosinophils_F24`, which(`Eosinophils_F24` < 0), NA)) %>%
  mutate(`Basophils_F24` = replace(`Basophils_F24`, which(`Basophils_F24` < 0), NA)) %>%
  mutate(`Lymphocytes_F24` = replace(`Lymphocytes_F24`, which(`Lymphocytes_F24` < 0), NA)) %>%
  mutate(`Monocytes_F24` = replace(`Monocytes_F24`, which(`Monocytes_F24` < 0), NA)) %>%
  # Other psychotic related phenotypes:
  mutate(`PSYCH_12_definite` = replace(`PSYCH_12_definite`, which(`PSYCH_12_definite` < 0), NA)) %>%
  mutate(`PSYCH_18_definite` = replace(`PSYCH_18_definite`, which(`PSYCH_18_definite` < 0), NA)) %>%
  mutate(`PSYCH_18_definite` = dplyr::recode(`PSYCH_18_definite`, `1`= "0", `2`="1")) %>%
  mutate(`PSYCH_24_definite` = replace(`PSYCH_24_definite`, which(`PSYCH_24_definite` < 0), NA)) %>%
  mutate(`PSYCH_24_disorder` = replace(`PSYCH_24_disorder`, which(`PSYCH_24_disorder` < 0), NA)) %>%
  # Other auxiliary variables:
  mutate(`SDQ_emotional_7` = replace(`SDQ_emotional_7`, which(`SDQ_emotional_7` < 0), NA)) %>%
  mutate(`SDQ_emotional_9` = replace(`SDQ_emotional_9`, which(`SDQ_emotional_9` < 0), NA)) %>%
  mutate(`SDQ_emotional_11` = replace(`SDQ_emotional_11`, which(`SDQ_emotional_11` < 0), NA)) %>%
  mutate(`SDQ_emotional_13` = replace(`SDQ_emotional_13`, which(`SDQ_emotional_13` < 0), NA)) %>%
  mutate(`DV_anxiety_dis_13` = replace(`DV_anxiety_dis_13`, which(`DV_anxiety_dis_13` < 0), NA)) %>%
  mutate(`DV_depressive_dis_13` = replace(`DV_depressive_dis_13`, which(`DV_depressive_dis_13` < 0), NA)) %>%
  mutate(`DV_any_anxiety_7` = replace(`DV_any_anxiety_7`, which(`DV_any_anxiety_7` < 0), NA)) %>%
  mutate(`DV_MDD_7` = replace(`DV_MDD_7`, which(`DV_MDD_7` < 0), NA)) %>%
  mutate(`DV_anxiety_dis_10` = replace(`DV_anxiety_dis_10`, which(`DV_anxiety_dis_10` < 0), NA)) %>%
  mutate(`ever_depression_diagnosis_22` = dplyr::recode(`ever_depression_diagnosis_22`,
                                                                        `3`="0",
                                                                        `4`="0",
                                                                        `1`="1",
                                                                        `2`="1")) %>%
  mutate(`WEMWBS_23` = replace(`WEMWBS_23`, which(`WEMWBS_23` < 0), NA)) %>%
  mutate(`WEMWBS_23` = replace(`WEMWBS_23`, which(`WEMWBS_23` <= 40 & `WEMWBS_23` > 0), 0)) %>%
  mutate(`WEMWBS_23` = replace(`WEMWBS_23`, which(`WEMWBS_23` >= 40), 1)) %>%
  mutate(`WEMWBS_23` = dplyr::recode(`WEMWBS_23`,`0`="0", `1`="1")) %>%
  mutate(`dep_bin_17` = replace(`dep_bin_17`, which(`dep_bin_17` < 0), NA)) %>%
  mutate(`mild_dep_dis_24` = replace(`mild_dep_dis_24`, which(`mild_dep_dis_24` < 0), NA)) %>%
  mutate(`IMD_score` = replace(`IMD_score`, which(`IMD_score` < 0), NA)) %>%
  mutate(`weight_kg_12` = replace(`weight_kg_12`, which(`weight_kg_12` < 0), NA)) %>%
  mutate(`height_cm_12` = replace(`height_cm_12`, which(`height_cm_12` < 0), NA)) %>%
  mutate(BMI_age12 = weight_kg_12/((height_cm_12)/100)^2 ) %>%
  mutate(`maternal_EDPS` = replace(`maternal_EDPS`, which(`maternal_EDPS` >= 0 & `maternal_EDPS` <= 12), 0)) %>%
  mutate(`maternal_EDPS` = replace(`maternal_EDPS`, which(`maternal_EDPS` >= 13), 1)) %>%
  mutate(`maternal_EDPS` = dplyr::recode(`maternal_EDPS`, `0`="0", `1`="1")) %>%
  mutate(`paternal_EDPS` = replace(`paternal_EDPS`, which(`paternal_EDPS` >= 0 & `paternal_EDPS` <= 12), 0)) %>%
  mutate(`paternal_EDPS` = replace(`paternal_EDPS`, which(`paternal_EDPS` >= 13), 1)) %>%
  mutate(`paternal_EDPS` = dplyr::recode(`paternal_EDPS`, `0`="0", `1`="1"))
  
# Check ages look sensible:
sapply(c("age_t01", "age_t02", "age_t03", "age_t04", "age_t05",
         "age_t06", "age_t07", "age_t08", "age_t09", "age_t10", "age_t11"), function(age) sum(dataSubQC[,age] < 0, na.rm = T))
min(dataSubQC$age_t01, na.rm = T)
max(dataSubQC$age_t01, na.rm = T)
min(dataSubQC$age_t10, na.rm = T)
max(dataSubQC$age_t11, na.rm = T)

# Check there are no negative values which should be coded as missing (ie. NA)
dataSubQC %>%
  filter_all(any_vars(. < 0)) # 0 rows :-)
# ----------------------------------------
# Convert table from wide format to long format by age and depression score
dataSubQCLong <- gather(dataSubQC, age_cat, age, age_t01:age_t11, factor_key=TRUE)
data_dep <- gather(dataSubQC, dep_cat, dep, mfq_t01:mfq_t11, factor_key=TRUE)
dataSubQCLong$dep_cat <- data_dep$dep_cat
dataSubQCLong$dep <- data_dep$dep
 
dataSubQCLong <- subset(dataSubQCLong, select = -c(mfq_t01:mfq_t11)) #remove redundant columns
dataSubQCLong <- dataSubQCLong[order(dataSubQCLong$Subject, dataSubQCLong$age_cat),]
head(dataSubQCLong)
# ----------------------------------------
# Save cleaned data frame 
write.csv(dataSubQCLong, "/Volumes/ALSPAC/users/amelia/Data/dataSubQCLong.csv", row.names = FALSE)
write.csv(dataSubQC, "/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv", row.names = FALSE)

# ----------------------------------------
# Create descriptive statistic tables for depression scores and PLEs
# Plot figures for ages and stacked bar plots of TRUE/FALSE for PLE or depressive episodes
source("Scripts/2_Explore_data.R")

# ----------------------------------------
# Create demographic tables
source("Scripts/demographics.R")


