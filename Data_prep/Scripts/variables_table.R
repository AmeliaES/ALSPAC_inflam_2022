# Create table showing ALSPAC id, description and the variable name I use

# ----------------------------------------
setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/Data_prep/")

# Read in libraries and clear working envrironment
source("Scripts/startup.R")
library(magrittr)
# ----------------------------------------
# Read in stata format data (.dta file)
# This takes a wee while... and need to be connected to VPN and Datastore server.
data <- read_dta("/Volumes/ALSPAC/data/B3421_Whalley_04Nov2021.dta")
bkup <- data

# Get the labels for the col names
vars <- var_label(data)

# ----------------------------------------
# Variables for main analysis
dataVars <- c("cidB3421" ,"qlet", # Unique pregnancy identifier for Heather Whalley(ACGDEHFB)
              "Sex" = "kz021", # Participant sex
              "Maternal education at birth" = "c645", # Mums highest ed qualification
              "Maternal social class at birth" = "c755", # Social Class - Maternal
              "Ethnicity" = "c804", # Child ethnic background
              "CRP (age 9)" = "CRP_f9", # C-Reactive protein mg/l, Focus@9
              "CRP (age 15)" = "crp_TF3", # C-reactive Protein mg/l, TF3, 15 years
              "CRP (age 17)" = "CRP_TF4", # C-reactive Protein mg/l, TF4, 17 years
              "CRP (age 24)" = "CRP_F24", # C-Reactive Protein mg/L, Focus@24
              "IL-6 (age 9)" = "IL6_f9", # Interleukin 6 pg/ml, Focus@9
              "Neutrophils_F24",
              "Eosinophils_F24",
              "Basophils_F24",
              "Lymphocytes_F24",
              "Monocytes_F24",
              # age at appointments for 7 and 9 years:
              "age_F07" = "f7003c",
              "age_F09" = "f9003c",
              # age at attendance (months):
              "age_t01" = "fd003c", # depression only
              "age_t02" = "ff0011a",# PLE and depression (age 12)
              "age_t03" = "fg0011a",# depression only
              "age_t04"= "ccs9991a",# depression and PLE (age 16)
              "age_t05"= "CCXD006",# depression and PLE (age 17/18)
              "age_t06" = "cct9991a",# depression only
              "age_t07" = "YPA9020",# depression and PLE (age 21)
              "age_t08" = "YPB9992",# depression only
              "age_t09" = "YPC2650",# depression only
              "age_t10" = "YPE9660",# depression only
              "age_t11" = "covid4yp_9650", # age in years
              # ages for PLE only time points:
              "PLE_11" = "cck991a", # age 11
              "PLE_13" = "ccn991a", # age 13
              "PLE_14" = "ccr991a", # age 14
              "PLE_18" = "FJ003a", # age 18 (F17)
              "PLE_24" = "FKAR0010", # age 24
              "PLE_26" = "YPF9520", # age 26
              # Other psychotic like phenotypes:
              "PSYCH_24_definite" = "FKPL2010", 
              "PSYCH_24_disorder" = "FKPL2240",
              "PSYCH_18_definite" = "FJPL162",
              # BMI related variables:
              "BMI_age9" = "f9ms026a", # BMI: F9
              "weight_kg_age7" = "f7ms026", # weight age 7 years
              "height_cm_age7" = "f7ms010", # height age 7 years
              "height_cm_age15" = "fh3000", # height TF3
              "weight_kg_age15" = "fh3010",# weight TF3
              "BMI_age17" = "FJMR022a" # BMI F17
)

# ----------------------------------------
varsTab <- lapply(dataVars, function(x) vars[str_detect(names(vars), x)] ) %>%
  data.frame() %>%
  t() %>%
  as.data.frame() %>%
  mutate(ALSPAC_ID = rownames(.)) %>%
  set_rownames(NULL) %>%
  filter(ALSPAC_ID != "Maternal.education.at.birth.c645a") %>%
  mutate(Variable_Name = names(dataVars)) %>%
  mutate(ALSPAC_ID = dataVars) %>%
  select("ALSPAC_ID", "ALSPAC_description" = V1, "Variable_Name") %>%
  mutate(Variable_Name = ifelse(Variable_Name %in% "", ALSPAC_ID, Variable_Name))

head(varsTab)

# ----------------------------------------
# depression variables:
dep_vars <-  c("fddp110", "fddp112", "fddp113", "fddp114", "fddp115", "fddp116", "fddp118", "fddp119", "fddp121", "fddp122", "fddp123", "fddp124", "fddp125", "ff6500", "ff6502", "ff6503", "ff6504", "ff6505", "ff6506", "ff6508", "ff6509", "ff6511", "ff6512", "ff6513", "ff6514", "ff6515", "fg7210", "fg7212", "fg7213", "fg7214", "fg7215", "fg7216", "fg7218", "fg7219", "fg7221", "fg7222", "fg7223", "fg7224", "fg7225", "ccs4500", "ccs4502" ,"ccs4503", "ccs4504", "ccs4505", "ccs4506", "ccs4508", "ccs4509", "ccs4511", "ccs4512", "ccs4513", "ccs4514", "ccs4515", "CCXD900", "CCXD902" ,"CCXD903", "CCXD904", "CCXD905", "CCXD906" ,"CCXD908", "CCXD909", "CCXD911", "CCXD912", "CCXD913", "CCXD914", "CCXD915", "cct2700", "cct2701", "cct2702", "cct2703", "cct2704" ,"cct2705" ,"cct2706", "cct2707", "cct2708", "cct2709", "cct2710", "cct2711", "cct2712", "YPA2000", "YPA2010", "YPA2020", "YPA2030", "YPA2040", "YPA2050", "YPA2060", "YPA2070", "YPA2080", "YPA2090", "YPA2100", "YPA2110", "YPA2120", "YPB5000" ,"YPB5010" ,"YPB5030" ,"YPB5040" ,"YPB5050", "YPB5060", "YPB5080", "YPB5090", "YPB5100", "YPB5120", "YPB5130", "YPB5150" ,"YPB5170", "YPC1650", "YPC1651", "YPC1653" ,"YPC1654", "YPC1655", "YPC1656", "YPC1658", "YPC1659", "YPC1660", "YPC1662", "YPC1663", "YPC1665", "YPC1667", "YPE4080", "YPE4082", "YPE4083", "YPE4084", "YPE4085", "YPE4086", "YPE4088", "YPE4089", "YPE4091", "YPE4092", "YPE4093", "YPE4094", "YPE4095", "covid4yp_4050", "covid4yp_4051", "covid4yp_4052", "covid4yp_4053", "covid4yp_4054", "covid4yp_4055", "covid4yp_4056", "covid4yp_4057", "covid4yp_4058", "covid4yp_4059",  "covid4yp_4060", "covid4yp_4061", "covid4yp_4062" )


# Extract first 3 letters from each variable and give them a number from 1-11
depNum <- as.numeric( factor(substr(dep_vars, 1, 3), levels = unique(substr(dep_vars, 1, 3) ) ))

depVarsTab <- lapply(dep_vars, function(x) vars[str_detect(names(vars), x)] ) %>%
  data.frame() %>%
  t() %>%
  as.data.frame() %>%
  mutate(ALSPAC_ID = rownames(.)) %>%
  set_rownames(NULL) %>%
  mutate(Variable_Name = paste0("(Depression Time Point ", depNum, ")")) %>%
  select("ALSPAC_ID", "ALSPAC_description" = V1, "Variable_Name") %>%
  mutate(Variable_Name = ifelse(Variable_Name %in% "", ALSPAC_ID, Variable_Name))

depVarsTab
# ----------------------------------------
# PLE variables
pleVars <- c("cck360","cck362","cck420","cck422","cck340","cck342","ff5060","ff5062","ff5030","ff5032","ff5100","ff5102",
"ccn240","ccn242","ccn260","ccn262","ccn230","ccn232","ccr360","ccr363","ccr380","ccr383","ccr340","ccr343","ccs2560","ccs2563",
"ccs2600","ccs2603","ccs2540","ccs2543","FJPL027","FJPL030","FJPL044","FJPL047","FJPL056","FJPL059","YPA2130","YPA2131","YPA2140","YPA2141","YPA2150","YPA2151","FKPL1300","FKPL1303","FKPL1600","FKPL1603","FKPL1700","FKPL1703","YPF7010","YPF7030","YPF7040","YPF7060","YPF7070","YPF7090")


# Extract first 3 letters from each variable and give them a number from 1-11
pleNum <- as.numeric( factor(substr(pleVars, 1, 3), levels = unique(substr(pleVars, 1, 3) ) ))

pleVarsTab <- lapply(pleVars, function(x) vars[str_detect(names(vars), x)] ) %>%
  data.frame() %>%
  t() %>%
  as.data.frame() %>%
  mutate(ALSPAC_ID = rownames(.)) %>%
  set_rownames(NULL) %>%
  mutate(Variable_Name = paste0("(PLE Time Point ", pleNum, ")")) %>%
  select("ALSPAC_ID", "ALSPAC_description" = V1, "Variable_Name") %>%
  mutate(Variable_Name = ifelse(Variable_Name %in% "", ALSPAC_ID, Variable_Name))

pleVarsTab

# ----------------------------------------
# For imputed analysis:
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

auxVarsTab <- lapply(auxVars, function(x) vars[str_detect(names(vars), x)] ) %>%
  data.frame() %>%
  t() %>%
  as.data.frame() %>%
  mutate(ALSPAC_ID = rownames(.)) %>%
  set_rownames(NULL) %>%
  filter(ALSPAC_ID != "ever_depression_diagnosis_22.YPB1233_imputeno") %>%
  mutate(ALSPAC_ID = auxVars) %>%
  mutate(Variable_Name = names(auxVars) ) %>%
  select("ALSPAC_ID", "ALSPAC_description" = V1, "Variable_Name") %>%
  mutate(Variable_Name = ifelse(Variable_Name %in% "", ALSPAC_ID, Variable_Name))

auxVarsTab
varsTab
depVarsTab
pleVarsTab

df <- rbind(varsTab, depVarsTab, pleVarsTab, auxVarsTab)

write.csv(df, "Output/Variables_used.csv", row.names = F)



