# check number of ppl that have had at least one depressive episode or pe:

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/Data_prep/")

# Read in librarys and clear working envrironment
source("Scripts/startup.R")

# ----------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

data %>%
  filter(!is.na(CRP..age.9.)) %>%
  filter(!is.na(IL.6..age.9.)) %>%
  filter(dep_episodes > 0) %>%
  nrow()

data %>%
  filter(!is.na(CRP..age.9.)) %>%
  filter(!is.na(IL.6..age.9.)) %>%
  filter(dep_episodes == 0) %>%
  nrow()

1980/(1980+2919)*100 # 40%

# Split by sex

# female
data %>%
  filter(!is.na(CRP..age.9.)) %>%
  filter(!is.na(IL.6..age.9.)) %>%
  filter(Sex == 1) %>%
  filter(dep_episodes > 0) %>%
  nrow()

data %>%
  filter(!is.na(CRP..age.9.)) %>%
  filter(!is.na(IL.6..age.9.)) %>%
  filter(Sex == 1) %>%
  filter(dep_episodes == 0) %>%
  nrow()

# male
data %>%
  filter(!is.na(CRP..age.9.)) %>%
  filter(!is.na(IL.6..age.9.)) %>%
  filter(Sex == 0) %>%
  filter(dep_episodes > 0) %>%
  nrow()

data %>%
  filter(!is.na(CRP..age.9.)) %>%
  filter(!is.na(IL.6..age.9.)) %>%
  filter(Sex == 0) %>%
  filter(dep_episodes == 0) %>%
  nrow()

# ------------------------------
# check i summed scores correctly
sub <- data %>%
  slice(1:30) %>%
  select(c(starts_with("mfq"),dep_episodes))

sub


# ------------------------------
dataSub <- data %>% 
  select(Sex, 
         `Maternal Education` = Maternal.education.at.birth, 
         `BMI (age 9 years)` = BMI_age9,
         `CRP (age 9 years)` = CRP..age.9., 
         `IL-6 (age 9 years)` = IL.6..age.9.,
         `Total Depressive Episodes` = dep_episodes,
         `Total PEs` = PLE_total
  ) %>%
  filter(!is.na(`CRP (age 9 years)`)) %>%
  filter(!is.na(`IL-6 (age 9 years)`)) %>%
  mutate_at(c("Sex", "Maternal Education", "Total Depressive Episodes", "Total PEs"), as.factor) %>%
  mutate_at(c("Maternal Education", "Total Depressive Episodes", "Total PEs"), function(x) fct_explicit_na(x, na_level = "Missing")) 

dataSub$`Maternal Education` <- recode_factor(dataSub$`Maternal Education`, `0` = "CSE/O-level/Vocational", `1` = "A-level/Degree")

dataSub$Sex <- recode_factor(dataSub$Sex, `1` = "Female", `0` = "Male")



# Remove people with CRP >= 10 mg/L, write in methods section the N for this, = 60.
sum(dataSub$`CRP (age 9 years)` >= 10)

dataSub <- dataSub[-which(dataSub$`CRP (age 9 years)` >= 10),]

demographic <- tbl_summary(dataSub, by = Sex, statistic = list(all_continuous() ~ "{mean} ({sd})"), missing_text = "Missing") 


tbl_summary(data %>%
              select(dep_episodes) %>%
              mutate(dep_episodes, as.factor(dep_episodes)),  statistic = list(all_continuous() ~ "{mean} ({sd})"))



