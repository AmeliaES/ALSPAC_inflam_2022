# Start up script

#setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_Study/ALSPAC_Inflam/")

library(car)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(haven)
library(labelled)
library(lme4)
# library(lmtest)
library(mosaic)
library(nlme)
library(purrr)
library(RNOmni)
library(stringr)
library(stringi)
library(tableone)
library(tibble)
library(tidyr)

rm(list=setdiff(ls(), c("data", "bkup")))

