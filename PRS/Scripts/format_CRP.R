# Reformat sum stats for SBayesR
setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS")

library(dplyr)
library(stringr)

gwas <- read.table("GWASs/CRP_ukb_sumstats/30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv", header = T)
vars <- read.table("GWASs/CRP_ukb_sumstats/variants.tsv", header = T)

sumstats <- merge(gwas, vars, by = "variant")

bkup <- sumstats

nrow(gwas)
nrow(vars)
nrow(sumstats)

sumstats <- sumstats %>%
	filter(minor_AF.x > 0.01 & info > 0.8) %>%
	select(SNP = rsid,
		   A1 = alt,
		   A2 = ref,
		   freq = minor_AF.x,
		   b = beta,
		   se = se,
		   p = pval,
		   N = n_complete_samples,
		   CHR = chr,
		   BP = pos) %>%
	filter(duplicated(SNP) == FALSE) 


nrow(sumstats) #9,663,948

head(sumstats)


write.table(sumstats, "GWASs/CRP_ukb_sumstats/30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.QC", quote = F, row.names = F, col.names = T)

