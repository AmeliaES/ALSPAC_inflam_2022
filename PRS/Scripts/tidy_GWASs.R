# Script to read in MDD, SCZ and PLE GWASs
# format to SBayesR cols
# remove MAF < 0.01, remove INFO < 0.8, remove duplicate SNPs (check numbers of these look sensible)
# ----------------------------------------------------------------
setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS")

library(dplyr)
library(stringr)

# ----------------------------------------------------------------
MDD <- read.table("GWASs/MDD/PGC_UKB_depression_genome-wide.txt", header = T)

files <- c("GWASs/PLE_UKB/ukbiobank-any_psychotic_experience.sumstats.txt",
"GWASs/PLE_UKB/ukbiobank-distressing_psychotic_experience.sumstats.txt",
"GWASs/PLE_UKB/ukbiobank-multiple_psychotic_experience.sumstats.txt")

PLEs <- lapply(files, function(file){
	read.table(file, header = T)
})


SCZ <- read.table("GWASs/SCZ/daner_PGC_SCZ52", header = T)

SCZ_w3 <- read.table("GWASs/SCZ_w3/PGC3_SCZ_wave3_public.v2.tsv", header = T, fill = TRUE )

# ----------------------------------------------------------------
# MDD
# Remove MAF < 0.01
# No INFO col
# Add a col for N = 500199 (from README found via PGC download site)
# Check for duplicates

MDD_QC <- MDD %>%
			filter(Freq > 0.01) %>%
			select(SNP = MarkerName,
				   A1,
				   A2,
				   freq = Freq,
				   b = LogOR,
				   se = StdErrLogOR,
				   p = P) %>%
			mutate(N = 500199) %>%
			mutate(A1 = toupper(A1), 
				   A2 = toupper(A2))

head(MDD_QC)

sum(duplicated(MDD_QC$SNP)) # no duplicated SNPs
# ----------------------------------------------------------------
# SCZ

SCZ_QC <- SCZ %>%
			filter(FRQ_A_35476 > 0.01) %>%
			filter(INFO > 0.8) %>%
			mutate(log_OR = log(OR)) %>%
			select(SNP,
				   A1,
				   A2,
				   freq = FRQ_A_35476,
				   b = log_OR,
				   se = SE,
				   p = P) %>%
			mutate(N = 35476 + 46839) %>%
			filter(duplicated(SNP) == FALSE) 

head(SCZ_QC)


SCZ_w3_QC <- SCZ_w3 %>%
			filter(FRQ_A_67390 > 0.01) %>%
			filter(INFO > 0.8) %>%
			mutate(log_OR = log(OR)) %>%
			select(SNP,
				   A1,
				   A2,
				   freq = FRQ_A_67390,
				   b = log_OR,
				   se = SE,
				   p = P) %>%
			mutate(N = 67390 + 94015) %>%
			filter(duplicated(SNP) == FALSE) 

head(SCZ_w3_QC)
summary(SCZ_w3_QC)

# ----------------------------------------------------------------
# PLEs
# Remove MAF < 0.01
# Remove INFO < 0.8
# Add a col for N = 127966 (from README - https://walters.psycm.cf.ac.uk/)
# Check for duplicates and NAs

# Check for NAs:
lapply(PLEs, function(x) sum(is.na(pull(x, P))))

# Check for dups:
lapply(PLEs, function(x) sum(duplicated(select(x, SNP))))


PLEs_QC <- lapply(PLEs, function(PLE){
			PLE %>%
			filter(FRQ_A > 0.01) %>%
			filter(INFO > 0.8) %>%
			mutate(log_OR = log(OR)) %>%
			select(SNP,
				   A1,
				   A2,
				   freq = FRQ_A,
				   b = log_OR,
				   se = SE,
				   p = P) %>%
			mutate(N = 127966) %>%
			filter(duplicated(SNP) == FALSE) %>%
			filter(!is.na(p)) 
		})

lapply(PLEs_QC, head)
lapply(PLEs, nrow)
lapply(PLEs_QC, nrow)

# ----------------------------------------------------------------
# See how many SNPs there are at the GWS threshold
sapply(PLEs_QC, function(x) sum(pull(x, p) < 5e-08) )

lapply(PLEs_QC, function(x) filter(x, p < 5e-08))


# ----------------------------------------------------------------
# Save GWASs
write.table(MDD_QC, "GWASs/MDD/PGC_UKB_depression_genome-wide.txt.QC", row.names = F, quote = FALSE)

write.table(SCZ_QC, "GWASs/SCZ/daner_PGC_SCZ52.QC", row.names = F, quote = FALSE)

write.table(SCZ_w3_QC, "GWASs/SCZ_w3/PGC3_SCZ_wave3_public.v2.tsv.QC", row.names = F, quote = FALSE)

lapply(1:3, function(i)
	write.table(PLEs_QC[[i]], paste0(files[i], ".QC"), row.names = F, quote = FALSE)
)





