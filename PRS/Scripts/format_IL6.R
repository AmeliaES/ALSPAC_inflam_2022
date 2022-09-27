# Format IL-6 GWAS sumstats for SBayesR
setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS/")

library(dplyr)
library(stringr)

# Read in sumstats from Ahola-Olli et al.
sumstats <- read.table("GWASs/IL6_Ahola_Olli/meta_filt_IL_6_noBMI.txt", header = T)
head(sumstats)

# Check for duplicate SNPs
length( unique(sumstats$MarkerName) ) == nrow(sumstats)

# How many SNPs in sumstats
nsnps <- nrow(sumstats) ;nsnps # = 10,040,617

# make a backup before we overwrite the sumstats variable
bkup <- sumstats

# Change A1 and A2 cols to capital letters:
sumstats <- sumstats 

# Get Chr and BP info?

# Check format and col names:
# SNP A1 A2 freq b se p N 
sumstats <- sumstats %>%
			mutate(Allele1 = toupper(Allele1), 
				   Allele2 = toupper(Allele2)) %>%
			select(
			SNP = MarkerName,
			A1 = Allele1,
			A2 = Allele2,
			freq = Freq1 ,
			b = Effect,
			se = StdErr,
			p = P.value,
			N = Total_N) %>%
			filter(freq > 0.01) %>% # Remove SNPs with MAF < 0.01
			filter(duplicated(SNP) == FALSE) 

head(sumstats)
nrow(sumstats) # 9320307, 720310 SNPs removed 


write.table(sumstats, "GWASs/IL6_Ahola_Olli/meta_filt_IL_6_noBMI.txt.QC" , quote = F, row.names = F, col.names = T)

