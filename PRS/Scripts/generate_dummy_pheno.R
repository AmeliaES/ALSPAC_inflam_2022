# Create binary pheno file where 1 indicates precence of disorder
# First two columns should be FID and IID, subsequent columns are pheno1, pheno2 etc..
# each pheno column generates a separate PRS output file in PRSice-2
# third col a column of 0s and 1s as a no_pheno column.
# pheno columns containing only 0s or only 1s are rejected by PRSice-2
# column names do not matter

setwd("/exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS")

# read in .fam file of QC'd target data to get list of IDs
fam <- read.table("ALSPAC/1000G/data_QC.fam")

pheno <- data.frame("FID" = fam[,1], "IID" = fam[,1], "no_pheno" = 0)

# Add random 0 and 1s to no_pheno
pheno$no_pheno <- sample(c(0,1), replace = T, size = nrow(pheno))


# save table in your scratch dir
write.table(pheno, "ALSPAC/no_pheno.txt", row.names = F, quote = F)



