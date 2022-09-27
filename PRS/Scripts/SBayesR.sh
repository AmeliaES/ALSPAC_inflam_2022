#! /bin/sh
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -l h_vmem=128G
#$ -pe sharedmem 2
#$ -l h_rt=48:00:00
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk

# Conduct SBayesR on GWAS sum stats

cd /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS

# Exclude MHC:
# This option will remove SNPs in the MHC regions (chr6:28-34 Mb) from the analysis.

/exports/igmm/eddie/GenScotDepression/amelia/packages/gctb_2.03beta_Linux/gctb --sbayes R \
--mldm /exports/eddie/scratch/s1211670/ukb_50k_bigset_2.8M/ukb50k_2.8M_shrunk_sparse.new.mldmlist \
--pi 0.95,0.02,0.02,0.01 \
--gamma 0.0,0.01,0.1,1 \
--ambiguous-snp \
--impute-n \
--gwas-summary $sumstats \
--chain-length 10000 \
--exclude-mhc \
--burn-in 2000 \
--out-freq 10 \
--out $output

