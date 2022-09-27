#!/bin/sh
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk
#$ -l h_vmem=32G
#$ -pe sharedmem 8

cd /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS
    
. /etc/profile.d/modules.sh
module add igmm/apps/R/4.0.3

# -------------------------------------------------
# Create PRSs
# -------------------------------------------------
# From SBayesR output:
Rscript /exports/igmm/eddie/GenScotDepression/amelia/packages/PRSice_v2.3.3/PRSice.R \
    --dir . \
    --prsice /exports/igmm/eddie/GenScotDepression/amelia/packages/PRSice_v2.3.3/PRSice \
    --base $sumstats \
    --target ALSPAC/1000G/data_QC \
    --beta \
    --A1 A1 \
    --A2 A2 \
    --pvalue p \
    --snp SNP \
    --stat b \
    --no-default \
    --binary-target T \
    --pheno-file ALSPAC/no_pheno.txt \
    --pheno-col no_pheno \
    --fastscore \
    --bar-levels 5e-8,5e-7,5e-6,5e-5,5e-4,0.001,0.05,0.1,0.2,0.5,1 \
    --all-score  \
    --clump-r2 0.25 \
    --clump-kb 500 \
    --thread 8 \
    --print-snp \
    --out Output/$output

