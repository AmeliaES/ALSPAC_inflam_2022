#!/bin/sh
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk
#$ -l h_vmem=32G

# Prep ALSPAC genetic data to make PRSs from

cd /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS

# Add PLINK-2 to executable path.
export PATH=$PATH:/exports/igmm/eddie/GenScotDepression/amelia/packages/

. /etc/profile.d/modules.sh
module load igmm/apps/R/3.6.1

#------------------------------------------------------
# - Create dummy pheno file
Rscript Scripts/generate_dummy_pheno.R


