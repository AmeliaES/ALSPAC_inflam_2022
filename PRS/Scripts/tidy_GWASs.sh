#! /bin/sh
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -l h_vmem=128G
#$ -l h_rt=48:00:00
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk

# QC GWASs
cd /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS

. /etc/profile.d/modules.sh
module load igmm/apps/R/3.6.1

# ------------------------------------------
# Il-6
# Download here: https://data.bris.ac.uk/data/dataset/3g3i5smgghp0s2uvm1doflkx9x
# This is absolutely huge, a zip file of all cytokines - download all of them locally and then
# copy the IL-6 one (without BMI) to Eddie

# mkdir GWASs/IL6_Ahola_Olli
Rscript Scripts/format_IL6.R

# ------------------------------------------
# CRP
# Get GWAS sum stats from UKB on CRP and format them for SBayesR

# mkdir GWASs/CRP_ukb_sumstats
wget -P GWASs/CRP_ukb_sumstats https://broad-ukb-sumstats-us-east-1.s3.amazonaws.com/round2/additive-tsvs/30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.bgz -O 30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.bgz
wget -P GWASs/CRP_ukb_sumstats https://broad-ukb-sumstats-us-east-1.s3.amazonaws.com/round2/annotations/variants.tsv.bgz -O variants.tsv.bgz

mv variants.tsv.bgz variants.tsv.gz
gunzip variants.tsv.gz

mv 30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.bgz 30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.gz
gunzip 30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.gz

Rscript Scripts/format_CRP.R

# ------------------------------------------
# MDD
# Download Howard et al. 2019 MDD GWAS from PGC into GWASs/MDD/
# mkdir GWASs/MDD
wget -P GWASs/MDD https://datashare.ed.ac.uk/bitstream/handle/10283/3203/PGC_UKB_depression_genome-wide.txt
head GWASs/MDD/PGC_UKB_depression_genome-wide.txt

# ------------------------------------------
# SCZ
# Download Ripke et al. 2014 SCZ GWAS from PGC into GWASs/SCZ/
# mkdir GWASs/SCZ
wget -P GWASs/SCZ https://ndownloader.figshare.com/files/28570554
mv GWASs/SCZ/28570554 GWASs/SCZ/daner_PGC_SCZ52.gz
gunzip GWASs/SCZ/daner_PGC_SCZ52.gz
head GWASs/SCZ/daner_PGC_SCZ52

# -------------------
# SCZ - wave 3 (unpublished)
# mkdir GWASs/SCZ_w3
wget -O GWASs/SCZ_w3/PGC3_SCZ_wave3_public.v2.tsv.gz https://figshare.com/ndownloader/files/28169757
gunzip GWASs/SCZ_w3/PGC3_SCZ_wave3_public.v2.tsv.gz
head GWASs/SCZ_w3/PGC3_SCZ_wave3_public.v2.tsv

# ------------------------------------------
# PLEs
# Download from https://walters.psycm.cf.ac.uk/
# mkdir GWASs/PLE_UKB/
wget --no-check-certificate -P GWASs/PLE_UKB/ https://walters.psycm.cf.ac.uk/ukbiobank-any_psychotic_experience.sumstats.txt.gz
wget --no-check-certificate -P GWASs/PLE_UKB/ https://walters.psycm.cf.ac.uk/ukbiobank-distressing_psychotic_experience.sumstats.txt.gz
wget --no-check-certificate -P GWASs/PLE_UKB/ https://walters.psycm.cf.ac.uk/ukbiobank-multiple_psychotic_experience.sumstats.txt.gz

for f in GWASs/PLE_UKB/*.gz
do 
	gunzip $f
done

ls GWASs/PLE_UKB/
# ------------------------------------------
# Format MDD, SCZ and PLE GWASs:
Rscript Scripts/tidy_GWASs.R

