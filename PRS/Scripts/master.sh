# Pipeline to make PRSs for CRP and IL-6 in ALSPAC

cd /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS/

# ------------------------------------------------------------------------------
# Convert the 1000G ALSPAC data into bim, bed, fam files
qsub -P sbms_CrossImmune Scripts/1000G_ALSPAC/prep.sh
qsub -P sbms_CrossImmune Scripts/1000G_ALSPAC/bgen_to_pgen.sh
qsub -P sbms_CrossImmune Scripts/1000G_ALSPAC/pgen_merge.sh
qsub -P sbms_CrossImmune Scripts/1000G_ALSPAC/pgen_to_bed.sh

# ------------------------------------------------------------------------------
# Make dummy pheno file for PRS
qsub -P sbms_CrossImmune Scripts/ALSPAC.sh

# ------------------------------------------------------------------------------
# Download CRP GWAS sum stats from UKB and format into QC version for SBayesR
qsub -N CRP_prep -P sbms_CrossImmune Scripts/CRP_ukb_sumstats.sh
# ------------------------------------------------------------------------------
# Get and format GWASs
qsub -N GWAS_prep Scripts/tidy_GWASs.sh

# ------------------------------------------------------------------------------
# Prep for using SBayesR (takes a long time, downloads the LD matrices to scratch)
qsub -N parta_prep -P sbms_CrossImmune Scripts/SBayesR_prep/parta.sh
qsub -N partb_prep -P sbms_CrossImmune Scripts/SBayesR_prep/partb.sh
qsub -N partc_prep -P sbms_CrossImmune Scripts/SBayesR_prep/partc.sh
qsub -N partd_prep -P sbms_CrossImmune Scripts/SBayesR_prep/partd.sh
qsub -N parte_prep -P sbms_CrossImmune Scripts/SBayesR_prep/parte.sh
qsub -N SBayesR_prep -hold_jid part* -P sbms_CrossImmune Scripts/SBayesR_prep/SBayesR_prep.sh 
# ------------------------------------------------------------------------------
# Download SBayesR
# cd /exports/igmm/eddie/GenScotDepression/amelia/packages/
# curl -O https://cnsgenomics.com/software/gctb/download/gctb_2.03beta_Linux.zip
# unzip gctb_2.03beta_Linux.zip
# cd -

# Run SBayesR & calculate PRSs:

# "GWASs/PLE_UKB/ukbiobank-any_psychotic_experience.sumstats.txt.QC any" \
# "GWASs/SCZ/daner_PGC_SCZ52.QC SCZ" \
# "GWASs/SCZ_w3/PGC3_SCZ_wave3_public.v2.tsv.QC SCZ_w3" \
# "GWASs/MDD/PGC_UKB_depression_genome-wide.txt.QC MDD" \

for sumstats in \
"GWASs/CRP_ukb_sumstats/30710_raw.gwas.imputed_v3.both_sexes.varorder.tsv.QC CRP" \
"GWASs/IL6_Ahola_Olli/meta_filt_IL_6_noBMI.txt.QC IL6"
do
	set $sumstats
	qsub -N SBayesR_${2} -hold_jid *_prep -P sbms_CrossImmune \
 -v sumstats=$1,output=${1}"_SBayesR" \
Scripts/SBayesR.sh

qsub -N PRSice_${2} -hold_jid SBayesR_${2} -P sbms_CrossImmune \
-v sumstats=${1}"_SBayesR.snpRes",output=${2}"_SBayesR" \
Scripts/PRS_calc_1000G_SBayesR.sh

qsub -N CT_${2} -P sbms_CrossImmune \
-v sumstats=${1},output=${2}"_CT" \
Scripts/PRS_calc_1000G_CT.sh

done
# -------------------------------------------------
# Check why SBayesR dropped so many SNPs for CRP PRS, compare it to the C+T method

# first stage in QC'd data from datastore 
# qlogin -q staging
# cp /exports/cmvm/datastore/scs/groups/ALSPAC/users/amelia/Data/dataSubQCWide.csv /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes
# exit

# Rscript PRS/Scripts/CRP_PRS_check.R

# -------------------------------------------------
# Save numbers of SNPs to files:

qsub -N finalJob -hold_jid PRSice_* -P sbms_CrossImmune Scripts/final_job.sh  

# -------------------------------------------------
# Run regression between MDD, SCZ, PLE PRS and depressive episodes/PLEs locally: Scripts/PRS_predict.R


# -------------------------------------------------
