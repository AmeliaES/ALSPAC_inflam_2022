#$ -N pgen_merge_prep
#$ -hold_jid pgen_qc_prep
#$ -l h_vmem=16G
#$ -l h_rt=8:00:00
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk

#############################################################
###### Merge PGENs into one autosome file ###################
#############################################################

SCRATCH=/exports/eddie/scratch/$USER/
cd $SCRATCH

# Paste prefix of PGEN file names and save as txt file.
echo ${SCRATCH}ALSPAC/data_chr1_QC > $SCRATCH/mergelist.txt
for CHR in {2..22}; do
	echo ${SCRATCH}ALSPAC/data_chr${CHR}_QC >> $SCRATCH/mergelist.txt
done

/exports/igmm/eddie/GenScotDepression/local/bin/plink2 \
	--pmerge-list $SCRATCH/mergelist.txt 'pfile-vzs' \
	--make-pgen \
	--out $SCRATCH/ALSPAC/data_QC \
	--threads 1

