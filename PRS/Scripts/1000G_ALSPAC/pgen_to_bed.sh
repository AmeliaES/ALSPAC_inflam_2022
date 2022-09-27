#$ -hold_jid pgen_merge_prep
#$ -N pgen_to_bed_prep
#$ -l h_vmem=16G
# -pe sharedmem 4
#$ -l h_rt=4:00:00
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk


SCRATCH=/exports/eddie/scratch/$USER

/exports/igmm/eddie/GenScotDepression/local/bin/plink2 \
--pfile $SCRATCH/ALSPAC/data_QC \
--maf 0.01 \
--mac 100 \
--geno 0.02 \
--mind 0.02 \
--make-bed \
--out $SCRATCH/ALSPAC/data_QC \
--threads 4

