#$ -N pgen_qc_prep
#$ -hold_jid pgen_prep
#$ -t 1-22
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -l h_vmem=4G
#$ -l h_rt=12:00:00
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk


# Job to convert BGEN files to PGEN.CH

CHR=$SGE_TASK_ID

SCRATCH=/exports/eddie/scratch/$USER/

cd $SCRATCH

mkdir -p ALSPAC

/exports/igmm/eddie/GenScotDepression/local/bin/plink2 \
--bgen data/data_chr${CHR}.bgen 'ref-first' \
--sample data/data.sample \
--keep data/unrelated_children.txt \
--rm-dup 'exclude-all' \
--make-pgen 'vzs' \
--out ALSPAC/data_chr${CHR}_QC \
--memory 4000 \
--threads 1
