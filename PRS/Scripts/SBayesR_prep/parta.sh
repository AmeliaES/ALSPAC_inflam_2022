#! /bin/sh
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -l h_vmem=64G
#$ -l h_rt=48:00:00
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk

# Change directory to your scratch space
cd /exports/eddie/scratch/s1211670

# Download partitioned files to scratch directory (takes a few hours)
wget https://zenodo.org/record/3375373/files/ukb_50k_bigset_2.8M.zip.partaa

