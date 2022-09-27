#! /bin/sh
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -l h_vmem=64G
#$ -l h_rt=48:00:00
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk

# Change directory to your scratch space
cd /exports/eddie/scratch/s1211670

# Concatenate file parts
cat ukb_50k_bigset_2.8M.zip.parta* > ukb_50k_bigset_2.8M.zip 

# Unzip
unzip ukb_50k_bigset_2.8M.zip

# Clean up partitioned files
#rm ukb_50k_bigset_2.8M.zip.part*

# The file which contains the list of LD matrices ukb_50k_begset_2.8M/ukb50k_2.8M_shrunk_sparse.mldmlist is incorrect and contains wrongly named files. Run the following to create a new list which will be used in SBayesR (these must be in order of chromosomes 1 to 22):

for i in {1..22}
do
echo /exports/eddie/scratch/s1211670/ukb_50k_bigset_2.8M/ukb50k_shrunk_chr${i}_mafpt01.ldm.sparse >> ukb_50k_bigset_2.8M/ukb50k_2.8M_shrunk_sparse.new.mldmlist
done

