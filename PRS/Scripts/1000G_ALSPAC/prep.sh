# Copy genetic data from ALSPAC dir on datastore to scratch directory
#$ -N pgen_prep
#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -q staging
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk


cd /exports/cmvm/datastore/scs/groups/ALSPAC/data/genomics/B3421/genetics/1000G_2021-01-25/all1/
cp -r data/ /exports/eddie/scratch/$USER/

cd /exports/eddie/scratch/$USER/data/

mv data_chr01.bgen data_chr1.bgen
mv data_chr02.bgen data_chr2.bgen
mv data_chr03.bgen data_chr3.bgen
mv data_chr04.bgen data_chr4.bgen
mv data_chr05.bgen data_chr5.bgen
mv data_chr06.bgen data_chr6.bgen
mv data_chr07.bgen data_chr7.bgen
mv data_chr08.bgen data_chr8.bgen
mv data_chr09.bgen data_chr9.bgen

