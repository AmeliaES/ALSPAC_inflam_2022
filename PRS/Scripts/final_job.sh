#$ -e /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -o /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/joblogs
#$ -m beas
#$ -M amelia.edmondson-stait@ed.ac.uk
#$ -l h_vmem=1G

cd /exports/igmm/eddie/GenScotDepression/amelia/ALSPAC_inflam_episodes/PRS/

# Number of SNPs used to calculate PRSs:
echo "CRP SBayesR:" > Output/n_SNPs
tail -n +2 Output/CRP_SBayesR.snp | wc -l >> Output/n_SNPs
echo "IL6 SBayesR:" >> Output/n_SNPs
tail -n +2 Output/IL6_SBayesR.snp | wc -l >> Output/n_SNPs
echo "MDD SBayesR:" >> Output/n_SNPs
tail -n +2 Output/MDD_SBayesR.snp | wc -l >> Output/n_SNPs
echo "PLE (any) SBayesR:" >> Output/n_SNPs
tail -n +2 Output/any_SBayesR.snp | wc -l >> Output/n_SNPs
echo "SCZ SBayesR:" >> Output/n_SNPs
tail -n +2 Output/SCZ_SBayesR.snp | wc -l >> Output/n_SNPs
echo "SCZ (wave 3) SBayesR:" >> Output/n_SNPs
tail -n +2 Output/SCZ_w3_SBayesR.snp | wc -l >> Output/n_SNPs

cat Output/n_SNPs

