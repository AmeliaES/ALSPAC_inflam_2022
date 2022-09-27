# Early-life inflammatory markers and subsequent psychotic and depressive episodes between 10 to 28 years of age

## Authors
Amelia J. Edmondson-Stait, Xueyi Shen, Mark J. Adams, Miruna C. Barbu, Hannah J. Jones, Veronique E. Miron, Judith Allardyce, James P. Boardman, Stephen M. Lawrie, Andrew M. McIntosh, Golam M. Khandaker, Alex S. F. Kwong, Heather C. Whalley

## Abstract
Inflammation is implicated in depression and psychosis, including association of childhood inflammatory markers on the subsequent risk of developing symptoms. However, it is unknown whether early-life inflammatory markers are associated with the number of depressive and psychotic symptoms from childhood to adulthood.  Using the prospective Avon Longitudinal Study of Children and Parents birth cohort (N=up-to 6,401), we have examined longitudinal associations of early-life inflammation [exposures: interleukin-6 (IL-6), C-reactive protein (CRP) levels at age 9y; IL-6 and CRP DNA-methylation (DNAm) scores at birth and age 7y; and IL-6 and CRP polygenic risk scores (PRSs)] with the number of depressive episodes and psychotic experiences (PEs) between ages 10-28 years. Psychiatric outcomes were assessed using the Short Mood and Feelings Questionnaire and Psychotic Like Symptoms Questionnaires, respectively. Exposure-outcome associations were tested using negative binomial models, which were adjusted for metabolic and sociodemographic factors. Serum IL-6 levels at age 9y were associated with the total number of depressive episodes between 10-28y in the base model (n=4,835; β=0.066; 95%CI:0.020-0.113; pFDR=0.041) which was weaker when adjusting for metabolic and sociodemographic factors. Weak associations were observed between inflammatory markers (serum IL-6 and CRP DNAm scores) and total number of PEs. Other inflammatory markers were not associated with depression or PEs. Early-life inflammatory markers are associated with the burden of depressive episodes and of PEs subsequently from childhood to adulthood. These findings support a potential role of early-life inflammation in the aetiology of depression and psychosis and highlight inflammation as a potential target for treatment and prevention. 

## Broad overview of folder structure
**Folders include `Scripts` for code to run the analysis and `Output` for the output from these scripts.**
### Data_prep/
* `1_Clean_data.R` - preprocesses initial dataset to extract variables of interest
* `2_Explore_data.R` - create figures 
* `demographics.R` - create demographic tables
### PRS/
* Create polygenic risk scores on HPC Eddie. Please see the `master.sh` script in `Scripts/` for order to run scripts.
### DNAm/
* Create DNAm scores and PCs of residualised CpGs on HPC Eddie. Please see the `master.sh` script in `Scripts/` for order to run scripts.
### Imputation/
* Conduct multiple imputation on missing data. Please see the `master.sh` script in `Scripts/` for order to run scripts.
### Regression_analysis/
* Carry out all the regression analysis, create tables and figures. Please see the `combine_results.R` script in `Scripts/` for order to run scripts.

*If there are any errors or if you have any suggestions for improvements please contact me at amelia.edmondson-stait@ed.ac.uk.*
