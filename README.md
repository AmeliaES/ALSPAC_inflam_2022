## Code for [Edmondson-Stait et al., 2022](https://doi.org/10.1016/j.bbih.2022.100528)

<p align="center">
  <img src="https://github.com/AmeliaES/ALSPAC_inflam_2022/blob/main/paper_header.png?raw=true" alt="paper_header.png"/>
</p>

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
