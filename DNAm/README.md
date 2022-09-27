# Create methylation scores for IL-6 and CRP and PCs for residualised CpGs

- Please see `Scripts/master.sh` for scripts to run on a HPC (specifically the SGE called Eddie at the University of Edinburgh).

### CRP
7 CpGs genome wide significant in CRP EWAS (Ligthart et al. 2016) are used to calculate DNA methylayion risk scores. Method described in Barker et al. 2018 (also used in Steveson et al. 2020 and Green et al. 2021).

### IL-6
35 CpGs identified using an elastic net model (Stevenson et al. 2021).

### Residualised CpGs
CpGs across the genome are regressed onto sex, age and batch. PCA is performed on these residuals.


