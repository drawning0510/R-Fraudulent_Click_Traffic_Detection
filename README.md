# Fraudulent Click Traffic Detection
Built LDA and QDA models on variables obtained from Principal Component Analysis (PCA) and Kolmogorov-Smirnov (KS) and tuned by leave-one-out cross-validation (LOOCV) to predict fraudulent online advertising click traffic
# Instruction for running the code
1. Download both data files and R code in the same folder
2. Run `Fraud Detection.R` in Rstudio
# File list
1. Source files: `train_50k.csv`, `ks_variables.csv`
2. Supporting files: `Project Instructions.pdf`, `data dictionary.xlsx`
3. R code: `Fraud Detection.R`
4. Outputs: `PCA.png`, `PCA_cum.png`, `PCA_varaiance.csv`, `ROC_LDA_PCA.png`, `ROC_QDA_PCA.png`, `ROC_LDA_KS.png`, `ROC_QDA_KS.png`
5. Final Report: `Final presentation.pptx`, `Final Report on Fraud Detection.pdf`

P.S. The result obtained after running the code may be different from the result showed in the final report because the uploaded dataset is only part of the original data due to file size limit
