# Microplastics

Details:
- All data, code, and outputs for the microplastics paper: ``Individual Option Prices for Mitigating Microplastic Pollution``
- Preprint available here: <https://dx.doi.org/10.2139/ssrn.5258504>
- Inspired by <https://doi.org/10.1016/j.jpubeco.2004.01.005>
- Co-authors: Dr Alistair Hunt (University of Bath), Dr Stavros Georgiou (HSE), Dr Christoph Rheinberger (ECHA)
- Repo Author: Dr Peter King (p.king1@leeds.ac.uk)
- Last Change: 24/03/2026


Description:
- The anonymised survey data has 1564 obs.
- Execute `00_Microplastics_Replicator.R` to create all the outputs in the paper
- Files are numbered in order of execution
- Models executed with 10,000 bootstrap replicates so computationally intensive
- Most model outputs now uploaded, let me know if anything is missing.
- GLM reports positive log-likelihood and negative AIC, we flip those signs in outputs.
- Using specialist and High-Performance Computing systems provided by Information Services at the University of Kent, and by the Research Computing Team at the University of Leeds.
- Using RStudio and Apollo (Hess and Palma, 2019).
- This work was funded by DEFRA.
- Ethical approval for the data collection was granted by the Department of Economics, University of Bath.
- Reproduced copy of the survey: `SurveyPrint_2022_04_08.pdf`
- Reproduce the working environment in R using: `Data/session_info_2026_03_24.txt`


Directory structure:
```
├── 00_Microplastics_Replicator.R
├── SetupScripts/
│   ├── 01_Microplastics_SetupData_Order0.R
│   ├── 02_Microplastics_SetupData_Order1.R
│   └── 03_Microplastics_MergeSamples.R
├── CVScripts/
│   ├── 04_Microplastics_Models_SimulateEOP.R
│   ├── 05_Microplastics_Models_BothStages_InText.R
│   ├── 06_Microplastics_Models_Alternative1_Asymmetric.R
│   ├── 07_Microplastics_Models_Alternative2_NumericUncertainty.R
│   ├── 08_Microplastics_Models_RobustnessStage1.R
│   ├── 09_Microplastics_Models_RobustnessStage2.R
│   ├── 10_Microplastics_Models_Truncation.R
│   └── 20_Microplastics_TableC5_NaiveProbit.R
├── OtherScripts/
│   ├── Tables/
│   │   ├── 11_Microplastics_Table1_SampleSummary.R
│   │   ├── 12_Microplastics_Table4_SimulatedEOP.R
│   │   ├── 13_Microplastics_TableB1_SampleTest.R
│   │   ├── 14_Microplastics_TableB2_Summary.R
│   │   ├── 15_Microplastics_TableB3_ExpectationSummary.R
│   │   ├── 16_Microplastics_TableC1_AlternativeSpecifications.R
│   │   ├── 17_Microplastics_TableC2_TruncationModels.R
│   │   ├── 18_Microplastics_TableC3_RobustnessStage1.R
│   │   └── 19_Microplastics_TableC4_RobustnessStage2.R
│   └── Figures/
│       ├── 21_Microplastics_Figure2_EOP.R
│       ├── 22_Microplastics_FigureB1_Turnbull.R
│       ├── 23_Microplastics_FigureB2_Attitudes.R
│       ├── 24_Microplastics_FigureB3_MeanVariance.R
│       └── 25_Microplastics_FigureB4_MeanVariance.R
├── Data/               # anonymised survey data (1564 obs.)
├── Tables/             # all model output tables
├── Figures/            # all paper figures
└── Archive/            # old/superseded scripts, not needed for replication
```


