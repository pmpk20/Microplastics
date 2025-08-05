# Microplastics

Details:
- All data, code, and outputs for the microplastics paper: ``Individual Option Prices for Mitigating Microplastic Pollution``
- Preprint available here: <https://dx.doi.org/10.2139/ssrn.5258504>
- Co-authors: Dr Alistair Hunt (University of Bath), Dr Stavros Georgiou (HSE), Dr Christoph Rheinberger (ECHA)
- Repo Author: Dr Peter King (p.king1@leeds.ac.uk)
- Last Change: 05/08/2025


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
- Reproduce the working environment in R using: `session_info_2025_01_30.txt`


