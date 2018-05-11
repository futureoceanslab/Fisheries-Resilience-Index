# Fisheries-Resilience-Index
Ecological, socieconomic and institutional resilience to shifting fish stocks

## Folders

- **data**: contains the data used to plot the figures

- **Figures**: the figures produced

- **Tables**: tables produced

## scripts

- **aux_functions.R**: Contains auxiliary functions used in other scripts

- **Figure1.R**: This script reads `data/final_index.csv` and plots `Figures/Figure 1.png`.

- **Figure2.R**: This script reads `data/final_index.csv` and plots `Figures/Figure 2.png`.

- **Figure2SI.R**: Reads `data/ecological_factors_country.csv`, `data/institutional_factors_country.csv` and `data/socioeconomic_factors_country.csv` and plots `Figures/Fig 2 SI.png`

- **Figure3.R**: This script reads `data/final_index.csv`, plots `Figures/Figure 3.png `and creates `Tables/Fig3_p_values.docx`.

- **Figure3SI.R**: This script reads `data/final_index.csv` and plots `Figures/Fig 3 SI.png`.

- **Figure4SI.R**: Reads `data/ecological_indicators.csv`, `data/institutional_indicators.csv` and `data/socioeconomic_indicators.csv`. Computes correlation matrices for each set of indicators and plots the correlation matrices in `Figures/Fig 4 SI.png`.

- **Figure5SI.R**: This script reads `data/final_index.csv`, plots `Figures/Fig 5 SI.png` and creates `Tables/Fig5_p_values.docx`.

- **Figure6SIa_b.R**: This script reads `data/SSBLIM_FLIM_HAKE.csv` and `SSBLIM_FLIM_COD.csv` and plots `Figures/Fig 6 SIa.png` and `Figures/Fig 6 SIb.png`.

- **Figure8.R**: This script reads `data/final_index.csv`, plots `Figures/Figure 8.png` and creates `Tables/Fig8_p_values.docx`.

- **fig_dimensions.R**: This script reads `data/final_index.csv` and plots `Figures/Fig dimensions.png`.

- **indexes.R**: produces `data/final_index.csv` from `data/institutional_factors_country.csv`, `data/socioeconomic_factors_country.csv`, `data/ecological_factors_country.csv` and `data/Other_index.csv`.

- **process_indicators.R**: this script produces ecological, socioeconomic and institutional factors from `data/ecological_indicators.csv`, `data/socioeconomic_indicators.csv` and `data/institutional_indicators.csv`. Also, creates tables for "SI 2. INDICATORS AND FACTORS"

- **Wilcoxon_test.R**: this script reads the Resilience Index from `data/final_index.csv` with and without the indicators: `Above_TAC` and `Compliance` to compare the robustness of the index. 

- **glm_models.R**: this script run three different models to compare the Resilience Index with LAT and observe significative relationship with species and dimensions. Produces the Summary with the LAT-Index relationship. 


## Instructions

`run_all.R` runs all the scripts needed to reproduce the results. First run `process_indicators.R` to compute factors from indicators and get tables for "SI 2. INDICATORS AND FACTORS", then run `indexes.R` to compute `data/final_index.csv`. Then run all the figure scripts to generate the figures in Figures and some tables in Tables. 

## Notes

 `Figure1.R`, `Figure2.R` and `glm_models.R` require an Internet connection to download the map files.
 
 Since some of the packages used in this repository make use of the library rJava, please check that your Java version is updated and that you have installed the right version (32-bit or 64-bit) for your R distribution.