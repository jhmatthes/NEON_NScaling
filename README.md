# NEON_NScaling

This project aggregates ecosystem data on nitrogen pools and fluxes across NEON sites. It was developed as a group project as part of the NEON Science Summit in October 2019.

The R/ file contains the code to download and do preliminary pre-processing for the datasets used in this analysis. 

Within R/, download_data.R contains the code to download and stack the raw NEON datasets through the API, and prelim_processing.R contains scripts to load and clean the raw stacked data into forms that can be combined. 