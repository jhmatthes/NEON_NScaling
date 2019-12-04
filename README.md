# NEON_NScaling

This project aggregates ecosystem data on nitrogen pools and fluxes across NEON sites. It was developed as a group project as part of the NEON Science Summit in October 2019.

The R/ file contains the code to download and do preliminary pre-processing for the datasets used in this analysis. 

Within R/:
1. download_data.R contains the code to download and stack the raw NEON datasets through the API. It will make a local data/ file within your project path and download the raw and stacked NEON data to that directory. 
2. prelim_processing.R contains scripts to load and clean the raw stacked data into data frames that can be combined. 
