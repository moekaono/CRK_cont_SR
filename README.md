# Cospectral analysis between soil respiration component and biophysical drivers
This repository includes the code and data associated with the project to investigate the biophysical and abiotic drivers of soil respiration components (i.e., autotrophic and heterotrophic respiration) between 2022 and 2024 in Davy Crockett National Forest, TX, USA.

**Article**: Ono, M., Mitra, B., Baniya, B., Kim, D., and Noormets, A. (2025). The use of newly assimilated photosynthates by soil autotrophic and heterotrophic respiration on a diurnal scale, Biogeosciences, 22, 5833–5848, https://doi.org/10.5194/bg-22-5833-2025.

**Data**: Ono, M., & Noormets, A. (2025). Software from: The Use of Newly Assimilated Photosynthates by Soil Autotrophic and Heterotrophic Respiration on a Diurnal Scale (Version 1.0.0) (Computer software). https://doi.org/10.5281/zenodo.17176648. <br><br>

#### Code folder contents
| Code file title | Description |
| --- | --- |
| 00_CRK_cont_SR_qaqc.Rmd | QA/QC continuous soil chamber data |
| 01_EC_SR_combine.Rmd | Combining the 00 output with the tower data |
| 01A_daylight_GPP.R | Calculating daylight GPP |
| 01B_CRK_cont_fig.Rmd | Visualizing time-series data |
| 02_test_cospectral_raw_sim100.Rmd | Cospectral analysis between SR components and biophysical variables |
| 02A_cospectral_raw_env.Rmd | Cospectral analysis between GPP & PAR and Ts |
| 02_cospectral_raw.Rmd (OLD) | The old version of 02 |
| 03A_residual_analysis_hour_dielQ10.Rmd | Cospectral analysis between residuals of SR components and biophysical variables |
| 03B_residual_analysis_hour_weeklyQ10.Rmd | Cospectral analysis between residuals of SR components and biophysical variables |
| 03B_residual_analysis_hour_weeklyQ10.Rmd | Cospectral analysis between residuals of SR components and biophysical variables |
| 04_temp_pwr_peak.Rmd | Visualizing temporal peaks for spectral and cospectral analysis for every combination |
| 05_phase_diff_fig.Rmd | Visualizing the phase differences in hours |
| root_profile.R | Visualizing the root profile for Fig S1 |
| DBH_summary.R | Calculating site-level DBH |
