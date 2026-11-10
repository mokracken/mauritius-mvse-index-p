Divergent Climate Suitability Profiles for Dengue and Chikungunya Transmission by Aedes albopictus in Mauritius
Overview
This repository contains Google Earth Engine and R scripts, monthly Index P estimates, and supporting CSV files for a study comparing climate-driven transmission suitability for dengue and chikungunya in Mauritius using the Mosquito-borne Viral Suitability Estimator (MVSE) Index P.

The workflow covers ERA5-Land climate extraction, derivation of biological priors, locality-monthly Index P estimation, quality control, national and district summaries, spatial and seasonal suitability analyses, mapping, and supplementary sensitivity analyses. Index P represents relative climate suitability for transmission, not the probability of an outbreak.
Repository contents
Climate extraction
- Locality_Tmean_Tdew_monthly_extraction_GEE.js — Google Earth Engine script for extracting locality-level temperature and dewpoint data from ERA5-Land.
Main R scripts
1. 1_MVSE priors estimation.R — Derives climate-informed MVSE priors for dengue and chikungunya, including Aedes albopictus biting-rate and extrinsic-incubation-period parameters.
2. 2_Index P monthly MVSE model.R — Estimates locality-monthly Index P for both viruses using climate data and the priors from Script 1; applies virus-specific vector-competence multipliers and produces posterior summaries.
3. 3_Index P monthly Quality control.R — Checks the monthly Index P outputs and generates descriptive summaries and diagnostic plots.
4. 4_Index P District analysis.R — Joins locality-level estimates to districts and generates district and pooled national summaries.
5. 5_Index_P Suitability_analysis.R — Compares dengue and chikungunya suitability over time and space; produces national summaries, transmission-window metrics, mapping inputs, and supplementary tables.
Additional analyses and mapping
- Climate variable analysis.R — Summarises and plots district-level monthly temperature and relative humidity climatology.
- seasonal Index P mapping.R — Creates seasonal locality-level Index P maps.
- Peak suitability mapping.R — Creates locality-level peak suitability maps.
- VC_multiplier_comparison.R — Compares Index P summaries with and without the fixed virus-specific vector-competence multipliers.
- Face_Validity_spearman_corr.R — Calculates Spearman correlations between national monthly dengue Index P and reported dengue cases at lags of 0–4 months for January–October 2024.
Data and estimates
- MVSE_indexP_denv_monthly_estimates.csv — Locality-monthly dengue Index P estimates.
- MVSE_indexP_chik_monthly_estimates.csv — Locality-monthly chikungunya Index P estimates.
- locality_match_distr.csv — Lookup linking locality identifiers (site_id) to their administrative districts (district).
- National monthly dengue cases_2024.csv — National monthly dengue case data used in the supplementary Spearman correlation analysis.
- Supplementary Table S5 (spatial Index P estimates; CSV) — Locality-level spatial suitability summaries.
- Supplementary Table S6 (seasonal monthly Index P estimates; CSV) — Locality-level calendar-month suitability summaries.
The two monthly Index P files contain posterior median estimates (indexP_med) and their 2.5th and 97.5th percentile bounds (indexP_p025 and indexP_p975), alongside date and locality identifiers. The bounds describe uncertainty for individual locality-month estimates. Aggregated summaries of those bounds should not be interpreted as formal district- or national-level credible intervals.

File-name note: Script 2 writes files named MVSE_indexP_denv_monthly_FINAL.csv and MVSE_indexP_chik_monthly_FINAL.csv. The corresponding monthly estimate files provided in this repository use the _monthly_estimates.csv names listed above.
Workflow
1. Use the Google Earth Engine script to obtain locality-level ERA5-Land temperature and dewpoint data.
2. Run 1_MVSE priors estimation.R to derive the MVSE prior table.
3. Run 2_Index P monthly MVSE model.R to estimate monthly Index P for dengue and chikungunya.
4. Run 3_Index P monthly Quality control.R to inspect the estimates.
5. Run the district and suitability analysis scripts (4_... and 5_...) using both virus-specific Index P files and locality_match_distr.csv.
6. Run the seasonal and peak mapping scripts as required. The climate analysis, vector-competence comparison, and Spearman correlation scripts are additional analyses.
Several R scripts use file.choose() for interactive file selection. The mapping scripts also require locality boundary geometries (with identifiers matching the Index P files). These spatial boundaries and the original daily climate export are not shown among the repository files listed here; they may need to be obtained or generated separately to reproduce the full workflow from the beginning.
Main input fields
Daily locality-level climate data: date, site_id, tmean, tdew.

Monthly Index P estimates: date, site_id, indexP_med, indexP_p025, indexP_p975.

Locality-to-district lookup: site_id, district.
Priors and uncertainty
Separate dengue and chikungunya biological priors are derived in Script 1 and passed to the MVSE estimation in Script 2. Script 2 uses 500 posterior samples per locality, calculates the median and 2.5th/97.5th percentiles, and applies fixed vector-competence multipliers (0.354 for dengue and 0.625 for chikungunya). Consult the scripts for the precise distributions, settings, and implementation.
Software
Climate extraction uses Google Earth Engine; the statistical analysis and visualisation use R. Required R packages are declared in the respective scripts, including the MVSE package for Index P estimation.
Data availability
This repository provides the dengue and chikungunya monthly Index P estimates, the locality-to-district lookup, national monthly dengue case data for the supplementary correlation analysis, supplementary Index P summary tables, and analytical scripts. The underlying climate data source is ERA5-Land, available through the Copernicus Climate Data Store.
Citation
Please cite the associated manuscript once publication details are available.
Contact
Amar Teeluck
The University of Queensland
m.teeluck@student.uq.edu.au
