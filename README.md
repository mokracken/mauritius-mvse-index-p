# Divergent Climate Suitability Profiles for Dengue and Chikungunya Transmission by *Aedes albopictus* in Mauritius

## Overview

This repository contains the Google Earth Engine and R scripts used to estimate and compare climate-driven transmission suitability for dengue and chikungunya across Mauritian localities using the Mosquito-borne Viral Suitability Estimator (MVSE) Index P.

The workflow includes extraction of ERA5-Land temperature and dewpoint data, calculation of relative humidity, estimation of monthly Index P values using virus-specific priors, quality control, temporal and spatial summaries, district analysis, and preparation of mapping and supplementary outputs.

## Repository contents

### Google Earth Engine script

The Google Earth Engine script extracts daily ERA5-Land data for Mauritius locality polygons, including:

- mean, minimum, and maximum 2 m temperature;
- 2 m dewpoint temperature; and
- locality and date identifiers.

Temperature values are converted from Kelvin to degrees Celsius and exported as a CSV file.

### R scripts

- `Climate variable analysis.R`  
  Prepares the extracted climate data and calculates relative humidity.

- `MVSE priors estimation.R`  
  Defines the virus-specific biological parameter distributions used for dengue and chikungunya.

- `Index P monthly MVSE model.R`  
  Estimates monthly locality-level Index P values and posterior uncertainty bounds.

- `P index monthly Quality control.R`  
  Checks the Index P outputs and produces descriptive summaries and diagnostic plots.

- `Index_P_Suitability_analysis.R`  
  Compares dengue and chikungunya suitability over time and space and creates transmission-window, mapping, and supplementary outputs.

- `District analysis.R`  
  Links localities to districts and produces district and pooled national summaries.

- `seasonal Index P mapping.R`  
  Prepares calendar-month Index P values for seasonal mapping.

- `Peak suitability mapping.R`  
  Prepares locality-level peak suitability values for mapping.

## Workflow

Run the scripts in this order:

1. Google Earth Engine climate extraction
2. `Climate variable analysis.R`
3. `MVSE priors estimation.R`
4. `Index P monthly MVSE model.R`
5. `P index monthly Quality control.R`
6. `Index_P_Suitability_analysis.R`
7. `District analysis.R`
8. Seasonal and peak mapping scripts as required

## Input data

The Google Earth Engine script requires a locality polygon feature collection with a locality identifier.

The climate-processing and MVSE scripts require daily locality-level climate data containing:

```text
date
site_id
tmean
tdew
```

The downstream Index P scripts require separate dengue and chikungunya files containing:

```text
date
site_id
indexP_med
indexP_p025
indexP_p975
```

District and supplementary summaries also require a lookup file containing:

```text
site_id
district
```

## Priors

Separate biological parameter distributions are used for dengue and chikungunya. These represent uncertainty in mosquito survival, biting, viral development, vector competence, transmission, and the human infectious period.

The exact prior distributions and their sources are provided in `MVSE priors estimation.R`.

## Main outputs

The scripts generate:

- monthly locality-level dengue and chikungunya Index P estimates;
- posterior median and uncertainty bounds;
- national monthly suitability summaries;
- annual transmission-window duration;
- district-level descriptive summaries;
- locality-level median and peak suitability values;
- calendar-month suitability summaries; and
- supplementary-table outputs.

## Posterior uncertainty

The variables indexP_p025 and indexP_p975 represent the posterior 2.5th and 97.5th percentile bounds for each locality-month Index P estimate.

## Data availability

The repository includes the processed climate data, locality and district lookup files, generated dengue and chikungunya Index P estimates, and the analytical outputs used in the study.

No synthetic or dummy data are included.

## Software

Climate extraction was performed in Google Earth Engine. The remaining analyses were conducted in R.

The required R packages are loaded within each script. The core workflow uses the `MVSE` package together with packages for data handling, plotting, date processing, and parallel computation.

## Citation

Please cite the associated manuscript when publication details become available.

## Contact

Amar Teeluck  
University of Queensland  
m.teeluck@student.uq.edu.au
