# heatwave_dementia
Collection of data sources and scripts for "Heatwave and Dementia: A systematic analysis for the Global Burden of Disease Study 2019"

Chen, Y., Luo, Y., Yu, L., Zheng, X. & Luo, Q. Heatwave and dementia: a systematic analysis for the Global Burden of Disease Study 2019. bmjph 3, e002172 (2025).

## Data sources
We mainly derived original data from these databases:

### 1. Dementia data
We obtained the data for dementia from the Global Burden of Disease 2019 (GBD2019) study via the Global Health Data Exchange (GHDx; https://ghdx.healthdata.org), which includes age-standardized and all ages incidence and prevalence for dementia from 1990 to 2019 as well as incidence rate of dementia for 15 different groups of age and sex. We also derived the mortality data for dementia from the GBD.

### 2. Temperature and population data
We obtained the daily temperatures for these countries and territories using the ERA5 database, which is a gridded reanalysis dataset produced by the European Centre for Medium-Range Weather Forecasts with a 1° × 1° spatial resolution on the Google Earth Engine (https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_DAILY#description).

We derived the gridded daily temperatures at 1km resolution for the United Kingdom from HadUK-Grid Gridded Climate Observations (https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/haduk-grid).

To apply the population-weighted methods to the heatwave measures, we also obtained the estimated numbers of people (per 30-arc-second grid cell) from the 2010 Census on Google Earth Engine (https://developers.google.cn/earth-engine/datasets/catalog/CIESIN_GPWv411_GPW_Basic_Demographic_Characteristics?hl=en).

### 3. GDP data, population, and GNI indices
The World Bank provides the annual per capita GDP, GNI Index and population of countries around the world from 1960 to 2021 (https://databank.worldbank.org/).

GDP by states in United States was derived from the U.S. Bureau of Economic Analysis (https://www.beu.gov).

### 4. Nuisance variables
We also obtained the national gender and age structures from the World Population Review (https://worldpopulationreview.com/).

### 5. UK Biobank dataset
For individual-level validation, we utilized the home location and diagnosis data of dementia from UK Biobank (www.ukbiobank.ac.uk).

## Dataset
We constructed a Dementia-heatwave dataset spanning 153 countries from 1990 to 2019, stored in the "data" folder (https://github.com/Reese-Chen/heatwave_dementia/tree/main/data), which includes the data used for subsequent analysis.

## Scripts
All necessary scripts should be in the "code" file (https://github.com/Reese-Chen/heatwave_dementia/tree/main/code). Scripts for analysis are included in the file "code for analysis", and scripts for data extraction and index calculation are included in the file "data extraction and prepocessing". 

Scripts were developed using R and Python.
