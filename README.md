# ASFV-risk-mapping
R script for the risk mapping report of ASFV virus

This repository contains the R script and associated input/output files used for the spatial risk mapping analysis of African swine fever virus (ASFV) distribution in Europe.

## Repository content

- `01_ASFV_risk_mapping_script.R`: main R script used to run the analysis
- `AUC_value_replicates.csv`: AUC values obtained for the different model replicates
- Environmental raster layers used as predictors
- Study area shapefiles
- Output figures generated from the BRT models

## Main methods

The analysis is based on:

- Boosted Regression Trees (BRT)
- Presence and pseudo-absence data
- Spatial cross-validation
- Environmental suitability modelling
- Risk mapping 

## Objective

The objective of this work is to model the potential distribution of ASFV in Europe and to identify areas presenting higher environmental suitability for virus presence.

## Outputs

The repository includes the main outputs used in the report, such as model performance indicators, relative influence of environmental variables, response curves, risk maps and uncertainty maps.

## Authors

Awatif Chaaer et Benissa Mahfoud Hajar
