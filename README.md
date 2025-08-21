# Overview

Data and code to supplement Shapiro et al. XXXX (in prep).

*INSERT CITATION*

The contents of this repo illustrate the use of the [`MerkleLabGIS`](https://github.com/jmerkle1/MerkleLabGIS) package coupled with pronghorn monitoring data collected by the Wyoming Game and Fish Department to create a habitat-suitability map for pronghorn parturition habitat based on a resource selection function (RSF) fit using Random Forests.

# Contents
## Scripts
- **script_01_prepCovars.R**:  Prep covariate rasters using `MerkleLabGIS` package
- **script_02_prepPoints.R**:  Prep presence and (pseudo)absence points
- **script_03_fitRSF.R**:  Fit RSF using Random Forests model
- **script_04_makeMap.R**:  Create predicted habitat suitability map

## Input Data
- **StudyArea_AllOwnership.kml**:  Spatial polygon - the study area, a portion of the Medicine Bow pronghorn herd unit
- **StudyArea_Public.kml**:  Spatial polygon - public lands within the study area
- **SurveyData_TransectLines.kml**:  Spatial lines -  transects surveyed during aerial survey of pronghorn
- **SurveyData_PronghornPoints.kml**:  Spatial points - approximate location of pronghorn groups (subject to GPS error of approximately +/- 300 m)

## Prepped Data
- **CovarRasters.rds**:  Raster stack - layer for each of 22 predictor variables used to model pronghorn habitat suitability, output of script 01
- **Pronghorn_PresAbs.rds**:  R object - spatial points of pronghorn group locations (n = 188) and available locations (pseudo-absence, n = 188), output of script 02
- **Pronghorn_RandomForests_Model.rds**:  R object - Random Forests RSF model fit to pronghorn data and covariates, output of script 03

## Output
- **RF_xxx.xxx**:  Various outputs indicating Random Forests model performance, output of script 03
- **Map.png**:  Map of pronghorn habitat suitability and pronghorn locations, output of script 04

![](Output/Map.png)

# Data Sensitivity
Pronghorn locations on privately owned land have been omitted due to data sensitivity and sharing restrictions.  Land ownership was determined using a GIS layer from the Bureau of Land Management (BLM) from 2022.

# License
Not defined.
