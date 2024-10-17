# swts_ffip

This repository contains the code and resources for the analysis of SWTS II using various occupancy models. The analysis includes covariate processing and visualization of results to assess species distribution and threat patterns in relation to biophysical and anthropogenic factors.

**Note:** This repository is for exercise purposes only. The official analysis will be conducted at a later stage.

## Project Overview

The goal of this repository is to provide a framework for analyzing species occupancy using two statistical approaches:
1. **Maximum Likelihood Estimation (MLE) approach** located in `02_script/01_MLE`.
2. **Bayesian approach** located in `02_script/01_jags`.

Both approaches utilize the same dataset but require different data preparation methods.
- The **MLE approach** uses the `rpresence` package.
- The **Bayesian approach** uses `JAGS`.

## Data

The analysis is based on:
- **Detection histories**: Includes species detection histories and threat detection data.
- **Covariates**: Biophysical (e.g., land cover, elevation) and anthropogenic (e.g., distance to roads, human population density) covariates for each survey site.

**Note:** The dataset is restricted and only accessible to appointed members.
