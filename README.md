# Light Mode PG-CATA analysis code

This folder contains the final R scripts associated with the Light Mode manuscript. It is the code package intended for transfer to a public GitHub repository after the manuscript authors have completed their final review.

## Contents

### 01_PHASE1_VALIDATION

- `01_SESSION_PROCESSING/`: processes the anonymized T1 and T2 raw response workbooks and creates sample-level aroma fingerprints.
- `02_STATISTICAL_ANALYSIS/`: reproduces the Bray-Curtis analysis, category-percentage changes, Shannon entropy and Jaccard agreement analysis, time-point mixed-effects models and forest plot, GLMM results table, and consensus-stability figure.

Run the two session-processing scripts first. Then run the six statistical-analysis scripts in numerical order. The statistical scripts expect the Phase 1 data-directory structure supplied in `02_ANALYSIS_AND_SOURCE_DATA/01_VALIDATION_T1_T2`.

### 02_BREEDING_POPULATION_DEPLOYMENT

- `01_SESSION_PROCESSING/`: processes one deployment session using the final nine aroma categories.
- `02_COMBINE_SESSIONS/`: combines the session-level percentage outputs.
- `03_POPULATION_ANALYSIS/`: creates the breeding-population heatmap and correspondence analysis.
- `04_ANCHOR_REPRODUCIBILITY/`: analyzes the repeated anchor genotypes.

Run these folders in numerical order. The scripts expect the deployment data-directory structure supplied in `02_ANALYSIS_AND_SOURCE_DATA/02_DEPLOYMENT_86_SAMPLES`.

### 03_REUSABLE_LIGHT_MODE_TOOL

Contains the simplified supplementary R script intended for future users who want to analyze a new Light Mode PG-CATA session. Its companion beginner guide is `Supplementary_Guide_Run_LM` in the manuscript submission package.

## Important publication notes

- The manuscript analyses use nine final GC-O-supported aroma categories.
- The category labels include `Earthy/Musty/Moldy` and `Resinous/Pine-like`.
- All data uploaded with this code must be anonymized. Do not upload any private participant re-identification key.
- Before public release, add the final article citation, repository license, author/contact information, and a versioned release corresponding to the accepted manuscript.

## Software

The scripts are written in R. Required packages are declared near the beginning of each script. Package versions and the R version should be recorded from the final verified analysis environment before publication.
