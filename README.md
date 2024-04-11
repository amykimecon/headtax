# headtax

## Scripts (code folder)
- _main.R: This is the main analysis file, which imports required packages, initializes paths and variables, and runs all dependencies and analysis scripts.
- helper.R: This file contains helper functions used in other scripts

### 0_dataclean
- can_census_clean.R: This file cleans and compiles various years of raw Canadian census data
- register_clean.R: This file cleans Chinese Register data
- us_census_clean.R: DEPRECATED

### 1_descriptives
- descriptive_analysis.R: This file produces initial descriptives, including summary statistics and raw graphs

### 2_analysis
- inflow.R: This file produces analysis tables & graphs for Section 4 of the paper (effects of the head tax on immigrant inflow)
- selection.R: This file produces analysis tables & graphs for Section 6 of the paper (effects of the head tax on selection into immigration)
