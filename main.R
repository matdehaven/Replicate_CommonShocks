##
##  Main script of the project, run script to rerun whole project
##  Takes data/raw_data folder as input, recreates all of
##  /data/cleaned_data
##  /output/
##
##  Matthew DeHaven
##  2022 10 29
##


## Data Processing
source("./code/clean_raw_data.R")
source("./code/transform_clean_data.R")


## Data Charting
source("./code/chart_clean_transformed_data.R")


## VAR Analysis
