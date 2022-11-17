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
source("./code/pull_data/clean_raw_data.R")
source("./code/pull_data/transform_clean_data.R")


## Data Charting
source("./code/pull_data/chart_clean_transformed_data.R")


## VAR Analysis
source("./code/VAR/replication/replicate_VAR_analysis.R")

## VAR Shock Charts
source("./code/VAR/replication/replicate_VAR_charts.R")

## Historical Variance Decomposition
source("./code/VAR/replication/create_historical_variance_decomposition.R")
source("./code/VAR/replication/chart_FOMC_decomposition.R")