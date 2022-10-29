##
##  Take merged data, perform transformations before analysis (logs, changes, etc.)
##
##  Matthew DeHaven
##  2022 10 29
##
require(data.table)

## Read in merged data
data <- fread("./data/cleaned_data/merged_raw_data.csv")

## Yield Curve variables should be as day-to-day changes
data[, yc2 := yc2 - shift(yc2, 1, type = "lag")]
data[, yc5 := yc5 - shift(yc5, 1, type = "lag")]
data[, yc10 := yc10 - shift(yc10, 1, type = "lag")]

## Stock Price data should be day-to-day changes of log prices (so eq. pct change)
data[, sp := log(sp) - log(shift(sp, 1, type = "lag"))]

## EPU should also be changes
data[, epu := epu - shift(epu, 1, type = "lag")]

## Save transformed data to disk
fwrite(data, "./data/cleaned_data/transformed_data.csv")
