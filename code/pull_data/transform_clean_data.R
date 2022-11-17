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
data[!is.na(yc2), yc2 := yc2 - shift(yc2, 1, type = "lag")]
data[!is.na(yc5), yc5 := yc5 - shift(yc5, 1, type = "lag")]
data[!is.na(yc10), yc10 := yc10 - shift(yc10, 1, type = "lag")]

## Stock Price data should be day-to-day changes of log prices (so eq. pct change)
# data[!is.na(sp), sp := log(sp) - log(shift(sp, 1, type = "lag"))]

## Actually, it is log stock returns, so the ratio of prices, then logged
data[!is.na(sp), sp := log(sp / shift(sp, 1, type = "lag"))]

## EPU should also be changes
data[!is.na(epu), epu := log(epu) - log(shift(epu, 1, type = "lag"))]

## Save transformed data to disk
fwrite(data, "./data/cleaned_data/transformed_data.csv")
