##
##  Read in the raw_data, clean files, merge into one daily dataset
##
##  Matthew DeHaven
##  2022 10 29
##
require(data.table)
require(lubridate)

## raw_data file paths
## Filenames from download may change, the relative path is set
yc_file <- "./data/raw_data/feds200628.csv"
sp_file <- "./data/raw_data/28fsxnzwqukbjy0w.csv"
epu_file <- "./data/raw_data/All_Daily_Policy_Data.csv"


## Read in Yield Curve data
yc_data <- fread(yc_file, skip = 9)
yc_data[, date := ymd(Date)][, Date := NULL]

## Grab only the yield curve series needed
series <- c("date", "SVENY02", "SVENY05", "SVENY10")
new_names  <- c("date", "yc2", "yc5", "yc10")
yc_data <- yc_data[, ..series]
setnames(yc_data, series, new_names)

## 10-year values only start in 1971-08-16, so trim out NAs
yc_data <- na.omit(yc_data)


## Read in Stock Price data
sp_data <- fread(sp_file)
sp_data[, date := ymd(caldt)][, caldt := NULL]
setnames(sp_data, old = "spindx", new = "sp")
sp_data <- na.omit(sp_data)


## Read in Economic Policy Uncertainty data
epu_data <- fread(epu_file)
epu_data[, date := ymd(paste0(year, "-", month, "-", day))][, c("year","month","day") := NULL]
setnames(epu_data, old = "daily_policy_index", new = "epu")


## Merge data together
data <- yc_data |>
  merge(sp_data, by = "date", all = T) |>
  merge(epu_data, by = "date", all = T)


## Write to disk
fwrite(data, "./data/cleaned_data/merged_raw_data.csv")
