##
##  Replicate VAR analysis from "Common Shocks..." paper
##  Use their time sample
##
##  Matthew DeHaven
##  2022 10 29
##
require(data.table)
require(lubridate)

require(vars)
require(svars)

## The time sample used in the paper
date_range <- ymd(c("1983-01-01", "2017-12-31"))

## Read in the data
data <- fread("./data/cleaned_data/transformed_data.csv")
data[, date := ymd(date)]

## Subset to same time period and drop EPU
## Omit some NAs because we have EPU data on weekends
series <- c("yc2", "yc5", "yc10", "sp")
X <- as.matrix(na.omit(data[date %between% date_range, ..series]))

## Demean the data
X <- X - apply(X, 2, mean) 

## Find the BIC for lag length (should be 1)
## SC(n) is the BIC number
IC <- vars::VARselect(X)$selection
print(IC)

## Set up reduced form VAR
myvar <- vars::VAR(X, type = "const", ic = "SC")



