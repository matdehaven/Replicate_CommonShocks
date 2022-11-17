##
##  Replicate VAR analysis from "Common Shocks..." paper
##  Use their time sample
##
##  Matthew DeHaven
##  2022 11 10
##
require(data.table)
require(lubridate)
require(vars)
require(svars)
require(ggplot2)

myvar <- readRDS("./output/VAR_rotations/reduced_form_VAR.RDS")
rots <- readRDS("./output/VAR_rotations/successful_rotations.RDS")
rots_dt <- readRDS("./output/VAR_rotations/rotations_long_MT.RDS")
omega_summ <- readRDS("./output/VAR_rotations/structural_shock_summaries.RDS")

## Take the VAR we had originally calculated
var <- myvar$var

## Construct the SVAR object for the package 'svars'
svar <- id.chol(var)

## Check that its Choleskey matches the one we found
all(round(svar$B,4) == round(myvar$P,4))

## Replace the Choleskey with our MT sign-restricted matrix
svar$B <- rots[[rots_dt$MT_i]]

## Use the 'svars::hd' function to get the historical decomposition for each series
s <- 1:ncol(myvar$u)
series <- rownames(myvar$P)
shock_names <- c("g", "m", "pp", "pm")

hd_data <- 
  rbindlist(lapply(s, function(i){
    hd_dt <- hd(svar, series = i)$hidec[,-c(1,2)] |> as.data.table()
    names(hd_dt) <- c("Overall", shock_names)
    hd_dt[, variable := series[i]]
    hd_dt <- cbind(hd_dt, date = myvar$analysis_data[-1,date])
    hd_dt_l <- hd_dt |> melt(id = c("variable", "date", "Overall"), variable.name = "decomp")
    return(hd_dt_l)
  }))

## Save HD to disk
saveRDS(hd_data, "./output/VAR_rotations/HistoricalDecomposition_data.RDS")

## Plot the time series of the Historical Decomposition
hd_data |> 
  ggplot(aes(
    x = date,
    y = value,
    fill = decomp
  )) +
  geom_col() + 
  geom_line(aes(y = Overall)) +
  facet_wrap(vars(variable))

## Summarize, how much variance does each shock explain for each variable?
hd_data[, pct_var := abs(value) / sum(abs(value)), by = .(variable, date)]
hd_data[,.(pct_var_mean =  mean(pct_var)), by = .(variable, decomp)]

hd_data[,.(pct_var_mean = mean(pct_var)), by = .(variable, decomp)] |>
  ggplot(aes(
    x = variable,
    y = pct_var_mean,
    fill = decomp
  )) +
  geom_col(position = position_dodge())

hd_data[,.(sd_O = sd(Overall), sd_decomp = sd(value)), by = .(variable, decomp)]
hd_data[,.(sd_O = sd(Overall), sd_decomp = sd(value)), by = .(variable, decomp)][,.(sd_0 = mean(sd_O), sd_s = sum(sd_decomp)), by = .(variable)]

t <- hd_data[,.(O = sum(abs(Overall)), val = sum(abs(value))), by = .(variable, decomp)]
t[, O2 := sum(val), by = .(variable)]
t[, pct_var := val / O2]
t |>
  ggplot(aes(
    x = variable,
    y = pct_var,
    fill = decomp
  )) +
  geom_col(position = position_dodge())


hd_dt <- hd(svar, series = 1)$hidec[,-c(1,2)] |> as.data.table()
names(hd_dt) <- c("Overall", shock_names)
hd_dt[, variable := series[1]]
hd_dt <- cbind(hd_dt, date = myvar$analysis_data[-1,date])
hd_dt_l <- hd_dt |> melt(id = c("variable", "date"), variable.name = "decomp")


require(svars)
svar <- svars::id.chol(var)
test2 <- hd(svar, series = 2)
plot(test2)

svar2 <- copy(svar)
svar2$B <- rots[[1]]
test3 <- hd(svar2, series = 2)
test3$hidec[,c(1,4:7)] |>  as.data.table() |>
  melt(id = "V1") |>
  ggplot(aes(
    x = V1,
    y = value,
    fill = variable
  )) +
  geom_col()
