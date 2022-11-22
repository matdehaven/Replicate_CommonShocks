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

myvar <- readRDS("./data/VAR_data/extension_addEPU/estimated_VAR.RDS")
rot_files <- list.files("./data/VAR_data/extension_addEPU/rotations/", full.names = T, pattern = "csv")
rots <- lapply(rot_files, function(f){as.matrix(fread(f))})
rots_dt <- readRDS("./data/VAR_data/extension_addEPU/rotations_long_MT.RDS")
omega_summ <- readRDS("./data/VAR_data/extension_addEPU/structural_shock_summaries.RDS")

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
shock_names <- c("g", "m", "pp", "pm", "epu")

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
saveRDS(hd_data, "./data/VAR_data/extension_addEPU/HistoricalDecomposition_data.RDS")

## Plot the time series of the Historical Decomposition
hd_data[year(date) >= 2020] |> 
  ggplot(aes(
    x = date,
    y = value,
    fill = decomp
  )) +
  geom_col() + 
  # geom_line(aes(y = Overall)) +
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

