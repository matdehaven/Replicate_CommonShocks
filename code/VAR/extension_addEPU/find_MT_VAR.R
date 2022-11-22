##
##  Replicate VAR analysis from "Common Shocks..." paper
##  Use their time sample
##
##  Matthew DeHaven
##  2022 11 05
##
require(data.table)
require(lubridate)
require(vars)
require(ggplot2)

myvar <- readRDS("./data/VAR_data/extension_addEPU/estimated_VAR.RDS")
rot_files <- list.files("./data/VAR_data/extension_addEPU/rotations/", full.names = T, pattern = "csv")
rots <- lapply(rot_files, function(f){as.matrix(fread(f))})

## Only keep the first 1000 rotations
if(length(rots) > 1000) rots <- rots[1:1000]

## Calculate the 1000 structural shocks
omegas <- lapply(1:length(rots), function(i){
  rot_shocks <- data.table(i = i, myvar$u %*% solve(t(rots[[i]])))
  rot_shocks <- rbind(data.table(i = i, t(rep(0,5))), rot_shocks)
  names(rot_shocks) <- c("rot", "g", "m", "pp", "pm", "epu")
  rot_shocks <-  cbind(myvar$analysis_data[,.(date)], rot_shocks)
  return(rot_shocks)
})
omegas_data <- rbindlist(omegas)

## Identify the Median-Targetting Solution
rots_data <- rbindlist(lapply(1:length(rots), function(i){data.table(i = i, t(as.vector(rots[[i]])))} ))
rots_data_l <- melt(rots_data, id = "i")
rots_data_l[, med := median(value), by = .(variable)]
rots_data_l[, sd := sd(value), by = .(variable)]
rots_data_l[, err := ((value - med) / sd)^2]
i_min = rots_data_l[, .(err_sum = sum(err)), by = .(i)][err_sum == min(err_sum), i]

## Summarize the shocks into quintiles, mean
omegas_data_l <- melt(omegas_data, id = c("date", "rot"))
omegas_data_l[, cusum := cumsum(value), by = .(variable, rot)]

omegas_summ <- omegas_data_l[,.(
  MT = sum(ifelse(rot==i_min, cusum, 0)),
  min = min(cusum),
  pct10 = quantile(cusum, 0.1),
  pct25 = quantile(cusum, 0.25),
  mean = mean(cusum),
  med = median(cusum),
  pct75 = quantile(cusum, 0.75),
  pct90 = quantile(cusum, 0.9),
  max = max(cusum)
), by = .(date, variable)]

## Save out MT and shock summaries
saveRDS(list(rots_l = rots_data_l, MT_i = i_min), "./data/VAR_data/extension_addEPU/rotations_long_MT.RDS")
saveRDS(omegas_summ, "./data/VAR_data/extension_addEPU/structural_shock_summaries.RDS")


## Create Chart
omegas_summ |>
  ggplot(aes(
    x = date
  )) +
  geom_ribbon(aes(
     ymin = min,
     ymax = max
  ), fill = "gray80") +
  geom_ribbon(aes(
     ymin = pct10,
     ymax = pct90
  ), fill = "gray50") +
  geom_line(aes(
    y = MT, color = "MT"
  )) +
  geom_line(aes(
    y = med, color = "Median"
  )) +
  scale_color_manual(values = c(MT = "firebrick", Median = "black")) +
  facet_wrap(vars(variable), scales = "free_y") +
  theme_bw()

