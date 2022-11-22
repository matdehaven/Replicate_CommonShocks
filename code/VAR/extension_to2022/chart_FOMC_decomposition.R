##
##  Replicate VAR analysis from "Common Shocks..." paper
##  Use their time sample
##
##  Matthew DeHaven
##  2022 11 10
##
require(data.table)
require(lubridate)
require(ggplot2)
require(sandwich)

hd_data <- readRDS("./data/VAR_data/extension_to2022//HistoricalDecomposition_data.RDS")
FOMC <- fread("./data/raw_data/FOMC_dates.csv")
FOMC[, enddate := ymd(end_date)]
FOMC_dates <- FOMC[year(enddate) >= 1994 & year(enddate) <= 2017 & scheduled == 1, enddate]

## Create Dummy for the FOMC days
hd_data[, date := ymd(date)]
hd_data[, FOMC := fifelse(date %in% FOMC_dates, 1, 0)]

## Trim data to just periods we have FOMC dates for (should update data list)
hd_data <- hd_data[date >= ymd("1994-01-01")]
hd_data <- hd_data[year(date) <= max(year(ymd(FOMC_dates)))]
hd_data <- dcast(hd_data, variable + date + Overall + FOMC ~ decomp)
hd_data[, rp := pp + pm]

##
vars <- c("sp", "yc2", "yc5", "yc10")
ss <- c("Overall", "g", "m", "pp", "pm", "rp")
reg_result <-
  rbindlist(lapply(vars, function(v){
    rbindlist(lapply(ss, function(s){
      model <- lm(paste0(s, " ~ FOMC"), data = hd_data[variable == v])
      cov <- vcovHC(model)
      tt <- -qt(c(0.025,0.975),summary(model)$df[2])
      se <- sqrt(diag(cov))
      ci <-coef(model) + se %o% tt
      
      data.table(
        var = v,
        shock = s,
        beta = c("Int", "V1"),
        coef = coef(model),
        low_conf = ci[2,1],
        high_conf = ci[2,2]
      )
    }))
  }))


## Plot the Beta Coefficients for each Variable and Shock
## Need to extend FOMC days to 2017
## Fancy standard errors
## Combined risk premium shock?
reg_result[beta == "V1"] |>
  ggplot(aes(
    x = shock
  )) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(
    x = shock, xend = shock,
    y = low_conf, yend = high_conf
  )) +
  geom_point(aes(
    y = coef
  )) +
  facet_wrap(vars(var), scales = "free_y") +
  scale_x_discrete(limits = c("Overall","g", "m", "pp", "pm", "rp"), )


