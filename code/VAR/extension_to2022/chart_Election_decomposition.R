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

hd_data <- readRDS("./data/VAR_data/replication//HistoricalDecomposition_data.RDS")

data <- fread("./data/cleaned_data/transformed_data.csv")
data[, wday := lubridate::wday(date)]

## Subset to First Tuesdays in November, even years
all_dates <- seq.Date(from = ymd("1980-01-01"), to = Sys.Date(), by = "day")
nov_tues_even <- data.table(date = all_dates[lubridate::wday(all_dates) == 3 & year(all_dates) %% 2 == 0 & month(all_dates) == 11])
election_days <- nov_tues_even[, .(date = min(date)), by = .(year(date))]
election_days[day(date) == 1, date := date + 7]

## Mark data for election days
hd_data[, election := 0]
hd_data[date %in% (election_days[,date]), election := 1]
hd_data[, pres_election := 0]
hd_data[election == 1 & year(date) %% 4 == 0, pres_election := 1]


hd_data[election == 1, date] |> unique()
hd_data[pres_election == 1, date] |> unique()

## Create Dummy for the FOMC days
hd_data[, date := ymd(date)]

## 
hd_data <- dcast(hd_data, variable + date + Overall + election ~ decomp)
hd_data[, rp := pp + pm]

##
vars <- c("sp", "yc2", "yc5", "yc10")
ss <- c("Overall", "g", "m", "pp", "pm", "rp")
reg_result <-
  rbindlist(lapply(vars, function(v){
    rbindlist(lapply(ss, function(s){
      model <- lm(paste0(s, " ~ election"), data = hd_data[variable == v])
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


