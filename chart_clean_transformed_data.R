##
##  Chart basic data series for perusal
##
##  Matthew DeHaven
##  2022 10 29
##
require(data.table)
require(lubridate)
require(ggplot2)

## Read in merged data
clean_data <- fread("./data/cleaned_data/merged_raw_data.csv")
tfmd_data <- fread("./data/cleaned_data/transformed_data.csv")

clean_data[, date := ymd(date)]
tfmd_data[, date := ymd(date)]

## Clean data plots
clean_plot <- 
  clean_data |>
  melt(id = "date") |>
  ggplot(aes(
    x = date,
    y = value
  )) +
  geom_line() + 
  facet_wrap(
    vars(variable),
    scales = "free_y",
    ncol = 1
  )


## Transformed data plots
tfmd_plot <- 
  tfmd_data |>
  melt(id = "date") |>
  ggplot(aes(
    x = date,
    y = value
  )) +
  geom_line() + 
  facet_wrap(
    vars(variable),
    scales = "free_y",
    ncol = 1
  )


## Save plots out to PDF for viewing on GitHub
ggsave("./output/data_charts/clean_data_charts.pdf", clean_plot, height = 9, width = 6.5, paper = "letter")
ggsave("./output/data_charts/transformed_data_charts.pdf", tfmd_plot, height = 9, width = 6.5, paper = "letter")

