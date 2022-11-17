##
##  Pull S&P data from FRED
##
##  Matthew DeHaven
##  2022 11 11
##
require(data.table)
require(lubridate)
require(fredr)

## You can get your own API key from FRED and replace it here
apiKey <- readChar("./data/FRED_API_KEY.txt", file.info("./data/FRED_API_KEY.txt")$size)
fredr::fredr_set_key(apiKey)

##
fred_data <- fredr::fredr("SP500", frequency = "d", observation_start = ymd("2020-01-01")) |> as.data.table()
fred_data[, date := ymd(date)]

wrds_data <- fread("./data/raw_data/28fsxnzwqukbjy0w.csv")
wrds_data[, caldt := ymd(caldt)]


comb_data <- merge( wrds_data, fred_data[,.(date, value)], by.y = "date", by.x = "caldt", all = T)
comb_data[is.na(spindx), spindx := value]
comb_data[, value := NULL]

## Save Back to Disk
fwrite(comb_data, "./data/raw_data/sp_data_combined.csv")
