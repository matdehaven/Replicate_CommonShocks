################################################################################
# Pull FOMC Dates
#
# Pull and Parse FOMC meeting dates from Fed website
#
# Code originally started from https://github.com/returnandrisk/r-code
# But that code no longer worked, so rewritten in rvest
#
#
# Matthew DeHaven
# 2020 11 11
################################################################################
require(rvest)
require(stringr)

print("Getting FOMC dates for recent years...")

# get and parse web page content                                            
webpage <- rvest::session("http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm")
nodeset <- rvest::html_nodes(webpage, xpath = "//div[contains(@class, 'row fomc-meeting')]")

# iterate through nodeset and parse dates
years <- rvest::html_elements(webpage, xpath = "//div[contains(@class, 'panel-default')]")
length(years)

recent_FOMC <- 
  Reduce(rbind, lapply(1:length(years), function(i){
    year = rvest::html_text(rvest::html_nodes(years[[i]], xpath = ".//div[contains(@class, 'panel-heading')]" ))
    months = rvest::html_text(rvest::html_nodes(years[[i]], xpath = ".//div[contains(@class, 'fomc-meeting__month')]"))
    days = rvest::html_text(rvest::html_nodes(years[[i]], xpath = ".//div[contains(@class, 'fomc-meeting__date')]"))
    data.frame(
      year = year,
      month = months,
      day = days
    )
  }))

print("Getting FOMC dates for recent years...")

# get and parse web page content                                            
webpage <- rvest::session("http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm")
nodeset <- rvest::html_nodes(webpage, xpath = "//div[contains(@class, 'row fomc-meeting')]")

# iterate through nodeset and parse dates
years <- rvest::html_elements(webpage, xpath = "//div[contains(@class, 'panel-default')]")
length(years)

recent_FOMC <- 
  Reduce(rbind, lapply(1:length(years), function(i){
    year = rvest::html_text(rvest::html_nodes(years[[i]], xpath = ".//div[contains(@class, 'panel-heading')]" ))
    months = rvest::html_text(rvest::html_nodes(years[[i]], xpath = ".//div[contains(@class, 'fomc-meeting__month')]"))
    days = rvest::html_text(rvest::html_nodes(years[[i]], xpath = ".//div[contains(@class, 'fomc-meeting__date')]"))
    data.frame(
      year = year,
      month = months,
      day = days
    )
  }))

## Parse Recent Dates
recent_FOMC$year <- str_extract(recent_FOMC$year, "\\d{4}")
recent_FOMC$meeting_type  <- str_extract(recent_FOMC$day, "(?<=\\().*(?=\\))")
recent_FOMC$day <- str_remove(recent_FOMC$day, "\\(.*\\)")
recent_FOMC[is.na(recent_FOMC$meeting_type),]$meeting_type <- "meeting"

recent_FOMC$starred  <- str_detect(recent_FOMC$day, "\\*")
recent_FOMC$day <- str_trim(str_remove(recent_FOMC$day, "\\*"))

## Split Months
recent_FOMC$s_mon <- str_extract(recent_FOMC$month, "^\\w*" )
recent_FOMC$e_mon <- str_extract(recent_FOMC$month, "\\w*$" )

## Split Days
recent_FOMC$s_day <- str_extract(recent_FOMC$day, "^\\d*" )
recent_FOMC$e_day <- str_extract(recent_FOMC$day, "\\d*$" )

## Create Dates
recent_FOMC$start_date <- ymd(paste0(recent_FOMC$year, "-", recent_FOMC$s_mon, "-", recent_FOMC$s_day))
recent_FOMC$end_date <- ymd(paste0(recent_FOMC$year, "-", recent_FOMC$e_mon, "-", recent_FOMC$e_day))
recent_df <- recent_FOMC[,c("start_date", "end_date", "meeting_type")]


## Historical Dates
begyear = 1936
endyear = 2016
print(paste("Getting FOMC dates from", begyear, "to", endyear))

# iterate through each year                                            
historical_FOMC <- 
  Reduce(rbind, lapply(begyear:endyear, function(year){ 
    print(year)
    # get and parse web page content 
    webpage <- rvest::session(paste0("http://www.federalreserve.gov/monetarypolicy/fomchistorical", year, ".htm"))
    nodeset <- rvest::html_nodes(webpage, xpath = "//div[@id='article']")
    
    meetings <- rvest::html_nodes(nodeset, xpath = ".//div[contains(@class, 'panel-heading')]")
    if(length(meetings)==0) meetings <- rvest::html_nodes(nodeset, xpath = ".//h5[contains(@class, 'panel-heading')]")
    
    meetings_text <- rvest::html_text(meetings)  
    
    data.frame(
      year = year,
      meeting = meetings_text
    )
  }))


## Trim spaces and returns
historical_FOMC$meeting <- str_trim(gsub("\\r|\\n", "", historical_FOMC$meeting))
historical_FOMC$meeting <- str_trim(gsub(paste0(begyear:endyear, collapse = "|"), "", historical_FOMC$meeting))

all_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

## Get Months
months <- str_extract_all(historical_FOMC$meeting, paste0(all_months, collapse = "|"))
months_df <- Reduce(rbind, lapply(months, function(x){
  if(length(x)==2){return(data.frame(s_mon = x[[1]], e_mon = x[[2]]))}
  else return(data.frame(s_mon = x[[1]], e_mon = x[[1]]))
  }))

## Get Days
days <- str_extract_all(historical_FOMC$meeting, "\\d{1,2}[^\\d]{0,1}")
days_df <- Reduce(rbind, lapply(days, function(x){
  if(length(x)==2){return(data.frame(s_day = x[[1]], e_day = x[[2]]))}
  else return(data.frame(s_day = x[[1]], e_day = x[[1]]))
}))
days_df$s_day <- str_remove_all(days_df$s_day, "-| ")
days_df$e_day <- str_remove_all(days_df$e_day, "-| ")

## Get Meeting Type
historical_FOMC$meeting_type <- str_trim(gsub(paste0(c(all_months, "\\d", "-|/|,|and|\\(|\\)"), collapse = "|"),"", historical_FOMC$meeting))
historical_FOMC$meeting_type <- str_to_lower(historical_FOMC$meeting_type)
historical_FOMC$meeting_type <- str_replace(historical_FOMC$meeting_type, "conference calls", "conference call")


## Merge
historical_df <- cbind(historical_FOMC[,c("year", "meeting_type")], days_df, months_df)
historical_df$start_date <- ymd(paste0(historical_df$year, "-", historical_df$s_mon, "-", historical_df$s_day))
historical_df$end_date <- ymd(paste0(historical_df$year, "-", historical_df$e_mon, "-", historical_df$e_day))
historical_df <- historical_df[, c("start_date", "end_date", "meeting_type")]

## Fix September 15, 2003 meeting, which is listed as 2 meetings but just covered 2 days
historical_df[historical_df$s_date == ymd("2003-09-15"),]$end_date <- ymd("2003-09-16")
historical_df <- historical_df[historical_df$start_date != ymd("2003-09-16"),]


######
## Combine Historical and Recent
FOMC <- rbind(historical_df, recent_df)
FOMC$scheduled <- ifelse(FOMC$meeting_type=="meeting", 1, 0)
data.table::fwrite(FOMC, "./data/raw_data/FOMC_dates.csv")


f1 <- fread("./data/raw_data/FOMC Dates from 1936.csv")
f1[, begdate := ymd(begdate)]
f1[, enddate := ymd(enddate)]

FOMC <- as.data.table(FOMC)
comb <- merge(
  FOMC,
  f1,
  by.x = c("start_date", "end_date"),
  by.y = c("begdate", "enddate"),
  all = T
)

