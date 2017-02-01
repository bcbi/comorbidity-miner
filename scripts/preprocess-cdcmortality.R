# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
#                                         |
# credit to yuangqing liu                 |
# brown univeristy school of public health|
# ----------------------------------------|


library(readr)
library(magrittr)
library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(tidyr)
library(scales)
library(knitr)


# Function for downloading and parsing data:
CDC_parser <- function(year) {
  
  # Set up files
  year <- as.character(year)
  all_cdc_mort_name <- paste0("cdc_mort_", year)
  all_cdc_mort_save <- paste0("data/CDC-vitals", "all_cdc_mort_", year,".RData")
  url <- paste0("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort", year, "us.zip")
  
  
  # Layout of data
  layout <- fwf_widths(c(19,1,40,2,1,1,2,2,1,4,1,2,2,2,2,1,1,1,16,4,1,1,1,1,34,1,1,4,3,1,3,3,2,1,281,1,2,1,1,1,1,33,3,1,1),
                       col_names = c("drop1", "res_status", "drop2", "education_89", "education_03", "education_flag", "month", 
                                     "drop3", "sex", "detail_age", "age_flag", "age_recode", "age_recode2", "age_group", 
                                     "age_infant", "death_place", "marital", "day_of_week", "drop4", "data_year", "at_work", 
                                     "death_manner", "burial", "autopsy", "drop5", "activity", "injury_place", 
                                     "underlying_cause", "cause_recode358", "drop6", "cause_recode113", "cause_recode130", 
                                     "cause_recode39", "drop7", "multiple_causes", "drop8", "race", "race_bridged", "race_flag", 
                                     "race_recode", "race_recode2", "drop9", "hispanic", "drop10", "hispanic_recode"))
  
  temp <- tempfile()
  download.file(url, temp, quiet = T)
  
  # Read in data
  raw_file <- read_fwf(unzip(temp), layout)
  
  # Drop empty fields
  raw_file <- raw_file %>%
    select(-contains("drop"))
  
  # Save 'all_deaths' file
  assign(eval(all_cdc_mort_name), raw_file)
  save(list = all_cdc_mort_name, file = all_cdc_mort_save)
}

CDC_parser(2015)
load("all_cdc_mort_2015.RData")
CDC_parser(2014)
load("all_cdc_mort_2014.RData")
CDC_parser(2013)
load("all_cdc_mort_2013.RData")
CDC_parser(2012)
load("all_cdc_mort_2012.RData")
CDC_parser(2011)
load("all_cdc_mort_2011.RData")


# write mortality data to csv: run only if you want to have a copy of the data in CSV format
# write.csv(cdc_mort_2015, file = "~/cdc_mort_2015.csv") 
# write.csv(cdc_mort_2015, file = "~/cdc_mort_2014.csv") 
# write.csv(cdc_mort_2015, file = "~/cdc_mort_2013.csv") 
# write.csv(cdc_mort_2015, file = "~/cdc_mort_2012.csv") 
# write.csv(cdc_mort_2015, file = "~/cdc_mort_2011.csv") 
# read in data(2011-2015) from csv
# cdc_mort_2011 <- fread("~/cdc_mort_2011.csv",stringsAsFactors = FALSE)
# cdc_mort_2012 <- fread("~/cdc_mort_2012.csv",stringsAsFactors = FALSE)
# cdc_mort_2013 <- fread("~/cdc_mort_2013.csv",stringsAsFactors = FALSE)
# cdc_mort_2014 <- fread("~/cdc_mort_2014.csv",stringsAsFactors = FALSE)
# cdc_mort_2015 <- fread("~/cdc_mort_2015.csv",stringsAsFactors = FALSE)


# function to split multiple causes into entity axis and record axis, and unnests the record axis, so one condition per row
split_axis <- function(df) {
  df <- df %>%
    mutate(entity_axis = sapply(strsplit(df$multiple_causes, split = '\\s\\d{2}\\s', perl = TRUE), function(x) (x[1])),
           record_axis = sapply(strsplit(df$multiple_causes, split = '\\s\\d{2}\\s', perl = TRUE), function(x) (x[2])) ) %>%
    unnest(record_axis) %>%
    filter(!(record_axis) == "")  
}

cdc_mort_2011 <- split_axis(cdc_mort_2011)
cdc_mort_2012 <- split_axis(cdc_mort_2012)
cdc_mort_2013 <- split_axis(cdc_mort_2013)
cdc_mort_2014 <- split_axis(cdc_mort_2014)
cdc_mort_2015 <- split_axis(cdc_mort_2015)


# combine data to single data frame
cdc_mort_data <- rbind(cdc_mort_2011,cdc_mort_2012,cdc_mort_2013,cdc_mort_2014,cdc_mort_2015)
write.csv(cdc_mort_data,"~/cdc_mort_2011-2015.csv")

glimpse(cdc_mort_data)
