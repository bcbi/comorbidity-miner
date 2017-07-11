# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
#                                         |
# credit to yuangqing liu                 |
# brown university school of public health|
# ----------------------------------------|



#------------------------------------------raw data------------------------------------------#
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

#CDC_parser(2015)
load("data/CDC-vitals/all_cdc_mort_2015.RData")
#CDC_parser(2014)
#load("all_cdc_mort_2014.RData")
#CDC_parser(2013)
#load("all_cdc_mort_2013.RData")
#CDC_parser(2012)
#load("all_cdc_mort_2012.RData")
#CDC_parser(2011)
#load("all_cdc_mort_2011.RData")


# write mortality data to csv: run only if you want to have a copy of the data in CSV format
# write.csv(cdc_mort_2015, paste0(raw_datadir, "CDC-vitals/cdc_mort_2015.csv"), row.names = F) 
# write.csv(cdc_mort_2014, paste0(raw_datadir, "CDC-vitals/cdc_mort_2014.csv"), row.names = F) 
# write.csv(cdc_mort_2013, paste0(raw_datadir, "CDC-vitals/cdc_mort_2013.csv"), row.names = F) 
# write.csv(cdc_mort_2012, paste0(raw_datadir, "CDC-vitals/cdc_mort_2012.csv"), row.names = F) 
# write.csv(cdc_mort_2011, paste0(raw_datadir, "CDC-vitals/cdc_mort_2011.csv"), row.names = F) 
# read in data(2011-2015) from csv
#cdc_mort_2011 <- fread(paste0(raw_datadir, "CDC-vitals/cdc_mort_2011.csv"), stringsAsFactors = FALSE)
# cdc_mort_2012 <- fread(paste0(raw_datadir, "CDC-vitals/cdc_mort_2012.csv"), stringsAsFactors = FALSE)
# cdc_mort_2013 <- fread(paste0(raw_datadir, "CDC-vitals/cdc_mort_2013.csv"), stringsAsFactors = FALSE)
# cdc_mort_2014 <- fread(paste0(raw_datadir, "CDC-vitals/cdc_mort_2014.csv"), stringsAsFactors = FALSE)
# cdc_mort_2015 <- fread(paste0(raw_datadir, "CDC-vitals/cdc_mort_2015.csv"), stringsAsFactors = FALSE)


#------------------------------------------------------------------------------------#

# function to split multiple causes into entity axis and record axis, and unnests the record axis, so one condition per row
split_axis <- function(df) {
  df <- df %>%
    mutate(entity_axis = sapply(strsplit(df$multiple_causes, split = '\\s\\d{2}\\s', perl = TRUE), function(x) (x[1])),
           record_axis = sapply(strsplit(df$multiple_causes, split = '\\s\\d{2}\\s', perl = TRUE), function(x) (x[2])) )  
}

cdc_mort_2015 <- split_axis(cdc_mort_2015)
#cdc_mort_2014 <- split_axis(cdc_mort_2014)
#cdc_mort_2013 <- split_axis(cdc_mort_2013)
#cdc_mort_2012 <- split_axis(cdc_mort_2012)
#cdc_mort_2011 <- split_axis(cdc_mort_2011)


# combine data to single data frame
#cdc_mort_data <- rbind(cdc_mort_2011,cdc_mort_2012,cdc_mort_2013,cdc_mort_2014,cdc_mort_2015)
#write.csv(cdc_mort_data,"~/cdc_mort_2011-2015.csv")


# create patient ID
cdc_mort_2015$ID <- seq.int(nrow(cdc_mort_2015))



#-------------------------------------------input data-----------------------------------------#
# separate underlying_cause from record_axis
cdc_temp <- cdc_mort_2015 %>%
  select(ID, underlying_cause, record_axis) %>%
  rowwise() %>%
  mutate(record_axis = sub(underlying_cause, "", record_axis),
         record_axis = str_trim(record_axis, side = 'both'))

# create admit_id and sort by ID and admit_id
cdc_temp1 <- cdc_temp %>%
  select(ID, icd_code = underlying_cause) %>%
  mutate(admit_id = 1)
cdc_temp2 <- cdc_temp %>%
  select(ID, icd_code = record_axis) %>%
  mutate(admit_id = 2)
cdc_temporal <- rbind(cdc_temp1, cdc_temp2) %>%
  arrange(ID, admit_id) %>%
  filter(icd_code != "") %>%
  mutate(icd_code = gsub(" ", ",", icd_code),
         icd_code = gsub(",,", ",", icd_code)) %>%
  select(ID, admit_id, icd_code)


# ---------------------- demographics ---------------------- #
res_status_codes <- data.frame(res_status = c(1:4),
                               res_status2 = c('resident','nonresident_intrastate','nonresident_interterritory','foreign_resident'))
education_89_codes <- data.frame(education_89 = c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','99'),
                                 education_892 = c('no_education','1','2','3','4','5','6','7','8',
                                                   '9','10','11','12','college1','college2','college3','college4','college5+','not_stated'))
education_03_codes <- data.frame(education_03 = c(1:9),
                                 education_032 = c('k8','9-12','highschool_ged','some_college','associate','bachelors','masters','phd','unknown'))
age_group_codes <- data.frame(age_group = c('01','02','03','04','05','06','07','08','09','10','11','12'),
                              age_group2 = c('infant','1-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+','NotStated'))
age_infant_codes <- data.frame(age_infant = c(NA,"09","18","11","13","16","14","02","15","07","03","22","12","01","04","10","06","17",
                                              "08","05","19","21","20"),
                               age_infant2 = c('NotInfantNotStated','days7-13','months7','days21-27','months2','months5','months4','hours1-23','months4','days5',
                                               'days1','months11','months1','under1hour','days2','days14-20','days4','months6','days6','days3','months8','months10','months9'))
race_codes <- data.frame(race = c("01","03","07","05","02","28","68","38","04","18","78","06","48","58"),
                         race2 = c('White','AmericanIndian','Filipino','Japanese','Black','Korean','OtherAsianPacificIslander',
                                   'Samoan','Chinese','AsianIndian','CombinedAsianPacificIslander','Hawaiian','Vietnamese','Guamanian'))
hispanic_recode_codes <- data.frame(hispanic_recode = c(1:9),
                                    hispanic_recode2 = c('Mexican','PuertoRican','Cuban','CentralSouthAmerican','OtherUnknown',
                                                         'NonHispanicWhite','NonHispanicBlack','NonHispanicOther','HispanicUnknown'))

cdc_demographics <- cdc_mort_2015 %>%
  select(ID, res_status, education_89, education_03, sex, age_group, age_infant, marital, race, hispanic_recode) %>%
  left_join(res_status_codes) %>%
  left_join(education_89_codes) %>%
  left_join(education_03_codes) %>%
  left_join(age_group_codes) %>%
  left_join(age_infant_codes) %>%
  left_join(race_codes) %>%
  left_join(hispanic_recode_codes) %>%
  select(ID, res_status2, education_892, education_032, sex, age_group2, age_infant2, marital, race2, hispanic_recode2)


# write temporal format data and mimic demographics to ./data/input/ folder (read into memory in global.R)
f_name_t <- paste0(csv_dir, 'cdc_temporal.txt')
write.table(cdc_temporal, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)

f_name_d <- paste0(csv_dir, 'cdc_demographics.txt')
write.table(cdc_demographics, f_name_d, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)