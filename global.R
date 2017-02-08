# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
# ----------------------------------------|

# global.R reads in the data sources, including ICD-9 and ICD-10 categories.
# the preprocessing scripts found in ./scripts map the data sources to a generic format
# the generic format works with both arules and arules sequences

# TODO: add CDC data from CSV, add AEOLUS data from DB

######################
# setup
######################

# load packages
library(shiny)
library(shinydashboard)
library(googleVis)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(arules)
library(arulesViz)
library(arulesSequences)
library(stringi)
library(stringr)
library(DT)
library(ggplot2)

# set source as 'CSV' or 'DB', set raw and pre-processed data directories
src <- 'CSV'
raw_datadir <- './data/'
csv_dir <- './data/input/'



######################
# pull raw data
######################

if (src == 'CSV') {
  
  #-----------------------------MIMIC-----------------------------#
  
  # ehr data
  mimic_diag <- read_csv(paste0(raw_datadir, 'mimic_tables/DIAGNOSES_ICD.csv.gz'), na = c('NA','','NULL','null'))
  mimic_icd9 <- read_csv(paste0(raw_datadir, 'mimic_tables/D_ICD_DIAGNOSES.csv.gz'), na = c('NA','','NULL','null'))
  mimic_admit <- read_csv(paste0(raw_datadir, 'mimic_tables/ADMISSIONS.csv.gz'), na = c('NA','','NULL','null'))
  mimic_patients <- read_csv(paste0(raw_datadir, 'mimic_tables/PATIENTS.csv.gz'), na = c('NA','','NULL','null'))
  
  
  # #-----------------------------EMR ROBOTS-----------------------------#
  # # for demo purposes? - no longer need this bc we can use CDC for public demo
  # 
  # # ehr data
  # emrr_diag <- read_csv(paste0(raw_datadir, 'EMR-robots-100/AdmissionsDiagnosesCorePopulatedTable.txt.gz'), na = c('NA','','NULL','null'))
  # emrr_admit <- read_csv(paste0(raw_datadir, 'EMR-robots-100/AdmissionsCorePopulatedTable.txt'), na = c('NA','','NULL','null'))
  # emrr_patients <- read_csv(paste0(raw_datadir, 'EMR-robots-100/PatientCorePopulatedTable.txt'), na = c('NA','','NULL','null'))
 
  # names(emrr_diag) <- tolower(names(emrr_diag))
  # names(emrr_admit) <- tolower(names(emrr_admit))
  # names(emrr_patients) <- tolower(names(emrr_patients))
  
  
} else if (src == 'DB') {
  
  #-----------------------------mimic db-----------------------------#
  
  # TODO: test on SH, maybe add col names?
  mimic <- src_mysql(dbname = 'mimic', host = 'localhost',
                     user = 'mimic_user', password = 'mimic')
  
  mimic_diag <- collect(tbl(mimic, 'diagnoses_icd'))
  mimic_icd9 <- collect(tbl(mimic, 'd_icd_diagnoses'))
  mimic_admit <- collect(tbl(mimic, 'admissions'))
  mimic_patients <- collect(tbl(mimic, 'patients'))
  icd9_ccs_multi <- collect(tbl(mimic, 'd_icd_diagnoses_ccs_multi'))
  icd9_ccs_single <- collect(tbl(mimic, 'd_icd_diagnoses_ccs_single'))
  icd9_ccs_desc <- collect(tbl(mimic, 'd_ccs_cat_single'))
  
  
  #-----------------------------aeolus db-----------------------------#


  
}



########################
# process mapping tables
########################

#-----------------------------ICD10-----------------------------#

# icd10-cm - icd10 chapter - CCS
icd10_ccs <- read_csv(paste0(raw_datadir, 'icd_grouping/ccs_icd10cm_2017.csv'))

names(icd10_ccs) <- c("icd_code","ccs_code","icd_desc","ccs_desc","ccs1_code","ccs1_desc","ccs2_code","ccs2_desc")
names(icd10_ccs) <- tolower(names(icd10_ccs))
names(icd10_ccs) <- gsub("'", "", names(icd10_ccs))
names(icd10_ccs) <- gsub("ccs ", "", names(icd10_ccs))
names(icd10_ccs) <- gsub("multi ", "", names(icd10_ccs))
names(icd10_ccs) <- gsub("icd-10-cm ", "", names(icd10_ccs))
names(icd10_ccs) <- gsub(" ", "", names(icd10_ccs))

# remove single quotes from ccs table and reorder columns to match icd9
icd10_ccs <- icd10_ccs %>%
  mutate(icd_code = gsub("'", "", icd_code),
         ccs_code = gsub("'", "", ccs_code),
         ccs1_code = gsub("'", "", ccs1_code),
         ccs2_code = gsub("'", "", ccs2_code)) %>%
  select(icd_code,icd_desc,ccs_code,ccs_desc,ccs1_code,ccs1_desc,ccs2_code,ccs2_desc)

# make icd10 chapter description table
icd10_chap <- data.frame(icd_chp_code = c(1:22),
           icd_chp_desc = c('certain infectious and parasitic diseases','neoplasms',
                     'diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism',
                     'endocrine, nutritional and metabolic diseases','mental and behavioral disorders','Diseases of the nervous system',
                     'diseases of the eye and adnexa','diseases of the ear and mastoid process','diseases of the circulatory system',
                     'diseases of the respiratory system','diseases of the digestive system','diseases of the skin and subcutaneous tissue',
                     'diseases of the musculoskeletal system and connective tissue','diseases of the genitourinary system',
                     'pregnancy, childbirth and the puerperium','certain conditions originating in the perinatal period',
                     'congenital malformations, deformations and chromosomal abnormalities',
                     'symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified',
                     'injury, poisoning and certain other consequences of external causes','external causes of morbidity and mortality',
                     'factors influencing health status and contact with health services','codes for special purposes'))


# make complete CCS icd10 mappings table
icd10_ccs <- icd10_ccs %>%
  mutate(icd_chp_code = NA,
         icd_chp_code = ifelse(icd_code <= 'B99', 1, NA),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'C00' & icd_code <= 'D48'), 2, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'D50' & icd_code <= 'D89'), 3, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'E00' & icd_code <= 'D90'), 4, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'F00' & icd_code <= 'F99'), 5, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'G00' & icd_code <= 'G99'), 6, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'H00' & icd_code <= 'H59'), 7, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'H60' & icd_code <= 'H95'), 8, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'I00' & icd_code <= 'I99'), 9, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'J00' & icd_code <= 'J99'), 10, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'K00' & icd_code <= 'K93'), 11, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'L00' & icd_code <= 'L99'), 12, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'M00' & icd_code <= 'M99'), 13, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'N00' & icd_code <= 'N99'), 14, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'O00' & icd_code <= 'O99'), 15, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'P00' & icd_code <= 'P96'), 16, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'Q00' & icd_code <= 'Q99'), 17, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'R00' & icd_code <= 'R99'), 18, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'S00' & icd_code <= 'T98'), 19, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'V01' & icd_code <= 'Y98'), 20, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'Z00' & icd_code <= 'Z99'), 21, icd_chp_code),
         icd_chp_code = ifelse(is.na(icd_chp_code) & (icd_code >= 'U00' & icd_code <= 'U99'), 22, icd_chp_code)) %>%
  left_join(icd10_chap) %>%
  
  # create new columns to be used in dropdown menus for readability
  mutate(icd = paste0(icd_code, ' - ', icd_desc),
         icd_c = paste0(icd_chp_code, ' - ', icd_chp_desc),
         ccs_s = paste0(ccs_code, ' - ', ccs_desc),
         ccs1_desc = ifelse(ccs1_desc == 'Mental illness', 'Mental Illness', ccs1_desc),
         ccs1 = paste0(ccs1_code, ' - ', ccs1_desc),
         ccs2 = paste0(ccs2_code, ' - ', ccs2_desc))



#-----------------------------ICD9-----------------------------#

# icd9 : ccs - icd9 chapter
icd9_ccs_multi <- read_csv(paste0(raw_datadir, 'icd_grouping/icd_ccs_multi_2015.csv'), col_types = c('ccccccccc'))
icd9_ccs_single <- read_csv(paste0(raw_datadir, 'icd_grouping/icd_ccs_single_2015.csv'))
icd9_ccs_desc <- read_csv(paste0(raw_datadir, 'icd_grouping/icd_ccs_single_2013.csv'))
icd9_chap <- read_csv(paste0(raw_datadir, 'icd_grouping/icd9_chapters.csv'))

names(mimic_diag) <- tolower(names(mimic_diag))
names(mimic_diag)[5] <- 'icd_code'
names(mimic_icd9) <- tolower(names(mimic_icd9))
names(mimic_icd9)[2] <- 'icd_code'
names(mimic_admit) <- tolower(names(mimic_admit))
names(mimic_patients) <- tolower(names(mimic_patients))
names(icd9_ccs_multi) <- c('icd_code', 'lvl1_cd', 'lvl1_desc', 'lvl2_cd', 'lvl2_desc', 'lvl3_cd', 'lvl3_desc', 'lvl4_cd', 'lvl4_desc')
names(icd9_ccs_single) <- c('icd_code', 'ccs_cat', 'ccs_desc', 'icd_desc', 'ccs_opt_cat', 'cch_opt_desc')
names(icd9_ccs_desc) <- c('ccs_cat', 'ccs_full_desc')

### replace NA's in ccs_multi with previous category description
icd9_ccs_multi$lvl2_cd[is.na(icd9_ccs_multi$lvl2_cd)] <- icd9_ccs_multi$lvl1_cd[is.na(icd9_ccs_multi$lvl2_cd)]
icd9_ccs_multi$lvl2_desc[is.na(icd9_ccs_multi$lvl2_desc)] <- icd9_ccs_multi$lvl1_desc[is.na(icd9_ccs_multi$lvl2_desc)]
icd9_ccs_multi$lvl3_cd[is.na(icd9_ccs_multi$lvl3_cd)] <- icd9_ccs_multi$lvl2_cd[is.na(icd9_ccs_multi$lvl3_cd)]
icd9_ccs_multi$lvl3_desc[is.na(icd9_ccs_multi$lvl3_desc)] <- icd9_ccs_multi$lvl2_desc[is.na(icd9_ccs_multi$lvl3_desc)]
icd9_ccs_multi$lvl4_cd[is.na(icd9_ccs_multi$lvl4_cd)] <- icd9_ccs_multi$lvl3_cd[is.na(icd9_ccs_multi$lvl4_cd)]
icd9_ccs_multi$lvl4_desc[is.na(icd9_ccs_multi$lvl4_desc)] <- icd9_ccs_multi$lvl3_desc[is.na(icd9_ccs_multi$lvl4_desc)]

### strip out brackets and commas from ccs_multi descriptions
icd9_ccs_multi <- data.frame(sapply(icd9_ccs_multi, str_replace_all, pattern = '\\[.*\\]', replacement = ''))
icd9_ccs_multi <- data.frame(sapply(icd9_ccs_multi, str_replace_all, pattern = ',', replacement = ''))

### convert from factor to character
icd9_ccs_multi <- icd9_ccs_multi %>%
  mutate_if(is.factor, as.character)

### add chapter numbers to icd9_chap
mp <- unique(icd9_chap$icd_chp) 
mp <- data.frame(cbind(mp, seq(1, length(mp), 1)))
names(mp) <- c('icd_chp', 'icd_c')
icd9_chap <- icd9_chap %>%
  inner_join(mp, by = 'icd_chp')


### CCS icd9 mappings table
icd9_ccs <- icd9_ccs_multi %>% 
  # add icd9 codes and long titles
  inner_join(mimic_icd9, by = 'icd_code') %>%
  select(icd_code, long_title, lvl1_cd, lvl1_desc, lvl2_cd, lvl2_desc, lvl3_cd, lvl3_desc) %>%
  # add ccs single level categories
  inner_join(icd9_ccs_single, by = 'icd_code') %>%
  select(icd_code, long_title, ccs_cat, ccs_desc, lvl1_cd, lvl1_desc, lvl2_cd, lvl2_desc, lvl3_cd, lvl3_desc) %>%
  # add icd9 chapters
  inner_join(icd9_chap, by = c('icd_code' = 'icd')) %>%
  select(icd_code, icd_desc = long_title, ccs_code = ccs_cat, ccs_desc, ccs1_code = lvl1_cd, ccs1_desc = lvl1_desc, ccs2_code = lvl2_cd, ccs2_desc = lvl2_desc, ccs3_code = lvl3_cd, ccs3_desc = lvl3_desc, 
         icd_chp_code = icd_c, icd_chp_desc = icd_chp) %>%
  # create new columns to be used in dropdown menus for readability
  mutate(icd = paste0(icd_code, ' - ', icd_desc),
         icd_c = paste0(icd_chp_code, ' - ', icd_chp_desc),
         ccs_s = paste0(ccs_code, ' - ', ccs_desc),
         ccs1_desc = ifelse(ccs1_desc == 'Mental illness', 'Mental Illness', ccs1_desc),
         ccs1 = paste0(ccs1_code, ' - ', ccs1_desc),
         ccs2 = paste0(ccs2_code, ' - ', ccs2_desc),
         ccs3 = paste0(ccs3_code, ' - ', ccs3_desc),
         ccs_code = as.character(ccs_code),
         icd_chp_code = as.character(icd_chp_code)
         )



######################
# pull processed data
######################

mimic_base_temporal <- read_csv(paste0(csv_dir, 'mimic_temporal.txt'), col_names = F)
mimic_base_demographics <- read_csv(paste0(csv_dir, 'mimic_demographics.txt'), col_names = F)
names(mimic_base_temporal) <- c('subject_id', 'admit_id', 'icd_code')
names(mimic_base_demographics) <- c('subject_id', 'age', 'age_dod', 'sex_at_birth', 'expire_flag', 
                                    'admit_type', 'admit_location', 'insurance', 'language', 'religion', 'marital', 'ethnicity', 'hospital_expire_flag')

cdc_base_temporal <- read_csv(paste0(csv_dir, 'cdc_temporal.txt'), col_names = F)
cdc_base_demographics <- read_csv(paste0(csv_dir, 'cdc_demographics.txt'), col_names = F)
names(cdc_base_temporal) <- c('subject_id', 'admit_id', 'icd_code')
names(cdc_base_demographics) <- c('subject_id', 'citizenship', 'education1', 'education2', 'sex_at_birth', 'age_group', 'age_infant', 'marital', 'race', 'hispanic')

#aeolus_base_temporal <- read_csv(paste0(csv_dir, 'aeolus_temporal.txt'), col_names = F)
#aeolus_base_demographics <- read_csv(paste0(csv_dir, 'aeolus_demographics.txt'), col_names = F)
#names(aeolus_base_temporal) <- c('subject_id', 'admit_id', 'icd_code')
#names(aeolus_base_demographics)[1] <- 'subject_id'


# transform data using tidyr so that each condition gets it's own row
mimic_tidy_temporal <- mimic_base_temporal %>%
  mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
  unnest(icd_code)
cdc_tidy_temporal <- mimic_base_temporal %>%
  mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
  unnest(icd_code)




######################
# rm dataframes
######################

rm(icd10_chap, icd9_ccs_desc, icd9_ccs_multi, icd9_ccs_single, icd9_chap,
   mimic_admit, mimic_diag, mimic_icd9, mimic_patients)