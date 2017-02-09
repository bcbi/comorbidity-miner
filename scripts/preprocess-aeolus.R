# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
# ----------------------------------------|

# we're going to have to filter to just those records with a snomed-ct concept mapping
# in order to make a fair comparison between icd-ccs mappings with other datasets

library(readr)
library(magrittr)
library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(tidyr)
library(scales)
library(knitr)



# combine patient ID and ISR to one column for all tables
aeolus_indi <- aeolus_indication %>%
  mutate(subject_id = primaryid,
         subject_id = ifelse(is.na(subject_id), isr, subject_id) ) %>%
  select(subject_id, indi_drug_code = indi_drug_seq, indi_snomed_code = snomed_indication_concept_id, indi_snowmed_desc = indi_pt)
aeolus_drill <- aeolus_drilldown %>%
  mutate(subject_id = primaryid,
         subject_id = ifelse(is.na(subject_id), isr, subject_id) ) %>%
  select(subject_id, drug_code = drug_concept_id, outcome_snomed_code = snomed_outcome_concept_id)

# how many patients do the data have in common? 6771 when n=500000
aeolus_df <- aeolus_indi %>%
  inner_join(aeolus_drill, by = 'subject_id') 

# how many drugs do the data have in common?
aeolus_df <- aeolus_df %>%
  _join(aeoul)

# how many cases of drug "kava" or Piper methysticum, black pepper or Piper nigrum, ginger or Zingiber officinalis are there?

# how many drug and anxiety cases are there?


## final datasets
aeolus_temporal <- 

aeolus_demographics <- # this will come from the FAERS data set, can join by ISR and patientID
  # maybe use the caseid variable in aeolus_unique_all_case to join with FAERS?


# write temporal format data and mimic demographics to ./data/input/ folder (read into memory in global.R)
f_name_t <- paste0(csv_dir, 'cdc_temporal.txt')
write.table(cdc_temporal, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)

f_name_d <- paste0(csv_dir, 'cdc_demographics.txt')
write.table(cdc_demographics, f_name_d, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)
