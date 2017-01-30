############################
############################
# ashley s. lee
# data science practice
# brown university cit
# 2017.01.29
# brown center for biomedical informatics
############################
############################


# TODO: turn this script into a function and source(pre-process mimic.R) in global

# base table of patients with number of admits and age of birth/death
base_patients <- mimic_admit %>% 
  group_by(subject_id) %>% 
  summarize(first_admit = min(admittime), admits = n_distinct(hadm_id)) %>% 
  inner_join(mimic_patients, by = 'subject_id') %>% 
  mutate(age = as.numeric(difftime(first_admit, dob, units = 'weeks'))/52.25,
         age_dod = as.numeric(difftime(dod, dob, units = 'weeks'))/52.25) %>%
  select(subject_id, age, age_dod)

# all admits for base patients with admittime as int
base_diags <- base_patients %>%
  inner_join(mimic_diag, by = 'subject_id') %>%
  inner_join(mimic_admit, by = c('subject_id', 'hadm_id')) %>%
  group_by(subject_id) %>%
  mutate(admit_id = dense_rank(admittime)) %>%
  ungroup() %>%
  select(subject_id, icd9_code, admit_id, age, age_dod)

# demographics
mimic_demographics <- base_patients %>%
  inner_join(mimic_patients, by = 'subject_id') %>%
  inner_join(mimic_admit, by = 'subject_id') %>%
  select(subject_id, age, age_dod, gender, expire_flag, admission_type, admission_location, insurance, language, religion, 
         marital_status, ethnicity, hospital_expire_flag) %>%
  distinct(subject_id, .keep_all = T) 

# temporal
mimic_temporal <- base_diags %>%
  left_join(icd9_ccs, by = c('icd9_code' = 'icd_code')) %>%
  mutate(icd_code = paste0(stri_replace_all_fixed(icd9_code, ',', ' '))) %>%
  select(subject_id, admit_id, icd_code) %>%
  group_by(subject_id, admit_id) %>%
  summarise_each(funs(paste(., collapse = ',')))

# write cohort to csv
# write temporal format data and mimic demographics to ./data/input/ folder (for cohort selection, then input into SPADE/APRIORI algorithms)
f_name_t <- paste0(raw_datadir, 'input/mimic_temporal.txt')
write.table(mimic_temporal, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)

f_name_d <- paste0(raw_datadir, 'input/mimic_demographics.txt')
write.table(mimic_demographics, f_name_d, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)







# # check for problems in mapping ICD9 to CCS
# patients and their unique icd-9 codes
# base_temporal <- read_csv(paste0(csv_dir, 'input/mimic_temporal.txt'), col_names = F)
# names(base_temporal) <- c('subject_id', 'admit_id', 'icd_code')
# tidy_temporal <- base_temporal %>%
#   mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
#   unnest(icd_code)
# tidy_temporal_cats <- tidy_temporal %>%
#   left_join(icd9_ccs, by = 'icd_code')
# base_cohort <- tidy_temporal_cats %>%
#   distinct(subject_id, .keep_all = T) %>%
#   select(subject_id)
# unique_icd9 <- base_cohort %>%
#   inner_join(tidy_temporal, by = 'subject_id') %>%
#   distinct(subject_id, icd_code, .keep_all = T) %>%
#   select(subject_id, icd_code)
# test <- unique_icd9 %>%
#   distinct(icd_code) %>%
#   left_join(icd9_ccs) %>%
#   filter(is.na(ccs1_code)) # there are 143 ICD9 codes in MIMIC that don't map to CCS categories
# write.csv(select(test, icd_code), paste0(raw_raw_datadir, 'nomatch_icd9_ccs_codes.csv'), row.names = F)
# test <- unique_icd9 %>%
#   distinct(icd9_code) %>%
#   left_join(icd9_ccs) %>%
#   filter(is.na(ccs_code)) # there are 143 ICD9 codes in MIMIC that don't map to CCS multi level 1 categories
# 
# test2 <- icd9_ccs_multi %>%
#   group_by(icd9_code) %>%
#   summarise(n = n()) %>%
#   filter(n > 1) # there are no ICD9 codes that map to multiple CCS categories in the hcup dataset
# test2 <- unique_icd9 %>%
#   distinct(icd9_code) %>%
#   inner_join(icd9_ccs) %>%
#   group_by(icd9_code) %>%
#   summarise(n = n()) %>%
#   filter(n > 1) # there are no ICD9 codes that map to multiple CCS categories in MIMIC
  