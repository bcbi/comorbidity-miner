# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
# ----------------------------------------|



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
  select(subject_id, icd_code, admit_id, age, age_dod)

# demographics
mimic_demographics <- base_patients %>%
  inner_join(mimic_patients, by = 'subject_id') %>%
  inner_join(mimic_admit, by = 'subject_id') %>%
  select(subject_id, age, age_dod, gender, expire_flag, admission_type, admission_location, insurance, language, religion, 
         marital_status, ethnicity, hospital_expire_flag) %>%
  distinct(subject_id, .keep_all = T) 

# temporal
mimic_temporal <- base_diags %>%
  left_join(icd9_ccs, by = c('icd_code' = 'icd_code')) %>%
  mutate(icd_code = paste0(stri_replace_all_fixed(icd_code, ',', ' '))) %>%
  select(subject_id, admit_id, icd_code) %>%
  group_by(subject_id, admit_id) %>%
  summarise_each(funs(paste(., collapse = ',')))

# write temporal format data and mimic demographics to ./data/input/ folder (read into memory in global.R)
f_name_t <- paste0(csv_dir, 'mimic_temporal.txt')
write.table(mimic_temporal, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)

f_name_d <- paste0(csv_dir, 'mimic_demographics.txt')
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

# USE CASE: mental health, substance abuse comorbidity differences between datasets
# number of diagnoses and number of patients per icd code
# - icd9 codes
mc <- icd9_ccs %>%
  filter(ccs_code %in% c(657)) %>%
  select(icd_code, icd_desc)
ac <- icd9_ccs %>%
  filter(ccs_code %in% c(651)) %>%
  select(icd_code, icd_desc)
sc <- icd9_ccs %>%
  filter(ccs_code %in% c(660, 661)) %>%
  select(icd_code, icd_desc)
# - num codes
mcp <- mimic_temporal %>%
  mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
  unnest(icd_code) %>%
  left_join(icd9_ccs, by = 'icd_code') %>%
  filter(icd_code %in% mc$icd_code)
acp <- mimic_temporal %>%
  mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
  unnest(icd_code) %>%
  left_join(icd9_ccs, by = 'icd_code') %>%
  filter(icd_code %in% ac$icd_code)
scp <- mimic_temporal %>%
  mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
  unnest(icd_code) %>%
  left_join(icd9_ccs, by = 'icd_code') %>%
  filter(icd_code %in% sc$icd_code)
# - num patients
mcpd <- mcp %>%
  distinct(subject_id, .keep_all = T)
acpd <- acp %>%
  distinct(subject_id, .keep_all = T)
scpd <- scp %>%
  distinct(subject_id, .keep_all = T)
# - histograms mood and anxiety disorders
ggplot(mcp, aes(icd_desc, fill = ccs_desc)) +
  geom_bar(stat = 'count') + 
  coord_flip() +
  ggtitle('Mood Disorder Codes in MIMIC')
ggplot(mcpd, aes(icd_desc)) +
  geom_bar(stat = 'count') + 
  coord_flip() +
  ggtitle('Patients with Mood Disorders in MIMIC (4718 total patients)')
# - histograms mood and anxiety disorders
ggplot(acp, aes(icd_desc, fill = ccs_desc)) +
  geom_bar(stat = 'count') + 
  coord_flip() +
  ggtitle('Anxiety Disorder Codes in MIMIC')
ggplot(acpd, aes(icd_desc)) +
  geom_bar(stat = 'count') + 
  coord_flip() +
  ggtitle('Patients with Anxiety Disorders in MIMIC 1850 total patients)')
# histograms substance and alcohol disorders
ggplot(scp, aes(icd_desc, fill = ccs_desc)) +
  geom_bar(stat = 'count') + 
  coord_flip() +
  ggtitle('Substance and Alcohol Disorder Codes in MIMIC')
ggplot(scpd, aes(icd_desc, fill = ccs_desc)) +
  geom_bar(stat = 'count') + 
  coord_flip() +
  ggtitle('Patients with Substance or Alcohol Disorders in MIMIC (4800 total patients)')

 
  