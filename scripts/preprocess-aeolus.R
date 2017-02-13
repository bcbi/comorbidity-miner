# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
# ----------------------------------------|

# we're going to have to filter to just those records with a snomed-ct concept mapping
# in order to make a fair comparison between icd-ccs mappings with other datasets



#  ---------------- combine patient ID and ISR to one column for all tables ---------------- #

aeolus_indi <- aeolus_case_indication %>%
  mutate(subject_id = primaryid,
         subject_id = ifelse(is.na(subject_id), isr, subject_id) ) %>%
  arrange(subject_id, indi_drug_seq) %>%
  select(subject_id, indi_desc = indi_pt, indi_snomed_id = snomed_indication_concept_id) %>%
  filter(!is.na(subject_id)) %>%
  filter(!is.na(indi_snomed_id))
  
aeolus_drill <- aeolus_drug_outcome_drilldown %>%
  mutate(subject_id = primaryid,
         subject_id = ifelse(is.na(subject_id), isr, subject_id) ) %>%
  select(subject_id, drug_id = drug_concept_id, outcome_snomed_id = snomed_outcome_concept_id) %>%
  filter(!is.na(subject_id)) %>%
  filter(!is.na(drug_id)) %>%
  filter(!is.na(outcome_snomed_id))

aeolus_drug <- aeolus_case_drug %>%
  mutate(subject_id = primaryid,
         subject_id = ifelse(is.na(subject_id), isr, subject_id) ) %>%
  select(subject_id, drug_seq, drug_id = standard_concept_id) %>%
  arrange(subject_id, drug_seq)
  

                  
           
# ---------------- combine and transform data ---------------- #

# join indication and drug: indication and drug, then drug names from concept
# - use this to select aeolus cohort, where c1 = drug and c2 = indication
# can potentially search indication by CCS category and not just snomed/icd codes
aeolus_demographics <- aeolus_drug %>%
  inner_join(aeolus_indi, by = 'subject_id') %>%
  arrange(subject_id, drug_seq) %>%
  left_join(aeolus_concept, by = c('drug_id' = 'concept_id')) %>%
  select(subject_id, drug_seq, drug_id, drug_desc = concept_name, indi_desc, indi_snomed_id) %>%
  left_join(aeolus_concept, by = c('indi_snomed_id' = 'concept_id')) %>%
  select(subject_id, drug_seq, drug_id, drug_desc, indi_desc, indi_snomed_id, indi_snomed_code = concept_code, indi_snomed_desc = concept_name)



# count number of visits per patient
aeolus_drill_ids <- aeolus_drill %>%
  group_by(subject_id) %>%
  summarise(n_visits = n_distinct(drug_id))
ggplot(aeolus_drill_ids, aes(n_visits)) +
  geom_bar()


# try splitting into two groups: patients who visited once and patients who visited more than once
aeolus_drill_ids_1 <- aeolus_drill_ids %>%               # get ids
  filter(n_visits == 1) %>%
  select(subject_id)
aeolus_drill_ids_1plus <- aeolus_drill_ids %>%
  anti_join(aeolus_drill_ids_1, by = 'subject_id') %>%
  select(subject_id)
aeolus_drill_1 <- aeolus_drill %>%                       # join on ids
  inner_join(aeolus_drill_ids_1, by = 'subject_id')
aeolus_drill_1plus <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_1plus, by = 'subject_id')


# join with drug, concept, and snomed mapping tables to create temporal dataset and reorder drug seq and change to admit_id
aeolus_t1 <- aeolus_drill_1 %>%
  inner_join(aeolus_drug, by = 'subject_id') %>%
  select(subject_id, drug_seq, outcome_snomed_id) %>%
  left_join(aeolus_concept, by = c('outcome_snomed_id' = 'concept_id')) %>%
  select(subject_id, drug_seq, outcome_snomed_code = concept_code) %>%
  left_join(icd9_snomedct, by = c('outcome_snomed_code' = 'snomed_code')) %>%
  select(subject_id, drug_seq, icd_code) %>%
  arrange(subject_id, drug_seq)
aeolus_t2 <- aeolus_drill_1plus %>%            # TOFIX: might be too large and crash R
  inner_join(aeolus_drug, by = 'subject_id') %>%
  select(subject_id, drug_seq, outcome_snomed_id) %>%
  left_join(aeolus_concept, by = c('outcome_snomed_id' = 'concept_id')) %>%
  select(subject_id, drug_seq, outcome_snomed_code = concept_code) %>%
  left_join(icd9_snomedct, by = c('outcome_snomed_code' = 'snomed_code')) %>%
  select(subject_id, drug_seq, icd_code) %>%
  arrange(subject_id, drug_seq)


# order by subject id and drug seq
aeolus_t1 <- aeolus_t1 %>%
  mutate(admit_id = dense_rank(drug_seq)) %>%
  select(subject_id, admit_id, icd_code)
aeolus_t2 <- aeolus_t2 %>%
  mutate(admit_id = dense_rank(drug_seq)) %>%
  select(subject_id, admit_id, icd_code)


# nest or collapse subject_id/admit_id
aeolus_t1 <- aeolus_t1 %>%
  group_by(subject_id, admit_id) %>%
  summarise_each(funs(paste(., collapse = ',')))
aeolus_t2 <- aeolus_t2 %>%
  group_by(subject_id, admit_id) %>%
  summarise_each(funs(paste(., collapse = ',')))


# append two dataframes (should have 2804660 patients after removing subject, drug, snomed, indication, and outcome NAs and joining)
aeolus_temporal <- as.data.frame(rbind(aeolus_t1, aeolus_t2))



# ---------------- write temporal file to csv ---------------- #
  
# write temporal data
f_name_t <- paste0(csv_dir, 'aeolus_temporal.txt')
write.table(aeolus_temporal, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)

# write demographic data
f_name_d <- paste0(csv_dir, 'aeolus_demographics.txt')
write.table(aeolus_demographics, f_name_d, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)








# ---------------- find anxiety herbals  ---------------- #

# how many cases of drug "kava" or Piper methysticum, black pepper or Piper nigrum, ginger or Zingiber officinalis are there? 0

# how many drug and anxiety cases are there?
# 651  Anxiety disorders    CCS Single Level
# 5.2       Anxiety disorders [651]    CCS Multi Level 2  
anxiety_codes <- data.frame(icd_code = c(29384,30000,30001,30002,30009,30010,30020,30021,30022,30023,30029,
                                         3003,3005,30089,3009,3080,3081,3082,3083,3084,3089,30981,3130,3131,
                                         31321,31322,3133,31382,31383))

