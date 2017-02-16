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
  

                  
           
# ---------------- combine and transform demographics data ---------------- #

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


# write demographic data to csv
f_name_d <- paste0(csv_dir, 'aeolus_demographics.txt')
write.table(aeolus_demographics, f_name_d, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)



# ---------------- split and recombine aeolus data because its huge ---------------- #

# count number of visits per patient
aeolus_drill_ids <- aeolus_drill %>%
  group_by(subject_id) %>%
  summarise(n_visits = n_distinct(drug_id))
#ggplot(aeolus_drill_ids, aes(n_visits)) +
#  geom_bar()


# try splitting into two groups: patients who visited once and patients who visited more than once
# split by number of visits and get IDs
aeolus_drill_ids_1 <- aeolus_drill_ids %>%             
  filter(n_visits == 1) %>%
  select(subject_id)
aeolus_drill_ids_2 <- aeolus_drill_ids %>%              
  filter(n_visits > 1 & n_visits <= 3) %>%
  select(subject_id)
aeolus_drill_ids_3 <- aeolus_drill_ids %>%
  filter(n_visits > 3 & n_visits <= 5) %>%
  select(subject_id)
aeolus_drill_ids_4 <- aeolus_drill_ids %>%
  filter(n_visits > 5 & n_visits <= 8) %>%
  select(subject_id)
aeolus_drill_ids_5 <- aeolus_drill_ids %>%
  filter(n_visits > 8 & n_visits <= 10) %>%
  select(subject_id)
aeolus_drill_ids_6 <- aeolus_drill_ids %>%
  filter(n_visits > 10 & n_visits <= 13) %>%
  select(subject_id)
aeolus_drill_ids_7 <- aeolus_drill_ids %>%
  filter(n_visits > 13 & n_visits <= 20) %>%
  select(subject_id)
aeolus_drill_ids_8 <- aeolus_drill_ids %>%
  filter(n_visits > 20) %>%
  select(subject_id)

# join on IDs
aeolus_drill_1 <- aeolus_drill %>%                      
  inner_join(aeolus_drill_ids_1, by = 'subject_id')
aeolus_drill_2 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_2, by = 'subject_id')
aeolus_drill_3 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_3, by = 'subject_id')
aeolus_drill_4 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_4, by = 'subject_id')
aeolus_drill_5 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_5, by = 'subject_id')
aeolus_drill_6 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_6, by = 'subject_id')
aeolus_drill_7 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_7, by = 'subject_id')
aeolus_drill_8 <- aeolus_drill %>%
  inner_join(aeolus_drill_ids_8, by = 'subject_id')

rm(aeolus_drill)



# ---------------- subset anxiety patients ---------------- #
# anxiety indication
anxiety <- 


# get subject ids and drug sequence
cohort <- kava %>%
  inner_join(aeolus_drug, by = c('concept_id' = 'drug_id')) %>%
  select(subject_id, drug_seq, drug_desc = concept_name, drug_code = concept_code) 

# get indication ids and descriptions
cohort <- cohort %>%
  left_join(aeolus_indi, by = 'subject_id') %>%
  select(subject_id, drug_seq, drug_desc, drug_code, indi_snomed_id)

# get snomed codes
cohort <- cohort %>%
  left_join(aeolus_concept, by = c('indi_snomed_id' = 'concept_id')) %>%
  select(subject_id, drug_seq, drug_desc, drug_code, indi_snomed_code = concept_code)

# get icd codes
cohort <- cohort %>%
  left_join(icd9_snomedct, by = c('indi_snomed_code' = 'snomed_code')) %>%
  select(subject_id, drug_seq, drug_desc, drug_code, indi_icd_code = icd_code)

# get ccs categories

# group_by ccs single level and tally





# ---------------- combine and transform temporal data ---------------- #

transform_aeolus <- function(df) {

  df2 <- df %>%
  # join with drug df to get drug sequence
    inner_join(aeolus_drug, by = 'subject_id') %>%
    select(subject_id, drug_seq, outcome_snomed_id) %>%
    
  # join with concept table to get snomed codes
    left_join(aeolus_concept, by = c('outcome_snomed_id' = 'concept_id')) %>%
    select(subject_id, drug_seq, outcome_snomed_code = concept_code) %>%
    
  # join with icd9 snomed mapping table to get icd codes
    left_join(icd9_snomedct, by = c('outcome_snomed_code' = 'snomed_code')) %>%
    select(subject_id, drug_seq, icd_code) %>%
    
  # remove NAs because no match between icd9 codes and snomed ct codes and order by subject/seq
    filter(!is.na(icd_code)) %>%
  
  # reorder by subject id and drug seq
    mutate(admit_id = dense_rank(drug_seq)) %>%   # min_rank leaves gaps while dense_rank does not
    select(subject_id, admit_id, icd_code) %>%
  
  # nest or collapse subject_id/admit_id
    group_by(subject_id, admit_id) %>%
    summarise_each(funs(paste(., collapse = ','))) %>%
    arrange(subject_id, admit_id)
}

aeolus_1 <- transform_aeolus(aeolus_drill_1)
rm(aeolus_1)
aeolus_2 <- transform_aeolus(aeolus_drill_2)
rm(aeolus_2)
aeolus_3 <- transform_aeolus(aeolus_drill_3)
rm(aeolus_3)
aeolus_4 <- transform_aeolus(aeolus_drill_4)
rm(aeolus_4)
aeolus_5 <- transform_aeolus(aeolus_drill_5)
rm(aeolus_5)
aeolus_6 <- transform_aeolus(aeolus_drill_6)
rm(aeolus_6)
aeolus_7 <- transform_aeolus(aeolus_drill_7)
rm(aeolus_7)
aeolus_8 <- transform_aeolus(aeolus_drill_8)

  
# write temporal data to csv
f_name_t <- paste0(csv_dir, 'aeolus_temporal.txt')
write.table(aeolus_1, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_2, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_3, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_4, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_5, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_6, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_7, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(aeolus_8, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)









# ---------------- find anxiety herbals  ---------------- #

# how many cases of drug "kava" or Piper methysticum, black pepper or Piper nigrum, ginger or Zingiber officinalis are there? 0

# how many drug and anxiety cases are there?
# 651  Anxiety disorders    CCS Single Level
# 5.2       Anxiety disorders [651]    CCS Multi Level 2  
anxiety_codes <- data.frame(icd_code = c(29384,30000,30001,30002,30009,30010,30020,30021,30022,30023,30029,
                                         3003,3005,30089,3009,3080,3081,3082,3083,3084,3089,30981,3130,3131,
                                         31321,31322,3133,31382,31383))

