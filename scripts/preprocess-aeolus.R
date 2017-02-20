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
  select(subject_id, drug_id = drug_concept_id, outcome_concept_id, outcome_snomed_id = snomed_outcome_concept_id) %>%
  filter(!is.na(subject_id)) %>%
  filter(!is.na(drug_id)) %>%
  filter(!is.na(outcome_concept_id))

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



# ---------------- subset anxiety and depression patients ---------------- #
# anxiety indication
# subject_ids
anxiety <- aeolus_indi %>%
  filter(grepl('ANXIETY', indi_desc)) %>%
  distinct(subject_id)
# get drug sequence and names
anxiety_drugs <- anxiety %>%
  left_join(aeolus_drug, by = 'subject_id') %>%
  left_join(aeolus_concept, by = c('drug_id' = 'concept_id')) 
# most used drugs
anxiety_drugs_plot <- anxiety_drugs %>%
  group_by(concept_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 1000)
# plot
ggplot(anxiety_drugs_plot, aes(concept_name, count)) +
  geom_bar(stat = 'identity') +
  coord_flip() + xlab('drug') + ylab('patients') + ggtitle('anxiety')

# depression indication
# subject ids
d <- aeolus_indi %>%
  filter(grepl('DEPRESS', indi_desc)) %>%
  distinct(subject_id)
# get drug sequence and names
d_drugs <- d %>%
  left_join(aeolus_drug, by = 'subject_id') %>%
  left_join(aeolus_concept, by = c('drug_id' = 'concept_id'))
# most used drugs
d_drugs_plot <- d_drugs %>%
  group_by(concept_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 1000)
# plot
ggplot(d_drugs_plot, aes(concept_name, count)) +
  geom_bar(stat = 'identity') +
  coord_flip() + xlab('drug') + ylab('patients') + ggtitle('depression')



# ---------------- constrain aeolus to cases with snomed codes that map to icd codes ---------------- #
# get outcome ids with match to eventual icd9-ccs mapping table
df <- aeolus_drill %>%
  distinct(outcome_snomed_id) %>%
  inner_join(aeolus_concept, by = c('outcome_snomed_id' = 'concept_id')) %>%
  select(outcome_snomed_id, outcome_snomed_code = concept_code) %>%
  inner_join(icd9_snomedct, by = c('outcome_snomed_code' = 'snomed_code')) %>%
  inner_join(icd9_ccs, by = 'icd_code') %>%
  select(outcome_snomed_id)

# filter aeolus_drill to outcomes with snomed_code (concept) >>> icd_code (snomedct) >>> icd_code (ccs) match
aeolus1 <- aeolus_drill %>%
  inner_join(df, by = 'outcome_snomed_id')

# get drug seq
aeolus2 <- aeolus1 %>%
  inner_join(aeolus_drug, by = c('subject_id', 'drug_id'))

# get outcome snomed codes >>> icd codes
aeolus3 <- aeolus2 %>%
  inner_join(aeolus_concept, by = c('outcome_snomed_id' = 'concept_id')) %>%
  inner_join(icd9_snomedct, by = c('concept_code' = 'snomed_code')) %>%
  select(subject_id, drug_seq, icd_code) %>%
  inner_join(icd9_ccs, by = 'icd_code') %>%
  select(subject_id, admit_id = drug_seq, icd_code)

# re-rank drug sequence to remove really large sequence values and change variable name to admit_id
aeolus <- aeolus3 %>%
  arrange(subject_id, admit_id) %>%
  mutate(admit_id = dense_rank(admit_id))


# write temporal data to csv
f_name_t <- paste0(csv_dir, 'aeolus_temporal.txt')
write.table(aeolus, f_name_t, quote = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)

