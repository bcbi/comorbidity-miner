############################
############################
# ashley s. lee
# data science practice
# brown university cit
# 2017.01.29
# brown center for biomedical informatics
############################
############################

shinyServer(function(input, output, session) {

######################
# data transformations
######################

# read in pre-processed data from CSV
# TODO: add a switch for different data sets with input$data == 'fname'
base_temporal <- read_csv(paste0(csv_dir, 'input/mimic_temporal.txt'), col_names = F)
base_demographics <- read_csv(paste0(csv_dir, 'input/mimic_demographics.txt'), col_names = F)

names(base_temporal) <- c('subject_id', 'admit_id', 'icd_code')
names(base_demographics)[1] <- 'subject_id'

# transform data using tidyr so that each condition gets it's own row
tidy_temporal <- base_temporal %>%
  mutate(icd_code = strsplit(as.character(icd_code), ",")) %>%
  unnest(icd_code)

# add switch for icd9 vs icd10 and join tidy data sets with ccs mapping tables
categs <- reactive({
  if(input$icd_version == 'icd9') {
    return(icd9_ccs)
    # tidy_temporal_cats <- tidy_temporal %>%
    #   left_join(icd9_ccs, by = "icd_code")
  }
  else if(input$icd_version == 'icd10') {
    return(icd10_ccs)
    # tidy_temporal_cats <- tidy_temporal %>%
    #   left_join(icd10_ccs, by = "icd_code")
  }
})


######################################################
# cohort selection
# search by ICD codes, ICD chapters, or CCS categories
######################################################

# get cohort temporal, association, and demographics datasets
get_cohort <- reactive({
  
  # add all categories to tidy temporal
  tidy_temporal_cats <- tidy_temporal %>%
    left_join(categs(), by = 'icd_code')
  
  # select primary condition
  base_cohort <- tidy_temporal_cats %>%
    filter(if(input$grouping_lvl == 'icd') {icd_code == gsub(' - .*', '', input$code)}
           else if(input$grouping_lvl == 'icd_c') {icd_chp_code == gsub(' - .*', '', input$code)}
           else if(input$grouping_lvl == 'ccs_s') {ccs_code == gsub(' - .*', '', input$code)}
           else if(input$grouping_lvl == 'ccs1') {ccs1_code == gsub(' - .*', '', input$code)}
           else if(input$grouping_lvl == 'ccs2') {ccs2_code == gsub(' - .*', '', input$code)}
           else if(input$grouping_lvl == 'ccs3') {ccs3_code == gsub(' - .*', '', input$code)}
           else if(input$grouping_lvl == 'phewas') {phewas_code == gsub(' - .*', '', input$code)}) %>%
    filter(if(input$min_two_visits == T) {admit_id > 1}) %>%
    distinct(subject_id, .keep_all = T) %>%
    select(subject_id)
  
  # select secondary condition
  cohort_plus <- base_cohort %>%
    inner_join(tidy_temporal_cats, by = 'subject_id') %>%
    filter(if(input$grouping_lvl2 == 'icd') {icd_code == gsub(' - .*', '', input$code2)}
           else if(input$grouping_lvl2 == 'icd_c') {icd_chp_code == gsub(' - .*', '', input$code2)}
           else if(input$grouping_lvl2 == 'ccs_s') {ccs_code == gsub(' - .*', '', input$code2)}
           else if(input$grouping_lvl2 == 'ccs1') {ccs1_code == gsub(' - .*', '', input$code2)}
           else if(input$grouping_lvl2 == 'ccs2') {ccs2_code == gsub(' - .*', '', input$code2)}
           else if(input$grouping_lvl2 == 'ccs3') {ccs3_code == gsub(' - .*', '', input$code2)}
           else if(input$grouping_lvl2 == 'phewas') {phewas_code == gsub(' - .*', '', input$code2)}) %>%
    distinct(subject_id, .keep_all = T) %>%
    select(subject_id)
  
  # cohort - secondary condition
  cohort_minus <- base_cohort %>% 
    anti_join(cohort_plus, by = 'subject_id')
  
  # select primary and secondary condition interaction
  if(input$interaction == 'Condition 1 + 2') {cohort <- cohort_plus}
  else if(input$interaction == 'Condition 1 - 2') {cohort <- cohort_minus}
  
  #------------------------get patient's diagnosis codes and demographics------------------------#
  # patients and their unique icd-9 codes
  unique_icd <- cohort %>%
    inner_join(tidy_temporal, by = 'subject_id') %>%
    distinct(subject_id, icd_code, .keep_all = T) %>%
    select(subject_id, icd_code)
  
  # patient demographics
  demographics <- cohort %>%
    inner_join(base_demographics, by = 'subject_id') %>%
    distinct(subject_id, .keep_all = T) 
  
  # temporal
  # TODO: add another if condition for icd9 vs icd10
  if(input$seq_grp_lvl == 'icd') {
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(icd_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}
  else if(input$seq_grp_lvl == 'icd_c') {        
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(icd_chp_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}
  else if(input$seq_grp_lvl == 'ccs_s') {        
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(ccs_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}
  else if(input$seq_grp_lvl == 'ccs1') {        
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(ccs1_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}
  else if(input$seq_grp_lvl == 'ccs2') {        
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(ccs2_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}
  else if(input$seq_grp_lvl == 'ccs3') {        
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(ccs3_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}
  else if(input$seq_grp_lvl == 'phewas') {        
    temporal <- cohort %>%
      inner_join(tidy_temporal_cats, by = "subject_id") %>%
      mutate(cat = paste0(stri_replace_all_fixed(phewas_code, ',', ' '))) %>%
      select(subject_id, admit_id, cat) %>%
      group_by(subject_id, admit_id) %>%
      summarise_each(funs(paste(., collapse = ',')))}


  # association 
  association <- temporal %>%
    select(subject_id, cat)

  list(unique_icd = unique_icd, demographics = demographics, temporal = temporal, association = association)
})


#####################################################
# write cohort to csv only when user selects "Submit"
#####################################################

observeEvent(input$genData, {
  # write temporal and association to ./data/input/ folder (for input into SPADE and APRIORI algorithms)
  f_name_1 <- ifelse(input$interaction == 'Condition 1 + 2', 
                     paste0(csv_dir, gsub(' - .*', '', input$code), '_plus_', gsub(' - .*', '', input$code2), '_temporal.txt'),
                     paste0(csv_dir, gsub(' - .*', '', input$code), '_minus_', gsub(' - .*', '', input$code2), '_temporal.txt'))
  f_name_2 <- ifelse(input$interaction == 'Condition 1 + 2',
                     paste0(csv_dir, gsub(' - .*', '', input$code), '_plus_', gsub(' - .*', '', input$code2), '_association.txt'),
                     paste0(csv_dir, gsub(' - .*', '', input$code), '_minus_', gsub(' - .*', '', input$code2), '_association.txt'))
  write.table(get_cohort()$temporal, f_name_1, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)
  write.table(get_cohort()$association, f_name_2, quote = FALSE, sep = ',', row.names = FALSE, col.names = FALSE)
})


########################################################
### add copy, print, and download buttons to data tables
########################################################

output$patientsTable <- renderDataTable({
  datatable(
    get_cohort()$unique_icd, 
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list('copy', 'print', list(
        extend = 'collection',
        buttons = 
          list(list(extend='csv', filename = 'hitStats'),
               list(extend='excel', filename = 'hitStats'),
               list(extend='pdf', filename= 'hitStats')),
        text = 'Download')),
      scrollX = TRUE,
      pageLength = nrow(get_cohort()$unique_icd9),
      order = list(list(1, 'asc'))
    ), rownames = F)
})

output$demographicsTable <- renderDataTable({
  datatable(
    get_cohort()$demographics, 
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download')),
      pageLength = nrow(get_cohort()$demographics),
      scrollX = TRUE,
      order = list(list(1, 'asc'))
    ), rownames = F)
})


###################################################
# creates reactive table of icd ccs phewas mappings
###################################################

output$outGroupTable <- renderDataTable({
  if(input$icd_version == 'icd9') {
    datatable(outGroup <- categs() %>%
                mutate(),
              rownames = F)
  } else if(input$icd_version == 'icd10') {
    datatable(outGroup <- categs() %>% 
                mutate(),
              rownames = F)
  }
})


####################################################################################
# update Cohort Selection Input with user-selected ICD version and Grouping Category
####################################################################################

observe({
  if(input$grouping_lvl == 'icd') {updateSelectInput(session, "code", choices = unique(categs()$icd))}
  else if(input$grouping_lvl == 'icd_c') {updateSelectInput(session, "code", choices = unique(categs()$icd_c))}
  else if(input$grouping_lvl == 'ccs_s') {updateSelectInput(session, "code", choices = unique(categs()$ccs_s))}
  else if(input$grouping_lvl == 'ccs1') {updateSelectInput(session, "code", choices = unique(categs()$ccs1))}
  else if(input$grouping_lvl == 'ccs2') {updateSelectInput(session, "code", choices = unique(categs()$ccs2))}
  else if(input$grouping_lvl == 'ccs3') {updateSelectInput(session, "code", choices = unique(categs()$ccs3))}
  else if(input$grouping_lvl == 'phewas') {updateSelectInput(session, "code", choices = unique(categs()$phewas))}

  if(input$grouping_lvl2 == 'icd') {updateSelectInput(session, "code2", choices = unique(categs()$icd))}
  else if(input$grouping_lvl2 == 'icd_c') {updateSelectInput(session, "code2", choices = unique(categs()$icd_c))}
  else if(input$grouping_lvl2 == 'ccs_s') {updateSelectInput(session, "code2", choices = unique(categs()$ccs_s))}
  else if(input$grouping_lvl2 == 'ccs1') {updateSelectInput(session, "code2", choices = unique(categs()$ccs1))}
  else if(input$grouping_lvl2 == 'ccs2') {updateSelectInput(session, "code2", choices = unique(categs()$ccs2))}
  else if(input$grouping_lvl2 == 'ccs3') {updateSelectInput(session, "code2", choices = unique(categs()$ccs3))}
  else if(input$grouping_lvl2 == 'phewas') {updateSelectInput(session, "code2", choices = unique(categs()$phewas))}
})


######################
# temporal
######################

# TODO: join codes with descriptions and add switch to view codes or descriptions

get_seqs <- reactive({
  
  #---------------------------------------cSPADE---------------------------------------#
  fname <- ifelse(input$interaction == 'Condition 1 + 2',
         paste0(csv_dir, gsub(' - .*', '', input$code), '_plus_', gsub(' - .*', '', input$code2), '_temporal.txt'),
         paste0(csv_dir, gsub(' - .*', '', input$code), '_minus_', gsub(' - .*', '', input$code2), '_temporal.txt'))
  
  ld <- read_baskets(fname, sep = ',', info = c('sequenceID', 'eventID'))
  
  # run SPADE algorithm to get sequences
  sequences <- cspade(ld, parameter=list(support = input$supportS), control = list(verbose = TRUE))
  
  #---------------------------------------seq_df---------------------------------------#
  # order sequences by support
  seqdf <- as(sequences, 'data.frame')
  seqdf <- arrange(seqdf, desc(support))
  
  # change format to make sequences more readable
  # - diagnoses within a single admission separated by '|'
  # - successive diagnoses in a sequence separated by '=>'
  seqdf$sequence <- str_replace_all(seqdf$sequence, c('[<>]' = '', 
                                                      '[}],[{]' = ' => ', 
                                                      '[{}]' = '', 
                                                      ',' = ' | '))
  
  # split sequence column into states
  len <- max(str_count(seqdf$sequence, " => "))  # get number of transitions
  print(len)
  state_names <- paste0("c_", 1:(len + 1))       # assign state names
  states <- separate(seqdf[-2], sequence, state_names, " => ", fill = 'right')
  seqdf <- cbind(sequence = seqdf$sequence, support = seqdf$support, states)
  
  # flag 4 different types of repeats within sequences and add length of sequence
  if(len == 1) {
    
    # add length of sequence and make each step it's own column
    seqdf <- seqdf %>%
      mutate(length2 = ifelse(complete.cases(seqdf$c_1, seqdf$c_2), 2, 0),
             length1 = ifelse(length2!=2 & complete.cases(seqdf$c_1), 1, 0),
             length = length1 + length2) %>%
      select(sequence, support, c_1, c_2, length)
    
    # repeats
    seqdf <- replace(seqdf, is.na(seqdf), 0) # replace NAs with 0s for logical statements to work
    seqdf <- seqdf %>%
      mutate(consec = ifelse(seqdf$c_1 == seqdf$c_2 & (seqdf$c_1!=0 & seqdf$c_2!=0), 1, 0),
             
             non_consec = 0,
             
             embed_consec = ifelse(((str_detect(seqdf$c_1, seqdf$c_2) | str_detect(seqdf$c_2, seqdf$c_1)) & !(seqdf$c_1 == seqdf$c_2)), 1, 0),
             
             embed_non_consec = 0) %>%
      select(sequence, support, c_1, c_2, length, consec, non_consec, embed_consec, embed_non_consec)
    
  } else if(len == 2) {

    # add length of sequence and make each step it's own column
    seqdf <- seqdf %>%
      mutate(length3 = ifelse(complete.cases(seqdf$c_1, seqdf$c_2, seqdf$c_3), 3, 0),
             length2 = ifelse(length3!=3 & complete.cases(seqdf$c_1, seqdf$c_2), 2, 0),
             length1 = ifelse(length3!=3 & length2!=2 & complete.cases(seqdf$c_1), 1, 0),
             length = length1 + length2 + length3) %>%
      select(sequence, support, c_1, c_2, c_3, length)

    # repeats
    seqdf <- replace(seqdf, is.na(seqdf), 0) # replace NAs with 0s for logical statements to work
    seqdf <- seqdf %>%
      mutate(consec = ifelse(seqdf$c_1 == seqdf$c_2 & (seqdf$c_1!=0 & seqdf$c_2!=0) |
                               seqdf$c_2 == seqdf$c_3 & (seqdf$c_2!=0 & seqdf$c_3!=0), 1, 0),

             non_consec = ifelse(seqdf$c_1 == seqdf$c_3 & !(consec == 1), 1, 0),

             embed_consec = ifelse(((str_detect(seqdf$c_1, seqdf$c_2) | str_detect(seqdf$c_2, seqdf$c_1)) & !(seqdf$c_1 == seqdf$c_2)) |
                                     ((str_detect(seqdf$c_2, seqdf$c_3) | str_detect(seqdf$c_3, seqdf$c_2)) & !(seqdf$c_2 == seqdf$c_3)), 1, 0),

             embed_non_consec = ifelse((str_detect(seqdf$c_1, seqdf$c_3) | str_detect(seqdf$c_3, seqdf$c_1)) &
                                         !(seqdf$c_1 == seqdf$c_3) & !(embed_consec == 1), 1, 0) ) %>%
      select(sequence, support, c_1, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec)

  } else if (len == 3) {

    # add length of sequence and make each step it's own column
    seqdf <- seqdf %>%
      mutate(length4 = ifelse(complete.cases(seqdf$c_1, seqdf$c_2, seqdf$c_3, seqdf$c_4), 4, 0),
             length3 = ifelse(length4!=4 & complete.cases(seqdf$c_1, seqdf$c_2, seqdf$c_3), 3, 0),
             length2 = ifelse(length4!=4 & length3!=3 & complete.cases(seqdf$c_1, seqdf$c_2), 2, 0),
             length1 = ifelse(length4!=4 & length3!=3 & length2!=2 & complete.cases(seqdf$c_1), 1, 0),
             length = length1 + length2 + length3) %>%
      select(sequence, support, c_1, c_2, c_3, c_4, length)

    # repeats
    seqdf <- replace(seqdf, is.na(seqdf), 0) # replace NAs with 0s for logical statements to work
    seqdf <- seqdf %>%
      mutate(consec = ifelse(seqdf$c_1 == seqdf$c_2 & (seqdf$c_1!=0 & seqdf$c_2!=0) |
                               seqdf$c_2 == seqdf$c_3 & (seqdf$c_2!=0 & seqdf$c_3!=0) |
                               seqdf$c_3 == seqdf$c_4 & (seqdf$c_3!=0 & seqdf$c_4!=0), 1, 0),

             non_consec = ifelse((seqdf$c_1 == seqdf$c_3 & !(consec == 1)) |
                                   (seqdf$c_2 == seqdf$c_4 & !(consec == 1)), 1, 0),

             embed_consec = ifelse(((str_detect(seqdf$c_1, seqdf$c_2) | str_detect(seqdf$c_2, seqdf$c_1)) & !(seqdf$c_1 == seqdf$c_2)) |
                                     ((str_detect(seqdf$c_2, seqdf$c_3) | str_detect(seqdf$c_3, seqdf$c_2)) & !(seqdf$c_2 == seqdf$c_3)) |
                                     ((str_detect(seqdf$c_3, seqdf$c_4) | str_detect(seqdf$c_4, seqdf$c_3)) & !(seqdf$c_3 == seqdf$c_4)), 1, 0),

             embed_non_consec = ifelse(((str_detect(seqdf$c_1, seqdf$c_3) | str_detect(seqdf$c_3, seqdf$c_1)) &
                                          !(seqdf$c_1 == seqdf$c_3) & !(embed_consec == 1)) |
                                         ((str_detect(seqdf$c_2, seqdf$c_4) | str_detect(seqdf$c_4, seqdf$c_2)) &
                                            !(seqdf$c_1 == seqdf$c_3) & !(embed_consec == 1)) |
                                         ((str_detect(seqdf$c_1, seqdf$c_4) | str_detect(seqdf$c_4, seqdf$c_1)) &
                                            !(seqdf$c_1 == seqdf$c_4) & !(embed_consec == 1)), 1, 0) ) %>%
      select(sequence, support, c_1, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec)
  }

  ### remove sequences with "|", indicating multiple items for an event in a sequence
  seqdf <- filter(seqdf, !grepl('[|]', sequence))

  ### remove sequences with length = 1 and sequences with any type of repeat
  # find rows with duplicates across 'c_*' columns and remove
  if(len == 3) {
    seqdf <- seqdf %>%
      rowwise() %>%
      mutate(repeats = ifelse(sum(duplicated(c(c_1,c_2,c_3,c_4), incomparables = NA)) == 0, 0, 1)) %>%
      filter(repeats == 0) %>%
      select(-repeats)
  } else if(len == 2) {
    seqdf <- seqdf %>%
      rowwise() %>%
      mutate(repeats = ifelse(sum(duplicated(c(c_1,c_2,c_3), incomparables = NA)) == 0, 0, 1)) %>%
      filter(repeats == 0) %>%
      select(-repeats)
  } else if(len == 1) {
    seqdf <- seqdf %>%
      rowwise() %>%
      mutate(repeats = ifelse(sum(duplicated(c(c_1,c_2), incomparables = NA)) == 0, 0, 1)) %>%
      filter(repeats == 0) %>%
      select(-repeats)
  }

  ### replace codes with descriptions
  if(len == 3) {
    if(input$seq_grp_lvl == 'icd') {
    seqdf <- seqdf %>%
      left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_1' = 'icd_code')) %>%
      select(sequence, support, c_1 = icd_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_2' = 'icd_code')) %>%
      select(sequence, support, c_1, c_2 = icd_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_3' = 'icd_code')) %>%
      select(sequence, support, c_1, c_2, c_3 = icd_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_4' = 'icd_code')) %>%
      select(sequence, support, c_1, c_2, c_3, c_4 = icd_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  } else if(input$seq_grp_lvl == 'icd_c') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_1' = 'icd_chp_code')) %>%
        select(sequence, support, c_1 = icd_chp_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_2' = 'icd_chp_code')) %>%
        select(sequence, support, c_1, c_2 = icd_chp_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_3' = 'icd_chp_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = icd_chp_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_4' = 'icd_chp_code')) %>%
        select(sequence, support, c_1, c_2, c_3, c_4 = icd_chp_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  } else if(input$seq_grp_lvl == 'ccs_s') {
    seqdf <- seqdf %>%
      left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_1' = 'ccs_code')) %>%
      select(sequence, support, c_1 = ccs_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_2' = 'ccs_code')) %>%
      select(sequence, support, c_1, c_2 = ccs_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_3' = 'ccs_code')) %>%
      select(sequence, support, c_1, c_2, c_3 = ccs_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_4' = 'ccs_code')) %>%
      select(sequence, support, c_1, c_2, c_3, c_4 = ccs_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  } else if(input$seq_grp_lvl == 'ccs1') {
    seqdf <- seqdf %>%
      left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_1' = 'ccs1_code')) %>%
      select(sequence, support, c_1 = ccs1_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_2' = 'ccs1_code')) %>%
      select(sequence, support, c_1, c_2 = ccs1_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_3' = 'ccs1_code')) %>%
      select(sequence, support, c_1, c_2, c_3 = ccs1_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_4' = 'ccs1_code')) %>%
      select(sequence, support, c_1, c_2, c_3, c_4 = ccs1_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  } else if(input$seq_grp_lvl == 'ccs2') {
    seqdf <- seqdf %>%
      left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_1' = 'ccs2_code')) %>%
      select(sequence, support, c_1 = ccs2_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_2' = 'ccs2_code')) %>%
      select(sequence, support, c_1, c_2 = ccs2_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_3' = 'ccs2_code')) %>%
      select(sequence, support, c_1, c_2, c_3 = ccs2_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_4' = 'ccs2_code')) %>%
      select(sequence, support, c_1, c_2, c_3, c_4 = ccs2_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  } else if(input$seq_grp_lvl == 'ccs3') {
    seqdf <- seqdf %>%
      left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_1' = 'ccs3_code')) %>%
      select(sequence, support, c_1 = ccs3_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_2' = 'ccs3_code')) %>%
      select(sequence, support, c_1, c_2 = ccs3_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_3' = 'ccs3_code')) %>%
      select(sequence, support, c_1, c_2, c_3 = ccs3_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_4' = 'ccs3_code')) %>%
      select(sequence, support, c_1, c_2, c_3, c_4 = ccs3_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  } else if(input$seq_grp_lvl == 'phewas') {
    seqdf <- seqdf %>%
      left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_1' = 'phewas_code')) %>%
      select(sequence, support, c_1 = phewas_desc, c_2, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_2' = 'phewas_code')) %>%
      select(sequence, support, c_1, c_2 = phewas_desc, c_3, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_3' = 'phewas_code')) %>%
      select(sequence, support, c_1, c_2, c_3 = phewas_desc, c_4, length, consec, non_consec, embed_consec, embed_non_consec) %>%
      left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_4' = 'phewas_code')) %>%
      select(sequence, support, c_1, c_2, c_3, c_4 = phewas_desc, length, consec, non_consec, embed_consec, embed_non_consec)
  }
  } else if(len == 2) {
    if(input$seq_grp_lvl == 'icd') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_1' = 'icd_code')) %>%
        select(sequence, support, c_1 = icd_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_2' = 'icd_code')) %>%
        select(sequence, support, c_1, c_2 = icd_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_3' = 'icd_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = icd_desc, length, consec, non_consec, embed_consec, embed_non_consec)
    } else if(input$seq_grp_lvl == 'icd_c') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_1' = 'icd_chp_code')) %>%
        select(sequence, support, c_1 = icd_chp_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_2' = 'icd_chp_code')) %>%
        select(sequence, support, c_1, c_2 = icd_chp_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_3' = 'icd_chp_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = icd_chp_desc, length, consec, non_consec, embed_consec, embed_non_consec)
    } else if(input$seq_grp_lvl == 'ccs') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_1' = 'ccs_code')) %>%
        select(sequence, support, c_1 = ccs_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_2' = 'ccs_code')) %>%
        select(sequence, support, c_1, c_2 = ccs_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_3' = 'ccs_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = ccs_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'ccs1') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_1' = 'ccs1_code')) %>%
        select(sequence, support, c_1 = ccs1_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_2' = 'ccs1_code')) %>%
        select(sequence, support, c_1, c_2 = ccs1_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_3' = 'ccs1_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = ccs1_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'ccs2') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_1' = 'ccs2_code')) %>%
        select(sequence, support, c_1 = ccs2_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_2' = 'ccs2_code')) %>%
        select(sequence, support, c_1, c_2 = ccs2_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_3' = 'ccs2_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = ccs2_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'ccs3') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_1' = 'ccs3_code')) %>%
        select(sequence, support, c_1 = ccs3_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_2' = 'ccs3_code')) %>%
        select(sequence, support, c_1, c_2 = ccs3_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_3' = 'ccs3_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = ccs3_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'phewas') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_1' = 'phewas_code')) %>%
        select(sequence, support, c_1 = phewas_desc, c_2, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_2' = 'phewas_code')) %>%
        select(sequence, support, c_1, c_2 = phewas_desc, c_3, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_3' = 'phewas_code')) %>%
        select(sequence, support, c_1, c_2, c_3 = phewas_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    }
  } else if(len == 1) {
    if(input$seq_grp_lvl == 'icd') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_1' = 'icd_code')) %>%
        select(sequence, support, c_1 = icd_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_code, icd_desc)), by = c('c_2' = 'icd_code')) %>%
        select(sequence, support, c_1, c_2 = icd_desc, length, consec, non_consec, embed_consec, embed_non_consec)
    } else if(input$seq_grp_lvl == 'icd_c') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_1' = 'icd_chp_code')) %>%
        select(sequence, support, c_1 = icd_chp_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), icd_chp_code, icd_chp_desc)), by = c('c_2' = 'icd_chp_code')) %>%
        select(sequence, support, c_1, c_2 = icd_chp_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'ccs') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_1' = 'ccs_code')) %>%
        select(sequence, support, c_1 = ccs_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs_code, ccs_desc)), by = c('c_2' = 'ccs_code')) %>%
        select(sequence, support, c_1, c_2 = ccs_desc, length, consec, non_consec, embed_consec, embed_non_consec)
    } else if(input$seq_grp_lvl == 'ccs1') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_1' = 'ccs1_code')) %>%
        select(sequence, support, c_1 = ccs1_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs1_code, ccs1_desc)), by = c('c_2' = 'ccs1_code')) %>%
        select(sequence, support, c_1, c_2 = ccs1_desc, length, consec, non_consec, embed_consec, embed_non_consec)
    } else if(input$seq_grp_lvl == 'ccs2') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_1' = 'ccs2_code')) %>%
        select(sequence, support, c_1 = ccs2_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs2_code, ccs2_desc)), by = c('c_2' = 'ccs2_code')) %>%
        select(sequence, support, c_1, c_2 = ccs2_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'ccs3') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_1' = 'ccs3_code')) %>%
        select(sequence, support, c_1 = ccs3_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), ccs3_code, ccs3_desc)), by = c('c_2' = 'ccs3_code')) %>%
        select(sequence, support, c_1, c_2 = ccs3_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    } else if(input$seq_grp_lvl == 'phewas') {
      seqdf <- seqdf %>%
        left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_1' = 'phewas_code')) %>%
        select(sequence, support, c_1 = phewas_desc, c_2, length, consec, non_consec, embed_consec, embed_non_consec) %>%
        left_join(distinct(select(categs(), phewas_code, phewas_desc)), by = c('c_2' = 'phewas_code')) %>%
        select(sequence, support, c_1, c_2 = phewas_desc, length, consec, non_consec, embed_consec, embed_non_consec) 
    }
  }
  
  ### remove sequences containing unclassified codes 
  if(len == 3) {
    seqdf <- seqdf %>%
      filter(!grepl('[Uu]nclassified', c_1) & !grepl('[Uu]nclassified', c_2) & !grepl('[Uu]nclassified', c_3) & !grepl('[Uu]nclassified', c_4))
  } else if(len == 2) {
    seqdf <- seqdf %>%
      filter(!grepl('[Uu]nclassified', c_1) & !grepl('[Uu]nclassified', c_2) & !grepl('[Uu]nclassified', c_3))
  } else if(len == 1) {
    seqdf <- seqdf %>%
      filter(!grepl('[Uu]nclassified', c_1) & !grepl('[Uu]nclassified', c_2))
  }
  
  ### select sequences that start with regular expression
  input$startsWithButton
  
  seqdf <- isolate(seqdf %>%
    filter(grepl(input$startsWith, c_1)))
  
  ### select sequences that contain regular expression
  input$containsButton
  isolate(
    if(len == 3) {
      seqdf <- seqdf %>%
        filter(grepl(input$contains, c_1) | grepl(input$contains, c_2) | grepl(input$contains, c_3) | grepl(input$contains, c_4))
    } else if(len == 2) {
      seqdf <- seqdf %>%
        filter(grepl(input$contains, c_1) | grepl(input$contains, c_2) | grepl(input$contains, c_3))
    } else if(len == 1) {
      seqdf <- seqdf %>%
        filter(grepl(input$contains, c_1) | grepl(input$contains, c_2))
    }
  )

  #---------------------------------------df_plot---------------------------------------#
  ### remove "sequence" column
  seqdfViz <- seqdf %>%
    select(-sequence)

  ### process seqdfViz for Google Vis
  seqdfViz <- data.frame(seqdfViz)
  df_plot <- data.frame()

  for (j in 3:(ncol(seqdfViz)-5)) {
    seq_cache <- seqdfViz %>%
      group_by(seqdfViz[,j-1], seqdfViz[,j]) %>%
      summarise(max_support = max(support))
    colnames(seq_cache)[1:2] <- c('from', 'to')

    for (i in 1:nrow(seq_cache)) {
      if(!(sum(is.na(seq_cache[i,])))>=1){
        # add tag indicating which state - needs to uniquely identify each link
        # support
        #seq_cache[i,]$from <- paste0(seq_cache[i,]$from, ' (', seq_cache[i,]$max_support + (j-2)* 0.000001, ')')
        #seq_cache[i,]$to <- paste0(seq_cache[i,]$to, ' (', seq_cache[i,]$max_support + (j-1) * 0.000001, ')')
        # state
        seq_cache[i,]$from <- paste0(seq_cache[i,]$from, ' (', j-2, ')')
        seq_cache[i,]$to <- paste0(seq_cache[i,]$to, ' (', j-1, ')')
        df_plot <- rbind(df_plot, seq_cache[i,])
      }}}
  
  list(seqdfTable = seqdf, seqdfViz = df_plot)
  
})

output$sequencesTable <- renderDataTable({
  
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   get_seqs()$seqdfTable
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  
  get_seqs()$seqdfTable
})


output$sankey <- renderGvis({
  
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   gvisSankey(get_seqs()$seqdfViz, from = 'from', to = 'to', weight = 'max_support',
                              options = list(height = 850,
                                             width = 850,
                                             sankey = "{
                                             link: {color: {
                                             fill: 'lightblue'}},
                                             node: {label: {
                                             fontName: 'Times',
                                             fontSize: 11,
                                             bold: true},
                                             interactivity: false,
                                             width: 6}}")) 
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  
  gvisSankey(get_seqs()$seqdfViz, from = 'from', to = 'to', weight = 'max_support',
                                       options = list(height = 850,
                                                      width = 850,
                                                      sankey = "{
                                                      link: {color: {
                                                      fill: 'lightblue'}},
                                                      node: {label: {
                                                      fontName: 'Times',
                                                      fontSize: 11,
                                                      bold: true},
                                                      interactivity: false,
                                                      width: 6}}"))
  })
       






# ======================================================================================================

# TODO: join codes with descriptions and add switch to view codes or descriptions
### Association Rules
rules <- reactive({
 # get filename
  fname <- ifelse(input$interaction == 'Condition 1 + 2',
                  paste0(csv_dir, gsub(' - .*', '', input$code), '_plus_', gsub(' - .*', '', input$code2), '_association.txt'),
                  paste0(csv_dir, gsub(' - .*', '', input$code), '_minus_', gsub(' - .*', '', input$code2), '_association.txt'))  
  
  
 # convert data to transactions format
 trans <- read.transactions(fname, format = 'basket', sep = ',', cols = 1, rm.duplicates = T)
 
 # item frequency plot with filters by support and top N
 #itemFrequencyPlot(trans, cex.names = 0.7, support = input$support, topN = 40)
 
 # mine association rules with apriori algorithm
 rules <- apriori(trans,
                  parameter = list(supp = input$supportA, conf = 0.8, target = 'rules', maxlen = 10,  # defaults
                                   minlen = 2))  # filter out rules with only one item (i.e., an empty antecent/LHS like {} => {beer})
 
 list(trans = trans, rules = rules)
 
})

# Grouped Matrix Plot - clusters LHS together using k means clustering, such that those with same RHS are collapsed on the x-axis
matrixPlot <- reactive({
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
  plot(rules()$rules, method = 'grouped', control = list(k = input$k))
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
})

graphPlot <- reactive({
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
  subrules <- head(sort(rules()$rules, by = input$sortBy), input$topN)
  
  plot(subrules, method = 'graph', measure = input$sortBy)
  incProgress(1/15)
  Sys.sleep(0.25)
                 }
               })
})

output$rulesPlot <- renderPlot(itemFrequencyPlot(rules()$trans, support = input$supportA, topN = input$topN))

output$rulesMatrixPlot <- renderPlot(matrixPlot(), height = 800)

output$rulesGraphPlot <- renderPlot(graphPlot(), height = 800)


# ======================================================================================================

# end Shiny session when browser window is closed
session$onSessionEnded(stopApp)
  
})


