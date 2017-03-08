# ----------------------------------------|
# ashley s. lee                           |
# data science practice                   |
# brown university cis                    |
# 2017.01.29                              |
# brown center for biomedical informatics |
# ----------------------------------------|

# TOFIX: size of tables and plots

shinyUI(dashboardPage(
  title = "CMiner",
  skin = "red",
  dashboardHeader(title = "Comorbidity Miner", titleWidth = 300),
  
  dashboardSidebar(
    
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem(
        "Settings", tabName = "settings"
      ),
      
      menuItem(
        "Explore All Patients", tabName = "all_patients"
      ),
      
      menuItem(
        "Explore Cohort", tabName = "cohort_patients"
      ),
      
      menuItem(
        "Sequential Patterns", tabName = "sequence"
        ),
      
      menuItem(
        "Association Rules", tabName = "association"
        )
    
      )
    
      
    ),
  
  dashboardBody(
    tabItems(
      # cohort
      tabItem("settings",
              
              fluidRow(
                
              column(width = 12,
                     
                     box(width = 12,
                         selectInput("data_src",
                                     "Select data source:",
                                     choices = c('MIMIC III' = 'mimic',
                                                 'CDC Mortality' = 'cdc_mortality',
                                                 'AEOLUS' = 'aeolus'),
                                     selected = 'mimic')
                     )
              ),
              
               column(width = 6,
                
                 box(width = 12,
                     selectInput("grouping_lvl",
                                 "Select condition 1 granularity:",
                                choices = c('ICD' = 'icd', 
                                            'ICD Chapter' = 'icd_c', 
                                            'CCS Single-level' = 'ccs_s', 
                                            'CCS Multi-level 1' = 'ccs1', 
                                            'CCS Multi-level 2' = 'ccs2'),
                                selected = 'ccs2')
                  ),
                  
                  box(width = 12, 
                     selectInput("code",
                                 "Select condition 1:",
                                 choices = list("Loading..."))
                  ),
                  box(width = 12,
                      radioButtons("interaction",
                                   "Select condition interaction:",
                                   choices = c("Condition 1 + Condition 2", "Condition 1 - Condition 2"),
                                   selected = "Condition 1 + Condition 2")
                  ),
                  box(width = 12,
                      selectInput("seq_grp_lvl",
                                 "Select sequences/associations granularity:",
                                 choices = c('ICD' = 'icd', 
                                             'ICD Chapter' = 'icd_c', 
                                             'CCS Single-level' = 'ccs_s', 
                                             'CCS Multi-level 1' = 'ccs1', 
                                             'CCS Multi-level 2' = 'ccs2'),
                                 selected = 'ccs2')
                  )
               ),

                column(width = 6,
              
                  box(
                    width = 12,
                    selectInput("grouping_lvl2",
                                "Select condition 2 granularity:",
                                choices = c('ICD' = 'icd', 
                                            'ICD Chapter' = 'icd_c', 
                                            'CCS Single-level' = 'ccs_s', 
                                            'CCS Multi-level 1' = 'ccs1', 
                                            'CCS Multi-level 2' = 'ccs2'),
                                selected = 'ccs2')
                  ),
                  
                  box(
                    width=12,
                    selectInput("code2",
                                "Select condition 2:",
                                choices = list("Loading..."))
                  ),
                  
                  box(
                    width = 12,
                    radioButtons("min_two_visits",
                                 "Select only repeat visit patients?",
                                 choices = c(TRUE, FALSE),
                                 selected = TRUE)
                  ),
                  box(
                    width = 12,
                    h5("Write cohort to file with each new cohort selection."),
                    actionButton("genData", "Write Cohort to File")
                  )
                )
              )
      ),
      
      # all patients
      tabItem("all_patients",
              tabsetPanel(
                tabPanel('ICD Mapping', dataTableOutput("outGroupTable")),
                tabPanel('Condition Histogram', plotOutput("frequentCodesPlot"))
                )
              ),
              
      
      # cohort patients
      tabItem("cohort_patients",
              tabsetPanel(
                tabPanel("Demographics", dataTableOutput("demographicsTable")),
                tabPanel("All Unique Codes", dataTableOutput("patientsTable")),
                tabPanel("Condition Histogram", plotOutput("frequentPatientsPlot"))
                )
              ),
              
      
      # sequence
      tabItem("sequence",
              fluidRow(
                column(width = 3,
                      box(width = 12,
                          sliderInput("supportS",
                                       "Select minimum support:",
                                       min = 0.01,
                                       max = 1,
                                       value = 0.25,
                                       step = 0.01)
                                ),
                       box(
                           width = 12,
                           textInput("startsWith",
                                     "Subset sequences starting with
                                            (case sensitive):",
                                            value = NULL,
                                            placeholder = 'ex. Asthma|Mood'),
                                  actionButton("startsWithButton",
                                               "Submit Query")
                                ),
                                box(
                                  width = 12,
                                  textInput("contains",
                                            "Subset sequences containing
                                            (case sensitive):",
                                            value = NULL,
                                            placeholder = 'ex. Asthma|Mood'),
                                  actionButton("containsButton",
                                               "Submit Query")
                                )
                         ),
                column(width = 9,
                       tabsetPanel(
                         tabPanel("Sequences", dataTableOutput("sequencesTable")),
                         tabPanel("Sankey Diagram", htmlOutput("sankey"))
                       )
                       )
              )
      ),
      
      
      # association rule
      tabItem("association",
              fluidRow(
                column(width = 4, 
                      box(
                                  width = 12,
                                  sliderInput("supportA",
                                              "Select minimum support:",
                                              min = 0.01,
                                              max = 1,
                                              value = 0.1,
                                              step = 0.01)
                                ),
                                box(
                                  width = 12,
                                  sliderInput("topN",
                                              "Select top N rules:",
                                              min = 1,
                                              max = 40,
                                              value = 10,
                                              step = 1)
                                ),
                                box(
                                  width = 12,
                                  sliderInput("k",
                                              "Select k number of LHS clusters:",
                                              min = 1,
                                              max = 100,
                                              value = 10,
                                              step = 1)
                                ),
                                box(
                                  width = 12,
                                  radioButtons("sortBy",
                                               "Select sort statistic:",
                                               choices = c("lift", "confidence", "support"))
                                )
                         ),
                       column(width = 8,
                              tabsetPanel(
                                tabPanel("Frequent Items", plotOutput("rulesPlot")),
                                tabPanel("Arules", plotOutput("rulesMatrixPlot")),
                                tabPanel("ArulesGraph", plotOutput("rulesGraphPlot"))
                              )
                       )
                )
      )
    )
  )
)
)
