# comorbidity-miner
This repository contains the scripts necessary to run an R Shiny app that mines healthcare data for association rules and sequential patterns and allows exploratory analysis by healthcare workers and researchers using different concept heirarchies developed by the biomedical informatics community. The app currently supports MIMIC-III and AEOLUS data as well as ICD9-CM codes, CCS single- and multi-level categories, and SNOMED-CT concepts, but it can handle any patient-level data. CDC National Vital Statistics and ICD10-CM codes and mappings will be integrated in the near future. 


## Accessing the Data
Some of the publicly available data are provided, while restricted data and very large data will need to be downloaded. The app is data-input-agnostic, meaning that data can be loaded from CSV files or from a database connection. Supported databases include SQL, mySQL, PostgreSQL, and sqlite.

#### MIMIC-III
MIMIC contains de-identified hospital records and requires researchers to complete a CITI training course and formally request access to the database. Instructions to access MIMIC can be found [here](https://mimic.physionet.org/gettingstarted/access/).

#### AEOLUS 
AEOLUS is normalized FDA Adverse Event Reporting data and is to be used for public health research. AEOLUS CSV files and database loading instructions can be found [here](http://datadryad.org/resource/doi:10.5061/dryad.8q0s4/1).

#### Mapping files
The HCUP files that map ICD9-CM codes to CCS categories are provided in the `data/icd_grouping` directory. More information can be found [here](https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp).

Download the files that map ICD9-CM codes to SNOMED-CT concepts [here](https://www.nlm.nih.gov/research/umls/mapping_projects/icd9cm_to_snomedct.html).


## Running the app
The raw data are read into memory in Global.R, massaged in preprocessing scripts customized to each data source, and then fed through the rest of the R Shiny pipeline. If reading in data from CSVs, make sure the MIMIC data sit in `data/mimic_tables`, AEOLUS data sit in `data/aeolus_v1`, and the SNOMED-CT mapping files sit in `data/icd_grouping`. If connecting to databases, update the authentication information in `Global.R`: 

```
  # mimic database
  mimic <- src_mysql(dbname = 'mimic', host = 'localhost', user = 'mimic_user', password = 'mimic')
  
  # aeolus database
  aeolus_db <- src_mysql(dbname = 'aeolus', host = 'host', user = 'aeolus_user', password = 'aeolus')
  ```
Once the data are loaded, run the preprocessing scripts, which will create a `temporal` and `association` TXT file for each of your data sources in `data/input`. 

Initialize the app by clicking "Run App" in the top right corner of the editor panel in R Studio. It will take a few minutes to launch in your browser.


## Using the app

* A demo with simulated data will be coming in the near future.
