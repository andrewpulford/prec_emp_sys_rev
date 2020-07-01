#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#####       Precarious Employment Systematic Review Data Preparation       #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


## remove any existing objects from global environment
rm(list=ls()) 

## install packages
library(tidyverse) # all kinds of stuff 
library(stringr) # for strings
library(readxl) # for reading excel file and all data sheets
library(writexl)
library(janitor) # for sorting out variable names etc


### open extracted data
## this function will read all excel sheets as a list
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

## call function with extracted data
mysheets <- read_excel_allsheets(filename = "./data/Prec_Emp_Data_Extract_20200623.xlsx")

## covert list into dataframes in global environment
list2env(mysheets, .GlobalEnv)

## rename df's
sr_log <- `SR log alpha` %>% as_tibble() %>% clean_names()
study_desc <- `Study Description` %>% as_tibble() %>% clean_names()
extraction <- Extraction %>% as_tibble() %>% clean_names()
rob <- `Risk of Bias (EPHPP)` %>% as_tibble() %>% clean_names()
  
names(sr_log)
names(study_desc)
names(extraction)

#------------------------------------------------------------------------------#
##### Table 1 - all grouped by outcomce grouping, and study
#------------------------------------------------------------------------------#

## high level overview by study for methds section

table_1 <- sr_log %>% full_join(study_desc, by = "study_record_id") %>% 
  full_join(rob, by = "id") %>% 
  select(c(study_record_id, id, data_source_s.x, first_author.x, year_published.x,
           countries_included_in_study, study_design.x, study_population, 
           global_rating, exposure_topic, outcome_topic_s)) %>% 
  group_by(study_record_id) %>% 
  mutate(study_row = row_number()) %>% 
  ungroup() %>% 
  select(-c(study_record_id, id))

write_xlsx(table_1, path = "output/table_1.xlsx")


#------------------------------------------------------------------------------#
##### Table 2 - 
#------------------------------------------------------------------------------#

## create flags for each outcome group
study_desc <- study_desc %>% mutate(gen_health = 
                        ifelse(str_detect(outcome_topic_s, "General health"),
                               1,0),
                      mental_health = 
                        ifelse(str_detect(outcome_topic_s, "Mental health"),
                               1,0),
                      phys_health = ifelse(str_detect(outcome_topic_s, "Physical health"),
                                           1,0))


## create flags for each exposure group
study_desc$exposure_topic <- factor(study_desc$exposure_topic)

#"Employment contract"    
#"Employment spells"      
#"Income volatility"      
#"Layoff contact"         
#"Multiple"               
#"Perceived job security"
#"Underemployment"

#------------------------------------------------------------------------------#
#####                              Mental health                           #####
#------------------------------------------------------------------------------#

MH <- MH %>% as_tibble(MH) %>% clean_names()

MH <- MH %>% arrange(study_id, id, sex)

MH$first_author <- factor(MH$first_author)

nrow(MH)  ## number of data points

## data points per study
mh_study_id <- MH %>% group_by(study_id) %>% summarise(data_points = n())

## data points per sex group
mh_sex <- MH %>% group_by(sex) %>% summarise(data_points = n())

## data points per study and sex
mh_sudyid_sex <- MH %>% group_by(study_id, sex) %>% summarise(data_points = n())

## data points per exposure
mh_exposure <- MH %>% group_by(exposure_group) %>% summarise(data_points = n())

## data points per outcome metric
mh_oucomemet <- MH %>% group_by(outcome_measure) %>% summarise(data_points = n())

## data points per putcome definition
mh_oucomedef <- MH %>% group_by(definition_of_outcome) %>% summarise(data_points = n())

## list 
mh_list <- list(mh_study_id, mh_sex, mh_sudyid_sex, mh_exposure, mh_oucomemet, mh_oucomedef)

## write list as Excel workbook
write_xlsx(mh_list,
           path = "output/mh_summary_tables.xlsx")

######################

MH$estimate <-  as.numeric(MH$estimate)

## plot OR results
MH %>% filter(outcome_measure == "OR" | outcome_measure == "AOR") %>% 
  ggplot(aes(x = estimate, y= first_author)) +
  geom_point() +
  xlim(0,2) + 
  geom_vline(aes(xintercept=1)) +
  facet_wrap(~sex)


## plot regression coefficient results
MH %>% filter(outcome_measure == "Regression coeffiecient") %>% 
  ggplot(aes(x = estimate, y= first_author)) +  geom_point() +
  geom_vline(aes(xintercept=1)) +
  facet_wrap(~sex)

MH %>% group_by(exposure_group) %>%  summarise(data_points = n())
