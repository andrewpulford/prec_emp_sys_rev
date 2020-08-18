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
mysheets <- read_excel_allsheets(filename = "./data/Prec_Emp_Data_Extract_20200729.xlsx")

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

## get study_id name consistent across df's
sr_log <- sr_log %>% rename("study_id" = "study_record_id")
study_desc <- study_desc %>% rename("study_id" = "study_record_id")


rm(mysheets, `SR log`, `SR log alpha`, Extraction, `Study Description`, `Risk of Bias (EPHPP)`)
rm(MH, `Notes - study description`, `Notes - extraction`, `Notes - risk of bias`)

#------------------------------------------------------------------------------#
##### Table 1 - all grouped by outcome grouping, and study
#------------------------------------------------------------------------------#

#### high level overview by study for methods section ----------------

table_1_raw <- sr_log %>% full_join(study_desc, by = "study_id") %>% 
  full_join(rob, by = c("study_id", "id")) %>% 
  select(c(study_id, id, data_source_s.x, first_author.x, year_published.x,
           countries_included_in_study, study_design.x, study_population, 
           global_rating, exposure_topic, outcome_topic_s)) %>% 
  group_by(study_id) %>% 
  mutate(study_row = row_number(), 
         n_studies = n()) %>% 
  ungroup() %>% 
  rename("study_id" = "study_id")


#### check studies with multiple papers  <<< here <<<
table_1_raw <- table_1_raw 

## temp df with vars needed from extraction template
ext_temp <- extraction %>% select(c(study_id, id, sample_size))
  
## temp df with studies that feature only once
table_1_single <- table_1_raw %>% 
  filter(n_studies == 1)%>% 
  select(-c(study_row, n_studies))

## temp df with studies that feature more than once
table_1_multiple <- table_1_raw %>% 
  filter(n_studies>1) %>% 
  left_join(ext_temp)  

## temp df with studies to be retained (largest sample size)
table_1_keep <- table_1_multiple %>% 
  group_by(study_id) %>% 
  slice(which.max(sample_size)) %>% 
  ungroup() %>% 
  select(-c(study_row, n_studies, sample_size)) %>% 
  unique()

## temp df with papers to be excluded as duplicates
table_1_dups <- table_1_multiple %>% 
  anti_join(table_1_keep) %>% 
  select(-c(study_row, n_studies, sample_size)) %>% 
  unique()

## merge singles and jkeeps to create final table 1
table_1 <- table_1_single %>% bind_rows(table_1_keep) %>% 
  filter(study_id != "SR030") %>% # remove as not in final set of studies
  arrange(data_source_s.x)

table_1

#write_xlsx(table_1, path = "output/table_1.xlsx")

#final_id <- table_1$id # vector of ID numbers for final analysis

#------------------------------------------------------------------------------#
##### Table 2 - 
#------------------------------------------------------------------------------#

#### descriptive information grouped by outcome domain, exposure domain, 
##### more specific outcome and exposure --------------------------------------

## create flags for each outcome group
table_2_raw <- extraction %>% mutate(gen_health = 
                        ifelse(str_detect(outcome_topic_s, "General health"),
                               1,0),
                      mental_health = 
                        ifelse(str_detect(outcome_topic_s, "Mental health"),
                               1,0),
                      phys_health = ifelse(str_detect(outcome_topic_s, "Physical health"),
                                           1,0))


## create flags for each exposure group

exp_df <- study_desc %>% 
  select(id, exposure_topic, study_population, study_design)

table_2_raw <- table_2_raw %>% left_join(exp_df)

table_2_raw$exposure_topic <- factor(table_2_raw$exposure_topic)

table_2_raw <- table_2_raw %>% 
  mutate(emp_contract = ifelse(str_detect(exposure_topic, 
                                          "Employment contract"),
                                 1,0),
         emp_spells = ifelse(str_detect(exposure_topic,
                                          "Employment spells"),
                                 1,0),
         inc_volatility = ifelse(str_detect(exposure_topic, 
                                            "Income volatility"),
                                 1,0),
         layoff_contact = ifelse(str_detect(exposure_topic,
                                            "Layoff contact"),
                                 1,0),
         multiple_exp = ifelse(str_detect(exposure_topic,
                                          "Multiple"),
                                 1,0),
         job_insecurity = ifelse(str_detect(exposure_topic,
                                            "Perceived job security"),
                                 1,0),
         underemployed = ifelse(str_detect(exposure_topic,
                                           "Underemployment"),
                                 1,0))


table_2 <- table_2_raw %>% 
#  filter(id %in% final_id) %>% # keep only papers in Table 1
  select(c(study_id, first_author, year_published, gen_health, mental_health, phys_health, 
           age_cat, sample_size, sex, study_population, exposure_group, exposure_topic, 
           comparator_group, definition_of_outcome, study_design)) %>% 
  arrange(exposure_topic, first_author, year_published)

table_2$study_id <- factor(table_2$study_id)
table_2$age_cat <- factor(table_2$age_cat)
table_2$comparator_group <- factor(table_2$comparator_group)


table_2 <- table_2 %>% group_by(study_id, definition_of_outcome) %>% 
  mutate(dp_row = row_number(), 
         n_dp = n()) %>% 
  ungroup() %>% 
  arrange(study_id, definition_of_outcome)

test <- table_2 %>% group_split(study_id)
names(test) <- levels(table_2$study_id)

lapply(names(test),function(x) assign(x,test[[x]],.GlobalEnv))

write_xlsx(test, paste0("./data/working/table-2_dedup.xlsx"))

########## NOTE - duplicate cases coded manually in Excel: dated 20200807
## Coding:
## 0 == keep
## 1 == duplicate (remove)
## 2 == duplicate for sensitivity analysis
## 3 == keep (pool sexes)

## call function with extracted data
mysheets2 <- read_excel_allsheets(filename = "./data/working/table-2_dedup_20200807.xlsx")

## covert list into dataframes in global environment - don't think needed
#list2env(mysheets2, .GlobalEnv)

## convert list in single df
tab2_dedup <- bind_rows(mysheets2)

## create df for dupliacte data points to be considered for sensitivity analysis/stratified 
## (dup_flag == 2 or 3)
tab2_sa <- tab2_dedup %>% filter(dup_flag == 2 | dup_flag == 3)
### add save command

## create main deduped table 2 df
tab2_dedup0 <- tab2_dedup %>% filter(dup_flag == 0) ## keep
tab2_dedup3 <- tab2_dedup %>% filter(dup_flag == 3) ## pool by sex before merging back into df

tab2_dedup3 <- tab2_dedup3 %>% 
  group_by(-c(sex, dp_row)) %>% 


##########################
table_2_gen <- table_2 %>% 
  filter(gen_health == 1) %>% 
  select(-c(3:5)) %>% 
  group_by(sex, exposure_topic, definition_of_outcome, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup()

table_2_mh <- table_2 %>% 
  filter(mental_health == 1) %>% 
  select(-c(3:5)) %>% 
  group_by(sex, exposure_topic, definition_of_outcome, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup()

table_2_phys <- table_2 %>% 
  filter(phys_health == 1) %>% 
  select(-c(3:5)) %>% 
  group_by(sex, exposure_topic, definition_of_outcome, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup()

write_xlsx(table_2_gen, path = "output/table_2_gen.xlsx")
write_xlsx(table_2_mh, path = "output/table_2_mh.xlsx")
write_xlsx(table_2_phys, path = "output/table_2_phys.xlsx")


#------------------------------------------------------------------------------#
##### Figure 1 - number of data points by exposure topic
#------------------------------------------------------------------------------#

study_desc_dedup <- study_desc %>% 
  filter(id %in% final_id) %>% # keep only papers in Table 1
  select(c(id, study_design, exposure_topic)) 

exc_dedup <- extraction %>% 
  filter(id %in% final_id) # keep only papers in Table 1
  
extraction1 <- exc_dedup %>% 
left_join(study_desc_dedup)

extraction1$exposure_topic <- factor(extraction1$exposure_topic)

extraction1 %>% 
  group_by(exposure_topic) %>% 
  mutate(dp_total = n()) %>% 
  ungroup() %>% 
  mutate(exposure_topic = fct_reorder(exposure_topic, dp_total)) %>% 
  group_by(exposure_topic, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=exposure_topic, y=data_points, col = study_design, fill = study_design)) + 
  geom_col() + 
  coord_flip()

#------------------------------------------------------------------------------#
##### Figure 2 - number of data points by outcome topic
#------------------------------------------------------------------------------#


extraction1 %>% group_by(outcome_topic_s, study_design) %>% 
  group_by(outcome_topic_s) %>% 
  mutate(dp_total = n()) %>% 
  ungroup() %>% 
  mutate(outcome_topic_s = fct_reorder(outcome_topic_s, dp_total)) %>% 
  group_by(outcome_topic_s, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=outcome_topic_s, y=data_points, col = study_design, fill = study_design)) + 
  geom_col() + 
  coord_flip()



################################################################################
################################################################################
################################################################################
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
