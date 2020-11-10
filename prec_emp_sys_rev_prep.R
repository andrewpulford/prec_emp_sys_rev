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
#mysheets <- read_excel_allsheets(filename = "./data/Prec_Emp_Data_Extract_20200729.xlsx") # previous version
mysheets <- read_excel_allsheets(filename = "./data/Prec_Emp_Data_Extract_20200825.xlsx")

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

extraction$id <- as.numeric(extraction$id)
rob$id <- as.numeric(rob$id)

## check study_desc and rob contain same records
sd_check <- study_desc %>% select(study_id, id, first_author)%>% unique()
rob_check <- rob %>% select(study_id, id, first_author)%>% unique()
sd_check %>% anti_join(rob_check)

ext_check <- extraction %>% select(study_id, id, first_author) %>% unique()

ext_check %>% anti_join(sd_check) %>% anti_join(rob)

rm(sd_check, rob_check, ext_check)

#------------------------------------------------------------------------------#
##### Extracted data - final set for synthesis 
#------------------------------------------------------------------------------#

#### descriptive information grouped by outcome domain, exposure domain, 
##### more specific outcome and exposure --------------------------------------
##### code befiore table 1 to dedup and get vector of final record ids

## select relevant variables from study description df
exp_df <- study_desc %>% 
  select(study_id, id, exposure_topic, study_population, study_design)
# join to extraction df
ext_fin <- extraction %>% left_join(exp_df, by = c("study_id", "id"))

## create dp identfier
ext_fin <- ext_fin %>%mutate(dp_id = paste0("dp",row_number())) %>% 
  select(c(dp_id, study_id, id, first_author, year_published, exposure_topic, outcome_cat, everything(),-mediators,-results_description))

## arrange df for checking for dups
ext_fin2 <- ext_fin %>% 
  arrange(study_id, exposure_topic, outcome_cat, year_published, first_author) %>% 
  select(c(dp_id, study_id, id, first_author, year_published, 
           sample_size, age_cat, sex,
           exposure_topic, exposure_group, comparator_group,
           outcome_cat, definition_of_outcome, outcome_type, study_design))

## create row numbers and total number of row per study
ext_fin2 <- ext_fin2 %>% 
group_by(study_id, exposure_topic, definition_of_outcome) %>% 
  mutate(dp_row = row_number(), 
         n_dp = n()) %>% 
  ungroup() 

# make study_id a factor var
ext_fin2$study_id <- factor(ext_fin2$study_id)

## split df into a list of separate df's by study
ext_fin_list <- ext_fin2 %>% group_split(study_id)

## set list names as study_id
names(ext_fin_list) <- levels(ext_fin2$study_id)

## this will add each df in list to global environment - not run for now
#list2env(ext_fin_list, .GlobalEnv)

write_xlsx(ext_fin_list, paste0("./data/working/table-2_dedup.xlsx"))

## load in the manually de-duped data 
########## NOTE - duplicate cases coded manually in Excel: dated 20201103
## Coding:
## 0 == duplicate (remove)
## 1 == keep
## 2 == duplicate (keep for sensitivity analysis)
manual_dup_list <- mysheets <- read_excel_allsheets(filename = "./data/working/table-2_dedup_20201103.xlsx")

manual_dup <- do.call(rbind.data.frame, manual_dup_list) #%>% 
#  select(study_id, first_author, 
#         year_published, age_cat, 
#         sex, exposure_group, comparator_group, 
#         definition_of_outcome, dup_flag)

## note records with no non-duplicate data points - to be excluded
manual_dup %>% group_by(study_id,id, first_author) %>% 
  summarise(total = sum(dup_flag)) %>% filter(total==0) 

## se;ect only vars needed for join back onto extraction df
manual_dup <- manual_dup %>% 
  select(c(dp_id, dup_flag))


## create final extraction df
ext_fin3 <- ext_fin %>% left_join(manual_dup) %>% 
  filter(dup_flag !=0)
##save??


ext_fin3$id <- factor(ext_fin3$id)

## create a vector of the final id numbers to be included in review
fin_id <- levels(ext_fin3$id)

## create final study_desc and rob df's
study_desc_fin <- study_desc %>% filter(id %in% fin_id)
rob_fin <- rob %>% filter(id %in% fin_id)

#------------------------------------------------------------------------------#
##### Table 1 - all grouped by outcome grouping, and study
#------------------------------------------------------------------------------#

#### high level overview by study for methods section ----------------

## drop data_source from study_desc_fin 
study_desc_fin <- study_desc_fin %>% select(-data_source_s)
## create vector of final study id's
study_id_fin <- unique(study_desc_fin$study_id)

## for rob keep only study id and global rating
rob_fin_global <- rob_fin %>% 
  filter(study_id %in% study_id_fin) %>% 
  select(c(study_id, id, global_rating))

table_1 <- sr_log %>% full_join(study_desc_fin, by = "study_id") %>% 
  full_join(rob_fin_global, by = c("study_id", "id")) %>% 
  select(c(study_id, id, data_source_s, first_author, year_published,
           countries_included_in_study, study_design, study_population, 
           global_rating, exposure_topic, outcome_topic_s)) %>% 
  filter(study_id != "SR030") %>% # remove as not in final set of studies
  arrange(data_source_s)
#### for some reason SR034 is not pulling through - check codes

write_xlsx(table_1, path = "output/table_1.xlsx")


#######################
table_2 <- table_2 %>% 
  #  filter(id %in% final_id) %>% # keep only papers in Table 1
  select(c(study_id, id, dp_id, first_author, year_published, 
           gen_health, mental_health, phys_health, health_behav,
           age_cat, sample_size, sex, study_population, 
           exposure_group, exposure_topic, 
           comparator_group, definition_of_outcome, study_design)) %>% 
  arrange(exposure_topic, first_author, year_published)

table_2$study_id <- factor(table_2$study_id)
table_2$age_cat <- factor(table_2$age_cat)
table_2$comparator_group <- factor(table_2$comparator_group)












##########################
table_2_gen <- table_2 %>% 
  filter(gen_health == 1) %>% 
  select(-c(4:6, 16:18)) 

table_2_mh <- table_2 %>% 
  filter(mental_health == 1) %>% 
  select(-c(4:6, 16:18)) 

table_2_phys <- table_2 %>% 
  filter(phys_health == 1) %>% 
  select(-c(4:6, 16:18)) 

write_xlsx(table_2_gen, path = "output/table_2_gen.xlsx")
write_xlsx(table_2_mh, path = "output/table_2_mh.xlsx")
write_xlsx(table_2_phys, path = "output/table_2_phys.xlsx")

## create flag for dp's to include
table_2$include <- 1
# remove unneccesary vars
table_2 <- table_2 %>% select(-c(dp_row, n_dp, dup_flag))

## create dataframe of data points for use in main analysis
## needs a bit of checking re missing values
dp_review <- table_2 %>% 
  left_join(extraction) %>% 
  filter(include == 1)



#------------------------------------------------------------------------------#
##### Figure 1 - number of data points by exposure topic
#------------------------------------------------------------------------------#

##### Mental health --------------

## create a temporary df with all exposure/outcome combinations
df_temp <- expand.grid(table_2_mh$exposure_topic, table_2_mh$outcome_cat)
names(df_temp) <- c("exposure_topic", "outcome_cat")
df_temp$data_points <- 0
  

table_2_mh <- read_xlsx("./output/table_2_mh_20200824.xlsx")

table_2_mh <- table_2_mh %>% select(-c(sex, definition_of_outcome)) %>% 
  group_by(exposure_topic, outcome_cat, study_design) %>% 
  summarise(data_points = sum(data_points)) %>% 
  ungroup() %>% 
  arrange(desc(data_points))

fig1_mh <- table_2_mh %>%
  select(-study_design) %>%
  bind_rows(df_temp) %>% 
  group_by(exposure_topic, outcome_cat) %>% 
  summarise(data_points = sum(data_points)) %>% 
  ungroup()
  
fig1_mh %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
  geom_tile(col="grey") +
  coord_fixed() +
  theme_classic()+
  scale_fill_gradient(low="white", high="red", name="Number of data points",
                      labels = c(1, 3, 5, 7, 9),
                      breaks = c(1, 3, 5, 7, 9)) +
  labs(x = "Outcome category", y = "Exposure category") +
  theme(axis.line.y=element_blank(),
        axis.line.x=element_blank(),
        plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        legend.position = "left",
        legend.justification = "top",
        axis.text.x=element_text(colour="Black", angle = 45, hjust = 1))  

################################################################################
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
