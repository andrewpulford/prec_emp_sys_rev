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

#------------------------------------------------------------------------------#
##### Mental health #####
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
