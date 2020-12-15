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
library(meta) # for meta analysis
library(metafor) # for meta analysis
# cant's get dmetar to install
#devtools::install_github("MathiasHarrer/dmetar")
#library(dmetar)  # for meta analysis

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

extraction <- extraction %>% 
  mutate(exposure_group=str_trim(exposure_group, side ="both"))

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
##### code before table 1 to dedup and get vector of final record ids

## select relevant variables from study description df
exp_df <- study_desc %>% 
  select(study_id, id, exposure_topic, study_population, study_design)
# join to extraction df
ext_fin <- extraction %>% left_join(exp_df, by = c("study_id", "id"))

## create dp identifier
ext_fin <- ext_fin %>%mutate(dp_id = paste0("dp",row_number())) %>% 
  select(c(dp_id, study_id, id, first_author, year_published, exposure_topic, outcome_cat, everything(),-mediators))

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
manual_dup_list <- mysheets <- read_excel_allsheets(filename = "./data/working/table-2_dedup_20201114.xlsx")

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
  select(c(dp_id, dup_flag, comparator_cat))


## create final extraction df
ext_fin3 <- ext_fin %>% left_join(manual_dup) %>% 
  filter(dup_flag !=0)
##save??

# convert id var to factor
ext_fin3$id <- factor(ext_fin3$id)

## create a vector of the final id numbers to be included in review
fin_id <- levels(ext_fin3$id)

## create primary synthesis extraction df
ext_fin3 <- ext_fin %>% left_join(manual_dup) %>% 
  filter(dup_flag !=0)
##save??

## create a vector of the  id numbers to be included in primary synthesis
ext_primary <- ext_fin3 %>% filter(dup_flag==1)

## create final study_desc and rob df's
study_desc_fin <- study_desc %>% filter(id %in% fin_id)
rob_fin <- rob %>% filter(id %in% fin_id)

## remove df's no longer needed
rm(study_desc, extraction, rob, mysheets, manual_dup_list, manual_dup,
   exp_df, ext_fin_list, ext_fin, ext_fin2)

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

write_xlsx(table_1, path = "output/table_1.xlsx")

#------------------------------------------------------------------------------#
##### Table 2 - GRADE and summary of findings
#------------------------------------------------------------------------------#


#######################

#table_2$study_id <- factor(table_2$study_id)
#table_2$age_cat <- factor(table_2$age_cat)
#table_2$comparator_group <- factor(table_2$comparator_group)

########################

#write_xlsx(table_2_gen, path = "output/table_2_gen.xlsx")
#write_xlsx(table_2_mh, path = "output/table_2_mh.xlsx")
#write_xlsx(table_2_phys, path = "output/table_2_phys.xlsx")



#------------------------------------------------------------------------------#
##### Figure 2 - number of data points by exposure topic/outcome category
#------------------------------------------------------------------------------#

fig_2_prep <- ext_primary %>% 
  filter(dup_flag==1) %>% # keep only non-dup dp's
  select(c(study_id, id, dp_id, first_author, year_published, 
           age_cat, sample_size, sex, study_population, 
           exposure_group, exposure_topic, comparator_group, 
           outcome_topic_s, definition_of_outcome, outcome_cat,
           outcome_type, study_design)) %>% 
  arrange(exposure_topic, first_author, year_published)


fig_2_gen <- fig_2_prep %>% 
  filter(outcome_topic_s == "General health") 

fig_2_mh <- fig_2_prep %>% 
  filter(outcome_topic_s == "Mental health") 

fig_2_phys <- fig_2_prep %>% 
  filter(outcome_topic_s == "Physical health")

fig_2_behav <- fig_2_prep %>% 
  filter(outcome_topic_s == "Health behaviours")

##### Genral health --------------

## create a temporary df with all exposure/outcome combinations
df_temp <- expand.grid(fig_2_gen$exposure_topic, fig_2_gen$outcome_cat)
names(df_temp) <- c("exposure_topic", "outcome_cat")
df_temp$data_points <- 0

fig_2_gen <- fig_2_gen %>% select(-c(sex, definition_of_outcome)) %>% 
  group_by(exposure_topic, outcome_cat, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup() %>% 
  arrange(desc(data_points))

max(fig_2_gen$data_points)

fig_2_gen <- fig_2_gen %>%
  select(-study_design) %>%
  bind_rows(df_temp) %>% 
  group_by(exposure_topic, outcome_cat) %>% 
  summarise(data_points = sum(data_points)) %>% 
  ungroup()

fig_2_gen_plot <- fig_2_gen %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
  geom_tile(col="grey") +
  geom_text(aes(label = data_points)) +
  coord_fixed() +
  theme_classic()+
  scale_fill_gradient(low="white", high="red", name="Number of data points",
                      labels = c(0,5,10,15),
                      breaks = c(0,5,10,15)) +
  labs(x = "Outcome category", y = "Exposure category") +
  theme(axis.line.y=element_blank(),
        axis.line.x=element_blank(),
        plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        legend.position = "left",
        legend.justification = "top",
        axis.text.x=element_text(colour="Black", angle = 45, hjust = 1))  

fig_2_gen_plot

#ggsave(fig_2_gen_plot, ".\prec_emp_sys_rev\charts")

##### Mental health --------------

## create a temporary df with all exposure/outcome combinations
df_temp <- expand.grid(fig_2_mh$exposure_topic, fig_2_mh$outcome_cat)
names(df_temp) <- c("exposure_topic", "outcome_cat")
df_temp$data_points <- 0


fig_2_mh <- fig_2_mh %>% select(-c(sex, definition_of_outcome)) %>% 
  group_by(exposure_topic, outcome_cat, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup() %>% 
  arrange(desc(data_points))

max(fig_2_mh$data_points)

fig_2_mh <- fig_2_mh %>%
  select(-study_design) %>%
  bind_rows(df_temp) %>% 
  group_by(exposure_topic, outcome_cat) %>% 
  summarise(data_points = sum(data_points)) %>% 
  ungroup()
  
fig_2_mh %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
  geom_tile(col="grey") +
  geom_text(aes(label = data_points)) +
  coord_fixed() +
  theme_classic()+
  scale_fill_gradient(low="white", high="red", name="Number of data points",
                      labels = c(0,5,10,15,20,25),
                      breaks = c(0,5,10,15,20,25)) +
  labs(x = "Outcome category", y = "Exposure category") +
  theme(axis.line.y=element_blank(),
        axis.line.x=element_blank(),
        plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        legend.position = "left",
        legend.justification = "top",
        axis.text.x=element_text(colour="Black", angle = 45, hjust = 1))  

##### Physical health --------------

## create a temporary df with all exposure/outcome combinations
df_temp <- expand.grid(fig_2_phys$exposure_topic, fig_2_phys$outcome_cat)
names(df_temp) <- c("exposure_topic", "outcome_cat")
df_temp$data_points <- 0



fig_2_phys <- fig_2_phys %>% select(-c(sex, definition_of_outcome)) %>% 
  group_by(exposure_topic, outcome_cat, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup() %>% 
  arrange(desc(data_points))

max(fig_2_phys$data_points)


fig_2_phys <- fig_2_phys %>%
  select(-study_design) %>%
  bind_rows(df_temp) %>% 
  group_by(exposure_topic, outcome_cat) %>% 
  summarise(data_points = sum(data_points)) %>% 
  ungroup()

fig_2_phys %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
  geom_tile(col="grey") +
  geom_text(aes(label = data_points)) +
  coord_fixed() +
  theme_classic()+
  scale_fill_gradient(low="white", high="red", name="Number of data points",
                      labels = c(0,5,10),
                      breaks = c(0,5,10)) +
  labs(x = "Outcome category", y = "Exposure category") +
  theme(axis.line.y=element_blank(),
        axis.line.x=element_blank(),
        plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        legend.position = "left",
        legend.justification = "top",
        axis.text.x=element_text(colour="Black", angle = 45, hjust = 1))  

##### Health behaviours --------------

## create a temporary df with all exposure/outcome combinations
df_temp <- expand.grid(fig_2_behav$exposure_topic, fig_2_behav$outcome_cat)
names(df_temp) <- c("exposure_topic", "outcome_cat")
df_temp$data_points <- 0



fig_2_behav <- fig_2_behav %>% select(-c(sex, definition_of_outcome)) %>% 
  group_by(exposure_topic, outcome_cat, study_design) %>% 
  summarise(data_points = n()) %>% 
  ungroup() %>% 
  arrange(desc(data_points))

max(fig_2_behav$data_points)


fig_2_behav <- fig_2_behav %>%
  select(-study_design) %>%
  bind_rows(df_temp) %>% 
  group_by(exposure_topic, outcome_cat) %>% 
  summarise(data_points = sum(data_points)) %>% 
  ungroup()

fig_2_behav %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
  geom_tile(col="grey") +
  geom_text(aes(label = data_points)) +
  coord_fixed() +
  theme_classic()+
  scale_fill_gradient(low="white", high="red", name="Number of data points",
                      labels = c(0,5,10),
                      breaks = c(0,5,10)) +
  labs(x = "Outcome category", y = "Exposure category") +
  theme(axis.line.y=element_blank(),
        axis.line.x=element_blank(),
        plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        legend.position = "left",
        legend.justification = "top",
        axis.text.x=element_text(colour="Black", angle = 45, hjust = 1))  

#assign(paste0("fig_1_test"), fig_1_mh)

#------------------------------------------------------------------------------#
##### Figure 3 - number of data points by exposure and outcome topic
#------------------------------------------------------------------------------#

### exposure -----------------
ext_primary$exposure_topic <- factor(ext_primary$exposure_topic)

ext_primary %>% 
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

### outcome ------------------------
ext_primary %>% group_by(outcome_topic_s, study_design) %>% 
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

#------------------------------------------------------------------------------#
##### Figure 4 - effect direction plots
#------------------------------------------------------------------------------#

#### need to check for missing estimate data 
ext_fin3 %>% filter(est_valid!=1) # currently 14 dps to be checked

eff_dir_df <- ext_primary %>% 
  mutate(direction = ifelse(results_description == "-1", "Better",
                            ifelse(results_description == "0", "Equivalent",
                                   ifelse(results_description == "1", "Worse", NA)))) %>%
  filter(!is.na(direction)) %>% 
  mutate(direction = factor(direction, levels = c("Better", "Equivalent", "Worse"))) %>% 
  arrange(exposure_topic, direction) %>% 
  group_by(exposure_topic) %>%
  mutate(ymax = n(),
         ypos = 1:ymax) 


table(eff_dir_df$direction, eff_dir_df$exposure_topic)

eff_dir_plot <- eff_dir_df %>% 
  ggplot(aes(x = exposure_topic, y = ypos, colour = direction)) + scale_colour_manual(values=c("darkgreen","gray58", "red4"), drop=F, name="Direction summary") + 
  geom_point(shape = 16, size = 6) + scale_y_continuous(limits=c(1,80), expand = c(0.05,0.05)) +
  coord_flip() + labs(x="Exposure category", y= "Number of data points")  + theme_classic()

eff_dir_plot


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#####                             Meta analysis                            #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
## https://ebmh.bmj.com/content/22/4/153
## https://www.researchgate.net/profile/Guido_Schwarzer/publication/283579105_Meta-Analysis_with_R/links/5710a4d008ae19b1869392a3.pdf
## https://cran.rstudio.org/doc/Rnews/Rnews_2007-3.pdf#page=40
## start thinking about PICOS combinations for analysis

#### need to check for missing data re significance
##    need point estimate and se; exact sample size not required
##    will be difficult to include non-sig estimates where exact or approx se not possible
## for formulae see: 
## https://handbook-5-1.cochrane.org/chapter_7/7_7_7_2_obtaining_standard_errors_from_confidence_intervals_and.htm
## https://handbook-5-1.cochrane.org/chapter_7/7_7_7_3_obtaining_standard_errors_from_confidence_intervals_and.htm


#------------------------------------------------------------------------------#
#### Grouping PECOS combinations for synthesis
#------------------------------------------------------------------------------#

## this pipeline groups dp's by PECOS configurations
## does not currently include population, outcome measure (eg OR, HR etc), or study type
## calculates the PECOS group number, row number within PECOS group 
## PECOS groups with 2 or more dp's can be considered for meta analysis (ma variable)
ext_primary <- ext_primary %>% group_by(outcome_cat, outcome_measure, # exposure_topic, 
                                        outcome_type) %>% 
  mutate(pecos = cur_group_id(),
         pecos_row = row_number(),
         pecos_total = n(),
         ma = ifelse(pecos_total>=2, 1, 0)) %>% arrange(pecos) %>% 
  ungroup()

ext_primary_list_bin <- ext_primary %>% 
  filter(outcome_type == "binary") %>% 
  group_split(pecos)

ext_primary_list_cont <- ext_primary %>% 
  filter(outcome_type == "continuous") %>% 
  group_split(pecos)

## set list names as study_id
#names(ext_primary_list) <- levels(ext_primary$pecos)

write_xlsx(ext_primary_list_bin, paste0("./data/working/pecos_binary.xlsx"))

write_xlsx(ext_primary_list_cont, paste0("./data/working/pecos_continuous.xlsx"))


summary(ext_primary$pecos_total)

##############
## check how many dp's have a valid std error
sum(ext_fin3$se_valid)

## check frequency of different outcome measures in primary analysis  
table(ext_primary$outcome_measure, ext_primary$outcome_type)

## check for missing estimates
ext_primary %>% filter(is.na(estimate))

#------------------------------------------------------------------------------#
#### Preparing binary outcomes for MA 
#------------------------------------------------------------------------------#

## Odds ratios are the preferred binary outcome measure for synthesis
## Hazard ratios to be treated as approximate to relative risk
## Relative risk to be converted based on reconfiguration of formula:
##    Risk=odds/(1+odds) (Grant 2014 - https://www.bmj.com/content/348/bmj.f7450.full)
## doesn't covert due to two odds terms - convert OR to RR instead?
##    

#### need to check sample size is correct for meta analysis
##    ^^^ see above ^^^


## create df for meta analyses of binary outcomes
ma_bin <- ext_primary %>% 
  filter(outcome_type=="binary" & 
           ma == 1 & 
           comparator_cat == "Persistent stable/low exposure") %>% 
#  filter(outcome_measure=="OR" | outcome_measure == "HR") %>% # filter only ORs and HRs
  # convert variables to numeric to allow calculations
  mutate(estimate = as.numeric(estimate),
         lowci = as.numeric(lowci),
         upci = as.numeric(upci),
         se = as.numeric(se)) %>% 
  # calculate log of estimate and CIs for conversion
  mutate(ln_est = log(estimate),
         ln_lowci = log(lowci),
         ln_upci = log(upci)) %>% 
  # convert p values into numeric values by dropping the < bit from strings
  mutate(p_value = gsub("[^0-9.-]", "", p_value)) %>% 
  mutate(p_value = as.numeric(p_value)) %>% 
  # calculate z scores for cases with only valid p value
  mutate(z_score = ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, qnorm(1-p_value/2), NA)) %>% 
  # calculate se based on log of CIs ===> need to check whether needs to then be exponentiated
  # next sort out se's where only have p value
  mutate(se2 = ifelse(se_valid ==0 & ci_valid == 1, (ln_upci-ln_lowci)/3.92, 
                     ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, estimate/z_score, se))) %>% 
  # create study var for display in forest plots
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group))


## produce meta analysis data ====> check se, seems OK but none were precalculated 

ma_bin_spine <- ma_bin %>%  select(pecos, outcome_measure, outcome_cat) %>% unique()
bin_spine_length <- nrow(ma_bin_spine)
ma_bin_pecos <- ma_bin_spine$pecos
ma_bin_labs <- paste(ma_bin_spine$pecos,ma_bin_spine$outcome_cat)

ma_bin_list <- vector(mode = "list", length = 0)

for(i in 1:bin_spine_length){
  group <- ma_bin_spine[i,1]$pecos
  out_meas <- ma_bin_spine[i,2]$outcome_measure
  out_cat <-  ma_bin_spine[i,3]$outcome_cat
  ma_bin_temp <- ma_bin %>% filter(pecos == group)
ma_test_run <- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study), data = ma_bin_temp,
                       comb.fixed = FALSE, comb.random = TRUE)
assign(paste0("ma_bin",group,"_",out_cat), ma_test_run)

ma_bin_list[[length(ma_bin_list) + 1]] <- ma_test_run
}


#------------------------------------------------------------------------------#
##### Binary forest plots
#------------------------------------------------------------------------------#

## loop through all binary MA objects to create forest plots 
## need to add code for assigning as an object and/or saving
for (i in seq_along(ma_bin_list)) {
  tiff(file = paste0("./charts/forest_plots/tiff/",ma_bin_labs[[i]],".tiff"), 
      width = 960, height = 480)
  forest_temp <- forest(x = ma_bin_list[[i]], leftcols = "studlab", addrow = TRUE)
  dev.off()
}

## loop through all binary MA objects to create forest plots 
## need to add code for assigning as an object and/or saving
for (i in seq_along(ma_bin_list)) {
  png(file = paste0("./charts/forest_plots/png/",ma_bin_labs[[i]],".png"), 
       width = 960, height = 480)
  forest_temp <- forest(x = ma_bin_list[[i]], leftcols = "studlab", addrow = TRUE)
  dev.off()
}


####  Binary - testing ---------------------------------------------------------

## test code for subgroup analysis by exposure topic -----
## NOTE - don't do this for draft plots in for loop, keep for final versions

## test df
ma_bin_test1 <- ma_bin %>% filter(outcome_cat == "Mental health symptoms")

## test MA
ma_bin_test2 <- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
        studlab = paste(study), data = ma_bin_test1,
        comb.fixed = FALSE, comb.random = TRUE)

## update MA with subgroups
update_ma_test <-update.meta(ma_bin_test2, 
                             byvar=exposure_topic, 
                             comb.random = TRUE, 
                             comb.fixed = FALSE)
update_ma_test

## forest with subgroups
forest(x = update_ma_test, leftcols = "studlab", overall = TRUE,
       subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)


## for subgroup mixed effects... (not currentlyt working)
#source("./functions/subgroup_analysis_mixed_effects.R")
#subgroup.analysis.mixed.effects(x = ma_bin_test2, subgroups = ma_bin$exposure_topic)

## ------

## HR df
hr_df <- ext_primary %>% filter(outcome_measure=="HR")

ma_bin %>% filter(id==2120)
ma_bin %>% filter(id==3965)
ma_bin %>% filter(id==3738)

#------------------------------------------------------------------------------#
#### Preparing continuous outcomes for MA 
#------------------------------------------------------------------------------#

## create df for meta analyses of continuous outcomes
ma_cont <- ext_primary %>% 
  filter(outcome_type=="continuous" & 
           ma == 1 & 
           comparator_cat == "Persistent stable/low exposure") %>% 
  # convert variables to numeric to allow calculations
  mutate(estimate = as.numeric(estimate),
         lowci = as.numeric(lowci),
         upci = as.numeric(upci),
         se = as.numeric(se)) %>% 
  # convert p values into numeric values by dropping the < bit from strings
  mutate(p_value = gsub("[^0-9.-]", "", p_value)) %>% 
  mutate(p_value = as.numeric(p_value)) %>% 
  # calculate z scores for cases with only valid p value
  mutate(z_score = ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, qnorm(1-p_value/2), NA)) %>% 
  # next sort out se's where only have p value
  mutate(se2 = ifelse(se_valid ==0 & ci_valid == 1, (upci-lowci)/3.92, 
                      ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, estimate/z_score, se))) %>% 
  # create study var for display in forest plots
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group))

ma_cont_spine <- ma_cont %>%  select(pecos, outcome_measure, outcome_cat) %>% unique()
cont_spine_length <- nrow(ma_cont_spine)
ma_cont_pecos <- ma_cont_spine$pecos
ma_cont_labs <- paste(ma_cont_spine$pecos,ma_cont_spine$outcome_cat)

ma_cont_list <- vector(mode = "list", length = 0)

for(i in 1:cont_spine_length){
  group <- ma_cont_spine[i,1]$pecos
  out_meas <- ma_cont_spine[i,2]$outcome_measure
  out_cat <-  ma_cont_spine[i,3]$outcome_cat
  ma_cont_temp <- ma_cont %>% filter(pecos == group)
  ma_test_run <- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                         studlab = paste(study), data = ma_cont_temp,
                         comb.fixed = FALSE, comb.random = TRUE)
  assign(paste0("ma_cont",group,"_",out_cat), ma_test_run)
  
  ma_cont_list[[length(ma_cont_list) + 1]] <- ma_test_run
}

#------------------------------------------------------------------------------#
##### Continuous forest plots
#------------------------------------------------------------------------------#

## loop through all continuous MA objects to create forest plots 
for (i in seq_along(ma_cont_list)) {
  tiff(file = paste0("./charts/forest_plots/tiff/continuous/",ma_cont_labs[[i]],".tiff"), 
       width = 960, height = 480)
  forest_temp <- forest(x = ma_cont_list[[i]], leftcols = "studlab", addrow = TRUE)
  dev.off()
}

## loop through all continuous MA objects to create forest plots 
## need to add code for assigning as an object and/or saving
for (i in seq_along(ma_cont_list)) {
  png(file = paste0("./charts/forest_plots/png/continuous/",ma_cont_labs[[i]],".png"), 
      width = 960, height = 480)
  forest_temp <- forest(x = ma_cont_list[[i]], leftcols = "studlab", addrow = TRUE)
  dev.off()
}

