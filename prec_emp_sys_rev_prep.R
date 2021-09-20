#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#####       Precarious Employment Systematic Review Data Preparation       #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


## remove any existing objects from global environment
rm(list=ls()) 

## disable scientific notation printing
options(scipen=999)

## install packages
library(tidyverse) # all kinds of stuff 
#library(dplyr) # for data manipulation
#library(forcats) # fpr categorical vars
#library(ggplot2) # for charts
#library(tibble) # for tibbles
#library(stringr) # for strings
library(readxl) # for reading excel file and all data sheets
library(writexl)
library(janitor) # for sorting out variable names etc
#library(fastR2) # for sign test CIs
# install latest version of meta to allow three-level forest plot production
#install.packages("devtools")
devtools::install_github("guido-s/meta")
library(meta) # for meta analysis
library(metafor) # for meta analysis
# cant's get dmetar to install
devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)  # for meta analysis




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
#mysheets <- read_excel_allsheets(filename = "./data/Prec_Emp_Data_Extract_20200825.xlsx") # previous version
mysheets <- read_excel_allsheets(filename = "./data/Prec_Emp_Data_Extract_20210131.xlsx")

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
  select(c(dp_id, study_id, id, first_author, year_published, exposure_topic, 
           exposure_type, outcome_cat, everything(),-mediators))

## arrange df for checking for dups
ext_fin2 <- ext_fin %>% 
  arrange(study_id, exposure_topic, exposure_type, outcome_cat, year_published, first_author) %>% 
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
#manual_dup_list <- mysheets <- read_excel_allsheets(filename = "./data/working/table-2_dedup_20201114.xlsx") # previous
manual_dup_list <- mysheets <- read_excel_allsheets(filename = "./data/working/table-2_dedup_20210201.xlsx")



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
  select(c(dp_id, dup_flag, comparator_cat, outcome_cat))


## create final extraction df
ext_fin3 <- ext_fin %>% 
  select(-outcome_cat) %>% 
  left_join(manual_dup) %>% 
  filter(dup_flag !=0) %>% 
  # remove Bender dp's for "other health conditions" - not specified in paper
  filter(outcome_cat != "“Other” Health Conditions")
##save
write.csv(ext_fin3, "./data/working/extracted_all.csv")

# convert id var to factor
ext_fin3$id <- factor(ext_fin3$id)

## create a vector of the final id numbers to be included in review
fin_id <- levels(ext_fin3$id)


## remove other duplicates (eg sex specific)
ext_primary <- ext_fin3 %>% filter(dup_flag==1)

##save primary extraction file
write.csv(ext_primary, "./data/working/extracted_primary.csv")


## create final study_desc and rob df's
study_desc_fin <- study_desc %>% filter(id %in% fin_id)
rob_fin <- rob %>% filter(id %in% fin_id)

## save these too
write.csv(study_desc_fin, "./data/working/studies_primary.csv")
write.csv(rob_fin, "./data/working/rob_primary.csv")

### add in additional papers here (Jin-Man and Virtanen) <----------------------
### check these for duplication <-----------------------------------------------


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

#### Study descriptives ----------------

## number of studies
length(unique(table_1$study_id)) # n = 44

## number of papers
length(unique(table_1$id)) # n = 49

## number of data points
nrow(ext_primary) # n = 236

## number of papers by study type
table(table_1$study_design)

## number of papers by study population
table(table_1$study_population) ## need revised categories if possible

## year of publication range
table(table_1$year_published)

## number of papers by country
table(table_1$countries_included_in_study)

## number of studies by country
country_df <- table_1 %>% select(study_id, countries_included_in_study)
country_df <- unique(country_df)
table(country_df$countries_included_in_study)

## number of papers or studies by RoB rating - do papers for now as that was 
## level RoB undertaken (1=low, 2=med, 3=high)
table(table_1$global_rating)

## number of papers by exposure topic
table(table_1$exposure_topic)

## number of papers by outcome topic
outcome_df <- ext_primary %>% select(id, outcome_topic_s)
outcome_df <- unique(outcome_df)
table(outcome_df$outcome_topic_s)

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

heat_gen <- fig_2_gen %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
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

heat_gen

png("./charts/heat_maps/png/heat_gen.png", 
    width = 900, height = 541, units = "px")
par(mar=c(5,3,2,2)+0.1) # removes margins
print(heat_gen)
dev.off()

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

heat_mh <- fig_2_mh %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
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

heat_mh

png("./charts/heat_maps/png/heat_mh.png", 
    width = 900, height = 541, units = "px")
par(mar=c(5,3,2,2)+0.1) # removes margins
print(heat_mh)
dev.off()

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

heat_phys <- fig_2_phys %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
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

heat_phys

png("./charts/heat_maps/png/heat_phys.png", 
    width = 900, height = 541, units = "px")
par(mar=c(5,3,2,2)+0.1) # removes margins
print(heat_phys)
dev.off()

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

heat_behav <- fig_2_behav %>% ggplot(aes(x = outcome_cat , y = exposure_topic, fill = data_points)) +
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

heat_behav

png("./charts/heat_maps/png/heat_behav.png", 
    width = 900, height = 541, units = "px")
par(mar=c(5,3,2,2)+0.1) # removes margins
print(heat_behav)
dev.off()

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
#------------------------------------------------------------------------------#
#####                          Narrative synthesis                         #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
##### Figure 4 - effect direction plots
#------------------------------------------------------------------------------#
######### ditch these
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
  mutate(ymax = n(),ypos = 1:ymax) 


table(eff_dir_df$direction, eff_dir_df$exposure_topic)

eff_dir_plot <- eff_dir_df %>% 
  ggplot(aes(x = exposure_topic, y = ypos, colour = direction)) + scale_colour_manual(values=c("darkgreen","gray58", "red4"), drop=F, name="Direction summary") + 
  geom_point(shape = 16, size = 6) + scale_y_continuous(limits=c(1,80), expand = c(0.05,0.05)) +
  coord_flip() + labs(x="Exposure category", y= "Number of data points")  + theme_classic()

eff_dir_plot

#------------------------------------------------------------------------------#
##### Figure 5 - harvest plots
#------------------------------------------------------------------------------#

# see Cochrane handbook ch12 and Ogilvie et al (2008) 
# ext_primary df adapted to include harvest_dir var 08/02/2021:
#     1 = negative outcome
#     -1 = positive outcome

# open harvest df
harvest_df <- read.csv("./data/working/extracted_primary_harvest.csv") %>% 
  #crate labels from harvest direction var
  mutate(harvest_lab = ifelse(harvest_dir == -1,
                              "Better",
                              "Worse")) %>% 
  left_join(rob_fin_global) %>% # add in risk of bias score
  # reverse scores to create height variable
  mutate(height = ifelse(global_rating == 1, 3,
                         ifelse(global_rating == 3, 1,
                                2))) %>% 
  arrange(desc(height), exposure_topic) %>% 
  group_by(outcome_topic_s, outcome_cat, harvest_dir) %>% 
  mutate(position = row_number()) %>% 
  ungroup()

#### general health ------------------
## number of dps by outcome category and effect direction
harvest_df %>%
  filter(outcome_topic_s=="General health") %>% 
  group_by(outcome_cat, harvest_lab) %>% 
  summarise(n = n()) %>% 
  print() %>% 
  ungroup()

# plot
harvest_gen <- harvest_df %>%
  filter(outcome_topic_s=="General health") %>% 
  ggplot(aes(x=position, y = height, fill = exposure_topic)) +
  geom_col(width = 0.5) + 
  #  geom_text(aes(y = 1, label = study_id), angle = 90, hjust = 1) +
  xlim(0,56) +
  facet_grid(outcome_cat ~ harvest_lab, switch = "y") +
  scale_fill_discrete(name  ="Exposure topic:") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
harvest_gen

png("./charts/harvest_plots/png/harvest_gen.png", 
    width = 960, height = 400)
par(mar=c(5,3,2,2)+0.1) # removes margins
print(harvest_gen)
dev.off()

#### mental health ---------------
## number of dps by outcome category and effect direction
harvest_df %>%
  filter(outcome_topic_s=="Mental health") %>% 
  group_by(outcome_cat, harvest_lab) %>% 
  summarise(n = n()) %>% 
  print() %>% 
  ungroup()

# plot
harvest_mh <- harvest_df %>%
  filter(!is.na(harvest_dir)) %>%  # temp line
  filter(outcome_topic_s=="Mental health") %>% 
  ggplot(aes(x=position, y = height, fill = exposure_topic)) +
  geom_col(width = 0.5) + 
  #  geom_text(aes(y = 1, label = study_id), angle = 90, hjust = 1) +
  xlim(0,56) +
  facet_grid(outcome_cat ~ harvest_lab, switch = "y") +
  scale_fill_discrete(name  ="Exposure topic:") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
harvest_mh

png("./charts/harvest_plots/png/harvest_mh.png", 
    width = 960, height = 200)
par(mar=c(5,3,2,2)+0.1) # removes margins
print(harvest_mh)
dev.off()


#### physical health ------------------
## might need to split - lots of rows

## number of dps by outcome category and effect direction
harvest_df %>%
  filter(outcome_topic_s=="Physical health") %>% 
  group_by(outcome_cat, harvest_lab) %>% 
  summarise(n = n()) %>% 
  print() %>% 
  ungroup()

harvest_phys <- harvest_df %>%
  filter(outcome_topic_s=="Physical health") %>% 
  ggplot(aes(x=position, y = height, fill = exposure_topic)) +
  geom_col(width = 0.5) + 
  #  geom_text(aes(y = 1, label = study_id), angle = 90, hjust = 1) +
  xlim(0,56) +
  facet_grid(outcome_cat ~ harvest_lab, switch = "y") +
  scale_fill_discrete(name  ="Exposure topic:") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
harvest_phys

## save
png("./charts/harvest_plots/png/harvest_phys.png", 
    width = 960, height = 960)
par(mar=c(5,3,2,2)+0.1) # removes margins
print(harvest_phys)
dev.off()


### if splitting

# plot 1
harvest_phys1 <- harvest_df %>%
  filter(outcome_topic_s=="Physical health",
         outcome_cat %in% c("Cancer", "Cardiovascular","Chronic condition", 
                            "Digestive", "Respiratory", "Skin/allergy")) %>% 
  ggplot(aes(x=position, y = height, fill = exposure_topic)) +
  geom_col(width = 0.5) + 
  #  geom_text(aes(y = 1, label = study_id), angle = 90, hjust = 1) +
  xlim(0,56) +
  facet_grid(outcome_cat ~ harvest_lab, switch = "y") +
  scale_fill_discrete(name  ="Exposure topic:") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
harvest_phys1

## save
png("./charts/harvest_plots/png/harvest_phys1.png", 
    width = 960, height = 480)
par(mar=c(5,3,2,2)+0.1) # removes margins
print(harvest_phys1)
dev.off()

# plot 2
harvest_phys2 <- harvest_df %>%
  filter(outcome_topic_s=="Physical health",
         outcome_cat %in% c("Blood pressure", "Dental", "External causes mortality", 
                            "Health problems", "Injury", "Migraine")) %>% 
  ggplot(aes(x=position, y = height, fill = exposure_topic)) +
  geom_col(width = 0.5) + 
  #  geom_text(aes(y = 1, label = study_id), angle = 90, hjust = 1) +
  xlim(0,56) +
  facet_grid(outcome_cat ~ harvest_lab, switch = "y") +
  scale_fill_discrete(name  ="Exposure topic:") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
harvest_phys2

## save
png("./charts/harvest_plots/png/harvest_phys2.png", 
    width = 960, height = 480)
par(mar=c(5,3,2,2)+0.1) # removes margins
print(harvest_phys2)
dev.off()

#### health behaviours --------------------------
## number of dps by outcome category and effect direction
harvest_df %>%
  filter(outcome_topic_s=="Health behaviours") %>% 
  group_by(outcome_cat, harvest_lab) %>% 
  summarise(n = n()) %>% 
  print() %>% 
  ungroup()

# plot
harvest_behav <- harvest_df %>%
  filter(outcome_topic_s=="Health behaviours") %>% 
  ggplot(aes(x=position, y = height, fill = exposure_topic)) +
  geom_col(width = 0.5) + 
  #  geom_text(aes(y = 1, label = study_id), angle = 90, hjust = 1) +
  xlim(0,56) +
  facet_grid(outcome_cat ~ harvest_lab, switch = "y") +
  scale_fill_discrete(name  ="Exposure topic:") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
harvest_behav
## save
png("./charts/harvest_plots/png/harvest_behav.png", 
    width = 960, height = 480)
par(mar=c(5,3,2,2)+0.1) # removes margins
print(harvest_behav)
dev.off()

#------------------------------------------------------------------------------#
##### sign tests for narrative synthesis
#------------------------------------------------------------------------------#

sign_df <- harvest_df %>% 
  # recode harvest_dir variable to 1/0 binary
  mutate(sign_dir = ifelse(harvest_dir == -1, 0, 1)) %>% 
  group_by(outcome_topic_s, outcome_cat) %>% 
  # create vars for sign test calculation:
  # u = number of negative outcomes  within outcome category
  # n = total number of dp's within outcome category
  summarise(u = sum(sign_dir),
            n = n()) %>% 
  # calculate proportion of dp's with negative outcome estimate
  mutate(prop_neg = u/n) %>% 
  ungroup()

sign_df$biom_p <- 0
sign_df$lowci <- 0
sign_df$upci <- 0

for(i in 1:25){
  u <- sign_df[[i,3]]
  n <- sign_df[[i,4]]
  binom_temp <- binom.test(u,n, ci.method = "Wilson")
  binom_temp <- data.frame(unlist(binom_temp))
  #  label <- sign_df[[i,1]]
  #  assign(paste0("binom_",label), binom_temp)
  # assign p value
  sign_df[i,6] <- as.numeric(binom_temp[3,])
  # assign confidence intervals
  sign_df[i,7] <- as.numeric(binom_temp[4,])
  sign_df[i,8] <- as.numeric(binom_temp[5,])
  
  
}



write.csv(sign_df, "./data/working/sign_test.csv")

## descriptives 

# number of dp's by outcome category and RoB global rating
table(harvest_df$global_rating, harvest_df$outcome_cat)

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

ext_primary  <- read.csv("./data/working/extracted_primary.csv")

#------------------------------------------------------------------------------#
#### Grouping PECOS combinations for synthesis
#------------------------------------------------------------------------------#

## this pipeline groups dp's by PECOS configurations
## does not currently include population, outcome measure (eg OR, HR etc), or study type
## calculates the PECOS group number, row number within PECOS group 
## PECOS groups with 2 or more dp's can be considered for meta analysis (ma variable)
ext_primary <- ext_primary %>% group_by(exposure_type,outcome_cat, outcome_measure,  
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
  mutate(z_score = ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, qnorm(1-p_value/2), NA))  %>% 
  # recode z score for Bender 2018 anxiety/depression estimate 
  #  mutate(z_score = ifelse(first_author=="Bender, K", 3.05, z_score)) %>% 
  # calculate se based on log of CIs ===> need to check whether needs to then be exponentiated
  # next sort out se's where only have p value
  mutate(se2 = ifelse(se_valid ==0 & ci_valid == 1, (ln_upci-ln_lowci)/3.92, 
                      ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, estimate/z_score, se))) %>% 
  # create study var for display in forest plots
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group)) %>% 
  # remove Cross for time being - don't think estimates are comparable
  filter(first_author != "Cross, J") %>% 
  # add additional info for Dobson (heavy/light smoker)
  mutate(study = ifelse(first_author=="Dobson, K", paste0(study,"; ",definition_of_outcome), study))

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
##### Binary forest plots - draft versions
#------------------------------------------------------------------------------#

## loop through all binary MA objects to create forest plots 
## need to add code for assigning as an object and/or saving
for (i in seq_along(ma_bin_list)) {
  tiff(file = paste0("./charts/forest_plots/tiff/binary/",ma_bin_labs[[i]],".tiff"), 
       width = 960, height = 480)
  forest_temp <- forest(x = ma_bin_list[[i]], leftcols = "studlab", addrow = TRUE)
  dev.off()
}

## loop through all binary MA objects to create forest plots 
## need to add code for assigning as an object and/or saving
for (i in seq_along(ma_bin_list)) {
  png(file = paste0("./charts/forest_plots/png/binary/",ma_bin_labs[[i]],".png"), 
      width = 960, height = 480)
  forest_temp <- forest(x = ma_bin_list[[i]], leftcols = "studlab", addrow = TRUE)
  dev.off()
}

#------------------------------------------------------------------------------#
##### Binary forest plots - publication versions
#------------------------------------------------------------------------------#

## next, create bespoke forest plots for binary pecos...
## include in paper if MA is based on more than one study
## supplementary if only one study
## group by exposure cat within MA


#### Meta analyses and forest plots for main paper -----------------------------

### Function for MA/forest plots to be included in paper ----
forest_paper1 <- function(exposure_lab, outcome_lab, out_meas,
                          w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- ma_bin %>% filter(exposure_type == exposure_lab &
                                  outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp <<- update.meta(ma_temp, byvar=exposure_topic, comb.random = TRUE, 
                          comb.fixed = FALSE)
  
  #produce ans save forest plot
  png(file = paste0("./charts/forest_plots/paper/binary_outcomes/",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  png(file = paste0("./charts/funnel_plots/binary_outcomes/",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  funnel(ma_temp)
  dev.off()
} # end of function ----

### Binary outcomes
## Alcohol consumption
forest_paper1(exposure_lab = "binary", outcome_lab = "Alcohol consumption",
              out_meas = "OR")

## All-cause mortality
forest_paper1(exposure_lab = "binary", outcome_lab = "All-cause mortality",
              out_meas = "OR")

## Chronic condition (excluded if Cross excluded from MA analysis)
#forest_paper1(exposure_lab = "binary", outcome_lab = "Chronic condition",
#             out_meas = "OR")

## Mental health symptoms
forest_paper1(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
              out_meas = "OR",h = 800)

## Self-assessed health
forest_paper1(exposure_lab = "binary", outcome_lab = "Self-assessed health",
              out_meas = "OR")

## Tobacco consumption
forest_paper1(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
              out_meas = "OR")





#### Meta analyses and forest plots for supplementary material -----------------

forest_supp <- function(exposure_lab, outcome_lab, w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- ma_bin %>% filter(exposure_type == exposure_lab &
                                  outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp <<- update.meta(ma_temp, byvar=exposure_topic, comb.random = TRUE, 
                          comb.fixed = FALSE)
  
  #produce and save forest plot
  png(file = paste0("./charts/forest_plots/paper/",outcome_lab,"_",exposure_lab,".png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
} # end of function ----



metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
        studlab = paste(study), 
        data = df_temp,
        comb.fixed = FALSE, comb.random = FALSE)


####  Binary - testing ---------------------------------------------------------

## checking HRs ------

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
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group)) %>% 
  # keep only dp's with valid info for MA
  filter(!is.na(se2)) %>% 
  # remove Cross for time being - don't think estimates are comparable
  filter(first_author != "Cross, J")  %>% 
  # remove MH symptoms DPs that don't use CES-D
  filter(outcome_cat!="Mental health symptoms" |
           (outcome_cat=="Mental health symptoms" & study=="Burgard, S (2017); Both; persistently insecure at T1 and T2") |
           (outcome_cat=="Mental health symptoms" & study=="Glavin, P (2015); Both; Persistent insecurity")) %>% 
  # separate out diastolic blood pressure
  mutate(outcome_cat = ifelse(grepl("diastolic", definition_of_outcome), "Diastolic blood pressure", outcome_cat)) %>% 
  # separate out cholesterol
  mutate(outcome_cat = ifelse(grepl("cholesterol", definition_of_outcome), "Cholesterol", outcome_cat))



ma_cont_spine <- ma_cont %>%  select(pecos, outcome_measure, outcome_cat) %>% unique()
cont_spine_length <- nrow(ma_cont_spine)
ma_cont_pecos <- ma_cont_spine$pecos
ma_cont_labs <- paste(ma_cont_spine$pecos,ma_cont_spine$outcome_cat)

ma_cont_list <- vector(mode = "list", length = 0)



#------------------------------------------------------------------------------#
##### Draft continuous MAs and forest plots
#------------------------------------------------------------------------------#

## loop through PECOS's 
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

#### Meta analyses and forest plots for main paper -----------------------------

### Function for MA/forest plots to be included in paper ----
forest_paper2 <- function(exposure_lab, outcome_lab, out_meas,
                          w = 960, h = 480, type){
  if(exists("df_temp2")) rm("df_temp2", envir = globalenv())
  if(exists("ma_temp2")) rm("ma_temp2", envir = globalenv())
  df_temp2 <<- ma_cont %>% filter(exposure_type == exposure_lab &
                                    outcome_cat==outcome_lab)
  ma_temp2 <<- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study), 
                       data = df_temp2,
                       comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp2 <<- update.meta(ma_temp2, byvar=exposure_topic, comb.random = TRUE, 
                           comb.fixed = FALSE)
  
  #produce and save forest plot
  png(file = paste0("./charts/forest_plots/paper/continuous_outcomes/",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp2, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  png(file = paste0("./charts/funnel_plots/continuous_outcomes/",outcome_lab,"_",exposure_lab,"_funnel.png"),
      width = w, height = h)
  funnel(ma_temp2)
  dev.off()
} # end of function ----


### Continuous outcomes
## Blood pressure - diastolic
forest_paper2(exposure_lab = "binary", outcome_lab = "Diastolic blood pressure",
              out_meas = "Adjusted mean difference")

## Cardiovascular
forest_paper2(exposure_lab = "binary", outcome_lab = "Cholesterol",
              out_meas = "Adjusted mean difference")


## Healthy weight
forest_paper2(exposure_lab = "binary", outcome_lab = "BMI",
              out_meas = "Adjusted mean difference")

## Mental health symptoms ====> probs to check
forest_paper2(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
              out_meas = "Regression coefficient")

## Self-assessed health ====> check Cross (2009)
forest_paper2(exposure_lab = "binary", outcome_lab = "Self-assessed health",
              out_meas = "Regression coefficient", w = 1000, h = 600)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#####                          Sub-group analysis                          #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Males 
#------------------------------------------------------------------------------#

males <- ext_fin3 %>% filter(sex=="Male") %>% 
  group_by(exposure_type,outcome_cat, outcome_measure,  
           outcome_type) %>% 
  mutate(pecos = cur_group_id(),
         pecos_row = row_number(),
         pecos_total = n(),
         ma = ifelse(pecos_total>=2, 1, 0)) %>% arrange(pecos) %>% 
  ungroup()


#### binary outcomes ---------

males_bin <- males %>% 
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
  mutate(z_score = ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, qnorm(1-p_value/2), NA))  %>% 
  # recode z score for Bender 2018 anxiety/depression estimate 
  #  mutate(z_score = ifelse(first_author=="Bender, K", 3.05, z_score)) %>% 
  # calculate se based on log of CIs ===> need to check whether needs to then be exponentiated
  # next sort out se's where only have p value
  mutate(se2 = ifelse(se_valid ==0 & ci_valid == 1, (ln_upci-ln_lowci)/3.92, 
                      ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, estimate/z_score, se))) %>% 
  # create study var for display in forest plots
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group)) %>% 
  # remove Cross for time being - don't think estimates are comparable
  filter(first_author != "Cross, J") %>% 
  # add additional info for Dobson (heavy/light smoker)
  mutate(study = ifelse(first_author=="Dobson, K", paste0(study,"; ",definition_of_outcome), study))

### Function for MA/forest plots to be included in sub-group analysis ----
forest_paper_sub1 <- function(exposure_lab, outcome_lab, out_meas,
                              w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- males_bin %>% filter(exposure_type == exposure_lab &
                                     outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp <<- update.meta(ma_temp, byvar=exposure_topic, comb.random = TRUE, 
                          comb.fixed = FALSE)
  
  #produce ans save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/binary_outcomes/males_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  png(file = paste0("./charts/funnel_plots/supplementary/binary_outcomes/males_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  funnel(ma_temp)
  dev.off()
  
  
} # end of function ----

### Binary outcomes
## Alcohol consumption
forest_paper_sub1(exposure_lab = "binary", outcome_lab = "Alcohol consumption",
                  out_meas = "OR")

## All-cause mortality
forest_paper_sub1(exposure_lab = "binary", outcome_lab = "All-cause mortality",
                  out_meas = "OR")

## Chronic condition (excluded if Cross excluded from MA analysis)
#forest_paper_sub1(exposure_lab = "binary", outcome_lab = "Chronic condition",
#             out_meas = "OR")

## Mental health symptoms
forest_paper_sub1(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
                  out_meas = "OR",h = 800)

## Self-assessed health
forest_paper_sub1(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                  out_meas = "OR")

## Tobacco consumption
forest_paper_sub1(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                  out_meas = "OR")


#### continuous outcomes ---------

## create df for meta analyses of continuous outcomes
males_cont <- males %>% 
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
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group)) %>% 
  # keep only dp's with valid info for MA
  filter(!is.na(se2)) %>% 
  # remove Cross for time being - don't think estimates are comparable
  filter(first_author != "Cross, J")  %>% 
  # remove MH symptoms DPs that don't use CES-D
  filter(outcome_cat!="Mental health symptoms" |
           (outcome_cat=="Mental health symptoms" & study=="Burgard, S (2017); Both; persistently insecure at T1 and T2") |
           (outcome_cat=="Mental health symptoms" & study=="Glavin, P (2015); Both; Persistent insecurity")) %>% 
  # separate out diastolic blood pressure
  mutate(outcome_cat = ifelse(grepl("diastolic", definition_of_outcome), "Diastolic blood pressure", outcome_cat)) %>% 
  # separate out cholesterol
  mutate(outcome_cat = ifelse(grepl("cholesterol", definition_of_outcome), "Cholesterol", outcome_cat))

### Function for MA/forest plots to be included in paper ----
forest_paper_sub2 <- function(exposure_lab, outcome_lab, out_meas,
                              w = 960, h = 480, type){
  if(exists("df_temp2")) rm("df_temp2", envir = globalenv())
  if(exists("ma_temp2")) rm("ma_temp2", envir = globalenv())
  df_temp2 <<- ma_cont %>% filter(exposure_type == exposure_lab &
                                    outcome_cat==outcome_lab)
  ma_temp2 <<- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study), 
                       data = df_temp2,
                       comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp2 <<- update.meta(ma_temp2, byvar=exposure_topic, comb.random = TRUE, 
                           comb.fixed = FALSE)
  
  #produce and save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/continuous_outcomes/males_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp2, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  #  png(file = paste0("./charts/funnel_plots/continuous_outcomes/",outcome_lab,"_",exposure_lab,"_funnel.png"),
  #      width = w, height = h)
  #  funnel(ma_temp2)
  #  dev.off()
} # end of function ----


### Continuous outcomes
## Blood pressure - diastolic
forest_paper_sub2(exposure_lab = "binary", outcome_lab = "Diastolic blood pressure",
                  out_meas = "Adjusted mean difference")

## Cardiovascular
forest_paper_sub2(exposure_lab = "binary", outcome_lab = "Cholesterol",
                  out_meas = "Adjusted mean difference")


## Healthy weight
forest_paper_sub2(exposure_lab = "binary", outcome_lab = "BMI",
                  out_meas = "Adjusted mean difference")

## Mental health symptoms ====> probs to check
forest_paper_sub2(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
                  out_meas = "Regression coefficient")

## Self-assessed health ====> check Cross (2009)
forest_paper_sub2(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                  out_meas = "Regression coefficient", w = 1000, h = 600)

#------------------------------------------------------------------------------#
#### Females 
#------------------------------------------------------------------------------#

females <- ext_fin3 %>% filter(sex=="Female") %>% 
  group_by(exposure_type,outcome_cat, outcome_measure,  
           outcome_type) %>% 
  mutate(pecos = cur_group_id(),
         pecos_row = row_number(),
         pecos_total = n(),
         ma = ifelse(pecos_total>=2, 1, 0)) %>% arrange(pecos) %>% 
  ungroup()


females_bin <- females %>% 
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
  mutate(z_score = ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, qnorm(1-p_value/2), NA))  %>% 
  # recode z score for Bender 2018 anxiety/depression estimate 
  #  mutate(z_score = ifelse(first_author=="Bender, K", 3.05, z_score)) %>% 
  # calculate se based on log of CIs ===> need to check whether needs to then be exponentiated
  # next sort out se's where only have p value
  mutate(se2 = ifelse(se_valid ==0 & ci_valid == 1, (ln_upci-ln_lowci)/3.92, 
                      ifelse(se_valid==0 & ci_valid == 0 & p_valid == 1, estimate/z_score, se))) %>% 
  # create study var for display in forest plots
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group)) %>% 
  # remove Cross for time being - don't think estimates are comparable
  filter(first_author != "Cross, J") %>% 
  # add additional info for Dobson (heavy/light smoker)
  mutate(study = ifelse(first_author=="Dobson, K", paste0(study,"; ",definition_of_outcome), study))

### Function for MA/forest plots to be included in sub-group analysis ----
forest_paper_sub3 <- function(exposure_lab, outcome_lab, out_meas,
                              w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- females_bin %>% filter(exposure_type == exposure_lab &
                                       outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp <<- update.meta(ma_temp, byvar=exposure_topic, comb.random = TRUE, 
                          comb.fixed = FALSE)
  
  #produce ans save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/binary_outcomes/females_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  png(file = paste0("./charts/funnel_plots/supplementary/binary_outcomes/females_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  funnel(ma_temp)
  dev.off()
  
  
} # end of function ----

### Binary outcomes
## Alcohol consumption
forest_paper_sub3(exposure_lab = "binary", outcome_lab = "Alcohol consumption",
                  out_meas = "OR")

## All-cause mortality
forest_paper_sub3(exposure_lab = "binary", outcome_lab = "All-cause mortality",
                  out_meas = "OR")

## Chronic condition (excluded if Cross excluded from MA analysis)
#forest_paper_sub1(exposure_lab = "binary", outcome_lab = "Chronic condition",
#             out_meas = "OR")

## Mental health symptoms
forest_paper_sub3(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
                  out_meas = "OR",h = 800)

## Self-assessed health
forest_paper_sub3(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                  out_meas = "OR")

## Tobacco consumption
forest_paper_sub3(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                  out_meas = "OR")

#### continuous outcomes ---------

## create df for meta analyses of continuous outcomes
females_cont <- females %>% 
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
  mutate(study = paste0(first_author," (",year_published,"); ",sex,"; ",exposure_group)) %>% 
  # keep only dp's with valid info for MA
  filter(!is.na(se2)) %>% 
  # remove Cross for time being - don't think estimates are comparable
  filter(first_author != "Cross, J")  %>% 
  # remove MH symptoms DPs that don't use CES-D
  filter(outcome_cat!="Mental health symptoms" |
           (outcome_cat=="Mental health symptoms" & study=="Burgard, S (2017); Both; persistently insecure at T1 and T2") |
           (outcome_cat=="Mental health symptoms" & study=="Glavin, P (2015); Both; Persistent insecurity")) %>% 
  # separate out diastolic blood pressure
  mutate(outcome_cat = ifelse(grepl("diastolic", definition_of_outcome), "Diastolic blood pressure", outcome_cat)) %>% 
  # separate out cholesterol
  mutate(outcome_cat = ifelse(grepl("cholesterol", definition_of_outcome), "Cholesterol", outcome_cat))

### Function for MA/forest plots to be included in paper ----
forest_paper_sub4 <- function(exposure_lab, outcome_lab, out_meas,
                              w = 960, h = 480, type){
  if(exists("df_temp2")) rm("df_temp2", envir = globalenv())
  if(exists("ma_temp2")) rm("ma_temp2", envir = globalenv())
  df_temp2 <<- ma_cont %>% filter(exposure_type == exposure_lab &
                                    outcome_cat==outcome_lab)
  ma_temp2 <<- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study), 
                       data = df_temp2,
                       comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp2 <<- update.meta(ma_temp2, byvar=exposure_topic, comb.random = TRUE, 
                           comb.fixed = FALSE)
  
  #produce and save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/continuous_outcomes/females_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp2, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  #  png(file = paste0("./charts/funnel_plots/continuous_outcomes/",outcome_lab,"_",exposure_lab,"_funnel.png"),
  #      width = w, height = h)
  #  funnel(ma_temp2)
  #  dev.off()
} # end of function ----


### Continuous outcomes
## Blood pressure - diastolic
forest_paper_sub4(exposure_lab = "binary", outcome_lab = "Diastolic blood pressure",
                  out_meas = "Adjusted mean difference")

## Cardiovascular
forest_paper_sub4(exposure_lab = "binary", outcome_lab = "Cholesterol",
                  out_meas = "Adjusted mean difference")


## Healthy weight
forest_paper_sub4(exposure_lab = "binary", outcome_lab = "BMI",
                  out_meas = "Adjusted mean difference")

## Mental health symptoms ====> probs to check
forest_paper_sub4(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
                  out_meas = "Regression coefficient")

## Self-assessed health ====> check Cross (2009)
forest_paper_sub4(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                  out_meas = "Regression coefficient", w = 1000, h = 600)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#####                         Sensitivity analysis                         #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Exclude high risk of bias studies 
#------------------------------------------------------------------------------#

# change id to factor to allow join
#rob_fin_global$id <- factor(rob_fin_global$id)

#### binary outcomes -----------------------------
sa_ma_bin <- ma_bin %>% 
  left_join(rob_fin_global) %>% # add in risk of bias score
  filter(global_rating != 3)

### Function for MA/forest plots to be included in paper ----
forest_sa1 <- function(exposure_lab, outcome_lab, out_meas,
                       w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- sa_ma_bin %>% filter(exposure_type == exposure_lab &
                                     outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp <<- update.meta(ma_temp, byvar=exposure_topic, comb.random = TRUE, 
                          comb.fixed = FALSE)
  
  #produce ans save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/sensitivity_analysis/binary_outcomes/sa_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  #  png(file = paste0("./charts/funnel_plots/binary_outcomes/",outcome_lab,"_",exposure_lab,"_exp.png"),
  #      width = w, height = h)
  #  funnel(ma_temp)
  #  dev.off()
} # end of function ----

### Binary outcomes
## Alcohol consumption
forest_sa1(exposure_lab = "binary", outcome_lab = "Alcohol consumption",
           out_meas = "OR")

## All-cause mortality
forest_sa1(exposure_lab = "binary", outcome_lab = "All-cause mortality",
           out_meas = "OR")

## Chronic condition (excluded if Cross excluded from MA analysis)
#forest_sa1(exposure_lab = "binary", outcome_lab = "Chronic condition",
#             out_meas = "OR")

## Mental health symptoms
forest_sa1(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
           out_meas = "OR",h = 800)

## Self-assessed health
forest_sa1(exposure_lab = "binary", outcome_lab = "Self-assessed health",
           out_meas = "OR")

## Tobacco consumption
forest_sa1(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
           out_meas = "OR")


#### continuous outcomes ----------------------------- 
sa_ma_cont <- ma_cont %>% 
  left_join(rob_fin_global) %>% # add in risk of bias score
  filter(global_rating != 3)

### Function for MA/forest plots to be included in paper ----
forest_sa2 <- function(exposure_lab, outcome_lab, out_meas,
                       w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- sa_ma_cont %>% filter(exposure_type == exposure_lab &
                                      outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      comb.fixed = FALSE, comb.random = TRUE)
  
  ma_temp <<- update.meta(ma_temp, byvar=exposure_topic, comb.random = TRUE, 
                          comb.fixed = FALSE)
  
  #produce ans save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/sensitivity_analysis/continuous_outcomes/sa_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  #  png(file = paste0("./charts/funnel_plots/continuous_outcomes/sa_",outcome_lab,"_",exposure_lab,"_exp.png"),
  #      width = w, height = h)
  #  funnel(ma_temp)
  #  dev.off()
} # end of function ----

### Continuous outcomes
## Blood pressure - diastolic
forest_sa2(exposure_lab = "binary", outcome_lab = "Diastolic blood pressure",
           out_meas = "Adjusted mean difference")

## Cardiovascular
forest_sa2(exposure_lab = "binary", outcome_lab = "Cholesterol",
           out_meas = "Adjusted mean difference")


## Healthy weight
forest_sa2(exposure_lab = "binary", outcome_lab = "BMI",
           out_meas = "Adjusted mean difference")

## Mental health symptoms ====> probs to check
forest_sa2(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
           out_meas = "Regression coefficient")

## Self-assessed health ====> check Cross (2009)
forest_sa2(exposure_lab = "binary", outcome_lab = "Self-assessed health",
           out_meas = "Regression coefficient", w = 1000, h = 600)

#------------------------------------------------------------------------------#
#### Three-level models 
#------------------------------------------------------------------------------#

## Data points taken from separate studies cannot be treated as entirely 
## independent (e.g. sex stratified estimates, or different exposure groups 
## using the same reference group).

ma_bin <- ma_bin %>% 
  group_by(study_id) %>% 
  mutate(id_n = n()) %>% 
  ungroup() %>% 
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()

#### Binary outcomes -----------------------------------------------------------
### Function for MA/forest plots to be included in paper ----
three_level_bin <- function(exposure_lab, outcome_lab, out_meas,
                            w = 960, h = 480, type){
  #if(exists("df_temp")) rm("df_temp", envir = globalenv())
  #if(exists("ma_temp")) rm("ma_temp", envir = globalenv())
  df_temp <<- ma_bin %>% filter(exposure_type == exposure_lab &
                                  outcome_cat==outcome_lab)
  ma_temp <<- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                      studlab = paste(study), 
                      data = df_temp,
                      byvar= exposure_topic, 
                      id = comp_id,
                      comb.fixed = FALSE, comb.random = TRUE)

  #produce and save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/sensitivity_analysis/three-level/binary_outcomes/3L_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  #png(file = paste0("./charts/funnel_plots/binary_outcomes/",outcome_lab,"_",exposure_lab,"_exp.png"),
  #    width = w, height = h)
  #funnel(ma_temp)
  #dev.off()
} # end of function ----

### Binary outcomes
## Alcohol consumption
three_level_bin(exposure_lab = "binary", outcome_lab = "Alcohol consumption",
                out_meas = "OR")

## All-cause mortality
three_level_bin(exposure_lab = "binary", outcome_lab = "All-cause mortality",
                out_meas = "OR")

## Chronic condition (excluded if Cross excluded from MA analysis)
#forest_paper1(exposure_lab = "binary", outcome_lab = "Chronic condition",
#             out_meas = "OR")

## Mental health symptoms
three_level_bin(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
                out_meas = "OR",h = 800)

## Self-assessed health
three_level_bin(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                out_meas = "OR")

## Tobacco consumption
three_level_bin(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                out_meas = "OR")

#### Continuous outcomes -------------------------------------------------------
### Function for MA/forest plots to be included in paper ----
three_level_cont <- function(exposure_lab, outcome_lab, out_meas,
                             w = 960, h = 480, type){
  if(exists("df_temp2")) rm("df_temp2", envir = globalenv())
  if(exists("ma_temp2")) rm("ma_temp2", envir = globalenv())
  df_temp2 <<- ma_cont %>% filter(exposure_type == exposure_lab &
                                    outcome_cat==outcome_lab)
  ma_temp2 <<- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study), 
                       data = df_temp2,
                       comb.fixed = FALSE, comb.random = TRUE,
                       id = study_id)
  
  #produce and save forest plot
  png(file = paste0("./charts/forest_plots/supplementary/sensitivity_analysis/three-level/continuous_outcomes/3L_",outcome_lab,"_",exposure_lab,"_exp.png"),
      width = w, height = h)
  forest(x = ma_temp2, leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)
  dev.off()
  
  # produce and save funnel plot
  #png(file = paste0("./charts/funnel_plots/continuous_outcomes/",outcome_lab,"_",exposure_lab,"_funnel.png"),
  #    width = w, height = h)
  #funnel(ma_temp2)
  #dev.off()
} ## end of function ----#


### Continuous outcomes
## Blood pressure - diastolic
three_level_cont(exposure_lab = "binary", outcome_lab = "Diastolic blood pressure",
                 out_meas = "Adjusted mean difference")

## Cardiovascular
three_level_cont(exposure_lab = "binary", outcome_lab = "Cholesterol",
                 out_meas = "Adjusted mean difference")


## Healthy weight
three_level_cont(exposure_lab = "binary", outcome_lab = "BMI",
                 out_meas = "Adjusted mean difference")

## Mental health symptoms ====> three-level model not rquired
#three_level_cont(exposure_lab = "binary", outcome_lab = "Mental health symptoms",
#              out_meas = "Regression coefficient")

## Self-assessed health ====> check Cross (2009)
three_level_cont(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                 out_meas = "Regression coefficient", w = 1000, h = 600)



##################### test area ################################################

df_tempx <- ma_bin %>% filter(exposure_type == "binary" &
                                outcome_cat=="Mental health symptoms" &
                                outcome_measure == "OR") %>% 
  group_by(study_id, sex) %>% 
  mutate(indep_grp = cur_group_id()) %>% 
  ungroup() %>% 
  # create study var for display in forest plots
  mutate(study = paste0(first_author," (",year_published,"); ",sex))

#id_vector <- unique(df_tempx$study_id)

ma_tempx <- metagen(TE = ln_est, seTE = se2, sm = paste("OR"), 
                    data = df_tempx,
                    studlab = paste(study), 
                    #byvar=exposure_topic,
                    comb.fixed = FALSE, 
                    comb.random = TRUE,
                    method.tau = "REML",
                    hakn = FALSE,
                    prediction = FALSE,
                    id = study)


ma_tempx <- update.meta(ma_tempx, byvar=exposure_topic, comb.random = TRUE, 
                        comb.fixed = FALSE,
                        tau.common =  TRUE,
                        id = study)

#forest_temp <- 
forest(x = ma_tempx, leftcols = "studlab", addrow = TRUE)

library(metaforest)

df_tempx <- df_tempx %>% mutate(v = se2*se2)

m_multi <- rma.mv(ln_est, v, random = list(~ 1 | dp_id, ~ 1 | study), data = df_tempx)
m_multi


#### 3-l test 20210919


df_temp3l <- ma_bin %>% 
  filter(exposure_type == "binary" &
                                  outcome_cat=="Mental health symptoms") %>% 
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()



ma_temp3l <- metagen(TE = ln_est, seTE = se2, sm = "OR", 
                      studlab = paste(study), 
                      data = df_temp3l,
                      byvar= exposure_topic, 
                      id = comp_id,
                      comb.fixed = FALSE, comb.random = TRUE)

  #produce ans save forest plot
forest(x = ma_temp3l, leftcols = "studlab", overall = TRUE,
       subgroup = TRUE, print.subgroup.labels = TRUE, study.results = TRUE)

### Binary outcomes
## Alcohol consumption
three_level_bin(exposure_lab = "binary", outcome_lab = "Alcohol consumption",
                out_meas = "OR")

