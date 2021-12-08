#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#####                Precarious Employment Systematic Review               #####
#####                Forest and Funnel plots for publication               #####
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


## remove any existing objects from global environment
rm(list=ls()) 

## disable scientific notation printing
options(scipen=999)

## install packages
library(tidyverse) # all kinds of stuff 
#library(readxl) # for reading excel file and all data sheets
#library(writexl)
#library(janitor) # for sorting out variable names etc
# install latest version of meta to allow three-level forest plot production
#install.packages("devtools")
devtools::install_github("guido-s/meta")
library(meta) # for meta analysis
library(metafor) # for meta analysis
# cant's get dmetar to install
devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)  # for meta analysis

#### load data -------
# binary outcomes
ma_bin <- read.csv("./data/working/ma_bin.csv") %>% 
  # add comparator ID var
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()

# continuous outcomes
ma_cont <- read.csv("./data/working/ma_cont.csv") %>% 
  # add comparator ID var
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup() %>% 
## add further detail for Burgard studies
  mutate(study = ifelse(study_id=="SR004", paste0(study," American’s Changing Lives"),
                        ifelse(study_id=="SR005",paste0(study," Midlife in the United States"),
                               study)))

## check number of studies in meta-analyses
ma_bin %>% select(study_id) %>% bind_rows(ma_cont) %>% 
  select(study_id) %>% unique() %>% arrange(study_id)


#------------------------------------------------------------------------------#
##### Functions  
#------------------------------------------------------------------------------#

#### Binary outcomes -----------------------------------------------------------

### Main function for three-level MA with binary outcome ----
three_level_bin <- function(exposure_lab, outcome_lab, out_meas){
  df_temp <- df_temp %>% filter(exposure_type == exposure_lab &
                                  outcome_cat==outcome_lab)
  ma_temp <- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                     studlab = paste(study), 
                     data = df_temp,
                     subgroup= exposure_topic, 
                     subgroup.name = "Exposure topic",
                     id = comp_id,
                     fixed = FALSE, random = TRUE)
} # end of function ----

### Un-grouped function for three-level MA ----
ungrouped_bin <- function(exposure_lab, outcome_lab, out_meas,
                            w = 960, h = h, type){
  df_temp <- df_temp %>% filter(exposure_type == exposure_lab &
                                 outcome_cat==outcome_lab)
  ma_temp <- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                     studlab = paste(study), 
                     data = df_temp,
                     id = comp_id,
                     fixed = FALSE, random = TRUE)
} # end of function ----

### MA for individual exposure topic with binary outcome ----
indiv_expos_bin <- function(exposure_tp, exposure_lab, outcome_lab, out_meas){
  df_temp <- df_temp %>% filter(exposure_topic==exposure_tp &
                                  exposure_type == exposure_lab &
                                  outcome_cat==outcome_lab)
  ma_temp <- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                     studlab = paste(study), 
                     data = df_temp,
#                     id = comp_id,
                     fixed = FALSE, random = TRUE)
} # end of function ----


#### Continuous outcomes -------------------------------------------------------
### Main function for three-level MA with continuous outcome ----
### Function for MA/forest plots to be included in paper ----
three_level_cont <- function(exposure_lab, outcome_lab, out_meas,
                             w = 960, h = h, type){
  df_temp2 <- df_temp2 %>% filter(exposure_type == exposure_lab &
                                    outcome_cat==outcome_lab)
  ma_temp2 <- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study),
                       subgroup = exposure_topic,
                       subgroup.name = "Exposure topic",
                       data = df_temp2,
                       id = comp_id,
                       fixed = FALSE, random = TRUE)
 
} ## end of function ----


#### produce and save forest plot  ---------------------------------------------
#### png files ---------------
forest1 <- function(datafile, 
                    datafile_lab,
                    w = 960, h = 960, type, 
                    lab_left = "Favours exposed", 
                    lab_right = "Favours unexposed",
                    textline = "", x_lab = ""){
  png(file = paste0("./charts/publication_versions/forest_plots/",datafile_lab,".png"),
      width = w, height = h)
  forest(x = datafile,
         leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, 
         study.results = TRUE, 
         label.left = lab_left,
         label.right = lab_right, 
         text.addline1 = textline,
         fs.addline = 18,
         xlab = x_lab)
  dev.off()  
  
} # end of function ---------

#### pdf files ---------------
forest2 <- function(datafile, 
                    datafile_lab,
                    pap = "a4r",
                    w = 11.69, h = 8.27, 
                    f_size = 8,
                    type, 
                    lab_left = "Favours exposed", 
                    lab_right = "Favours unexposed",
                    textline = "", x_lab = ""){

  pdf(file = paste0("./charts/publication_versions/forest_plots/",datafile_lab,".pdf"),
      paper = pap,
      width=w, height=h)
  forest(x = datafile,
         leftcols = "studlab", overall = TRUE,
         subgroup = TRUE, print.subgroup.labels = TRUE, 
         study.results = TRUE, 
         label.left = lab_left,
         label.right = lab_right, 
         fontsize=f_size, 
         text.addline1 = textline,
         fs.addline = 12,
         xlab = x_lab)
  dev.off()  
  
} # end of function ---------

#### produce and save funnel plots ---------------------------------------------
funnels1 <- function(ma_name,funnel_lab){

  ## png
  png(file = paste0("./charts/publication_versions/funnel_plots/",funnel_lab,".png"),
    width = 400, height = 400)
  funnel(ma_name)
  dev.off()

  ##pdf
  pdf(file = paste0("./charts/publication_versions/funnel_plots/",funnel_lab,".pdf"))
  funnel(ma_name)
  dev.off()
} # end of function ----

#### harvest plot function -----------------------------------------------------
harvest1 <- function(outcome_topic, harvest_lab, h = 960, w){
  harvest_temp <- harvest_df %>%
    filter(outcome_topic_s==outcome_topic) %>% 
    # reword SAH to SRH for plot
    mutate(outcome_cat = ifelse(outcome_cat=="Self-assessed health",
                                "Self-rated health", outcome_cat )) %>% 
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
  
  # save as png 
  png(file = paste0("./charts/publication_versions/harvest_plots/",harvest_lab,".png"),
      width = w, height = h)
  par(mar=c(5,3,2,2)+0.1) # removes margins
  print(harvest_temp)
  dev.off()
  
  ## save as pdf
  pdf(file = paste0("./charts/publication_versions/harvest_plots/",harvest_lab,".pdf"))
  par(mar=c(5,3,2,2)+0.1) # removes margins
  print(harvest_temp)  
  dev.off()  
  
} # end of function -----

#------------------------------------------------------------------------------#
##### Figure 1: General health forest plots - publication versions  
#------------------------------------------------------------------------------#

df_temp <- ma_bin
df_temp2 <- ma_cont

#### Figure 1(a) - Self-rated health binary outcome ----------------------------

## meta-analysis
srh_bin <- ungrouped_bin(exposure_lab = "binary", 
                         outcome_lab = "Self-assessed health",
                         out_meas = "OR")

## forest plot
forest1(datafile = srh_bin, 
                          datafile_lab = "1a_srh_bin",
                          h = 480, w = 960,
                          textline = "
                      
                           
(a) Poor self-rated health as a binary outcome")

forest2(datafile = srh_bin, 
        datafile_lab = "1a_srh_bin",
        textline = "
                      
                           
(a) Poor self-rated health as a binary outcome")


## funnel plot
funnels1(ma_name = srh_bin,
         funnel_lab = "S8.1_srh_bin")


#### Figure 1(b) - Self-rated health continuous outcome ------------------------

## meta-analysis
srh_cont <- three_level_cont(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                 out_meas = "Regression coefficient") 

## forest plot
forest1(datafile = srh_cont, datafile_lab = "1b_srh_cont", 
        textline = "
                           
                           
(b) Self-rated health as a continuous scale",
        x_lab = "Difference in five-point self-rated health scale",
        w = 1100, h = 600,
        lab_left = "Favours unexposed", 
        lab_right = "Favours exposed")


forest2(datafile = srh_cont, datafile_lab = "1b_srh_cont", 
        textline = "
                           
                           
(b) Self-rated health as a continuous scale",
        x_lab = "Difference in five-point self-rated health scale",
        lab_left = "Favours unexposed", 
        lab_right = "Favours exposed")

## funnel plot
funnels1(ma_name = srh_cont,
         funnel_lab = "S8.2_srh_cont")

#### Figure 1(c) - all-cause mortality -----------------------------------------

## meta-analysis
all_mort <- three_level_bin(exposure_lab = "binary", 
                            outcome_lab = "All-cause mortality",
                            out_meas = "OR")

## forest plot
forest1(datafile = all_mort, datafile_lab = "1c_all_mort",
        textline = "
                           

(c) All-cause mortality",
        h = 460)


forest2(datafile = all_mort, datafile_lab = "1c_all_mort",
        textline = "
                           

(c) All-cause mortality")


## funnel plot
funnels1(ma_name = all_mort,
         funnel_lab = "S8.3_all_mort")


#------------------------------------------------------------------------------#
##### Figure 2: Mental health forest plots - publication versions  
#------------------------------------------------------------------------------#

#### (a) Poor mental health as a binary outcome --------------------------------

## meta-analysis
mh_bin <- three_level_bin(exposure_lab = "binary", 
                            outcome_lab = "Mental health symptoms",
                            out_meas = "OR")

## forest plot
forest1(datafile = mh_bin, datafile_lab = "2a_mh_bin",
        textline = "
                           

(a) Poor mental health as a binary outcome",
        h = 850)

forest2(datafile = mh_bin, datafile_lab = "2a_mh_bin",
        textline = "
                           

(a) Poor mental health as a binary outcome",
        pap = "a4",
        h = 11.69, w = 8.27, f_size = 6)

## funnel plot
funnels1(ma_name = mh_bin,
         funnel_lab = "S8.4_mh_bin")

#### (b) Symptoms of poor mental health as a continuous outcome measure on the 
####     CES-D scale --------------------------------

## meta-analysis
mh_cont <- three_level_cont(exposure_lab = "binary", 
                            outcome_lab = "Mental health symptoms",
                out_meas = "Regression coefficient")


## forest plot
forest1(datafile = mh_cont, datafile_lab = "2b_mh_cont",
        textline = "
                           

(b) Symptoms of poor mental health as a continuous outcome measure on the CES-D scale", 
                x_lab = "Difference in CES-D scale",
        h = 300)

forest2(datafile = mh_cont, datafile_lab = "2b_mh_cont",
        textline = "
                           

(b) Symptoms of poor mental health as a continuous outcome measure on the CES-D scale", 
        x_lab = "Difference in CES-D scale")


#------------------------------------------------------------------------------#
##### Figure 3: Physical health forest plots - publication versions  
#------------------------------------------------------------------------------#

#### (a) Cholesterol level -----------------------------------------------------

## meta-analysis
cholesterol <- three_level_cont(exposure_lab = "binary", 
                                outcome_lab = "Cholesterol",
                                out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = cholesterol, datafile_lab = "3a_cholesterol",
        textline = "
                           

(a) Cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)",
        h = 400)

forest2(datafile = cholesterol, datafile_lab = "3a_cholesterol",
        textline = "
                           

(a) Cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)")


#### (b) Diastolic blood pressure ----------------------------------------------

## meta-analysis

diastolic <- three_level_cont(exposure_lab = "binary", 
                              outcome_lab = "Diastolic blood pressure",
                              out_meas = "Adjusted mean difference")

## forest plot

forest1(datafile = diastolic, datafile_lab = "3b_diastolic",
        textline = "
                           

(b) Diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)",
        h = 400)

forest2(datafile = diastolic, datafile_lab = "3b_diastolic",
        textline = "
                           

(b) Diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)")


#------------------------------------------------------------------------------#
##### Figure 4: Physical health forest plots - publication versions  
#------------------------------------------------------------------------------#

#### (a) Harmful alcohol consumption -------------------------------------------

## meta-analysis
alcohol <- three_level_bin(exposure_lab = "binary", 
                           outcome_lab = "Alcohol consumption",
                           out_meas = "OR")

## forest plot
forest1(datafile = alcohol, datafile_lab = "4a_alcohol",
        textline = "


(a) Harmful alcohol consumption",
        h=400)

forest2(datafile = alcohol, datafile_lab = "4a_alcohol",
        textline = "


(a) Harmful alcohol consumption")


#### (b) Body mass index -------------------------------------------------------

## meta-analysis
bmi <- three_level_cont(exposure_lab = "binary", outcome_lab = "BMI",
                        out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = bmi, datafile_lab = "4b_bmi",
        textline = "
                           

(b) Body mass index",
x_lab = "Adjusted mean difference in body mass index",
h = 400)

forest2(datafile = bmi, datafile_lab = "4b_bmi",
        textline = "
                           

(b) Body mass index",
        x_lab = "Adjusted mean difference in body mass index")


#### (c) Current smoking status ------------------------------------------------

## meta-analysis
smoking <- three_level_bin(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                           out_meas = "OR")

## forest plot
forest1(datafile = smoking, datafile_lab = "4c_smoking",
        textline = "


(c) Current smoking status",
        h=420)

forest2(datafile = smoking, datafile_lab = "4c_smoking",
        textline = "


(c) Current smoking status")



#------------------------------------------------------------------------------#
#####  Self-rated health exposure stratified - publication versions  
#------------------------------------------------------------------------------#

## this is for outcomes that could not be run as three-level MAs grouped by 
## exposure type due to at least one exposure consisting of one study with 
## multiple data points all using the same reference group

#### Figure S5.1: poor self-rated health by persistent perceived job security --
srh_perceived <- indiv_expos_bin(exposure_tp = "Perceived job security",
                exposure_lab = "binary", 
                outcome_lab = "Self-assessed health",
                out_meas = "OR")

## forest plot
forest1(datafile = srh_perceived, 
        datafile_lab = "S5.1_srh_perceived_bin",
        h = 180, w = 960)

forest2(datafile = srh_perceived, 
        datafile_lab = "S5.1_srh_perceived_bin")


#### Figure S5.2: poor self-rated health by persistent employment contract -----
srh_contract <- indiv_expos_bin(exposure_tp = "Employment contract",
                                 exposure_lab = "binary", 
                                 outcome_lab = "Self-assessed health",
                                 out_meas = "OR")

## forest plot
forest1(datafile = srh_contract, 
        datafile_lab = "S5.2_srh_contract_bin",
        h = 220, w = 960)

forest2(datafile = srh_contract, 
        datafile_lab = "S5.2_srh_contract_bin")


#### Figure S5.3: poor self-rated health by persistent multi-dimensional measure
#                 of precarious employment -------------------------------------
srh_multiple <- indiv_expos_bin(exposure_tp = "Multiple",
                                exposure_lab = "binary", 
                                outcome_lab = "Self-assessed health",
                                out_meas = "OR")

## forest plot
forest1(datafile = srh_multiple, 
        datafile_lab = "S5.3_srh_multiple_bin",
        h = 220, w = 960)

forest2(datafile = srh_multiple, 
        datafile_lab = "S5.3_srh_multiple_bin")

rm(df_temp, df_temp2)


#------------------------------------------------------------------------------#
##### Sub-group analyses - publication versions  
#------------------------------------------------------------------------------#

### load data
# males binary outcomes
males_bin <- read.csv("./data/working/males_bin.csv") %>% 
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()

# males binary outcomes
males_cont <- read.csv("./data/working/males_cont.csv") %>% 
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()

# males binary outcomes
females_bin <- read.csv("./data/working/females_bin.csv") %>% 
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()

# males binary outcomes
females_cont <- read.csv("./data/working/females_cont.csv") %>% 
  group_by(sex, comparator_group) %>% 
  mutate(comp_id = paste0(sex,", ",comparator_group)) %>% 
  ungroup()


#### Figure S6.1: general health -----------------------------------------------

### (a) Poor self-rated health as a binary outcome (male)
df_temp <- males_bin
## meta-analysis
srh_bin_m <- ungrouped_bin(exposure_lab = "binary", 
                         outcome_lab = "Self-assessed health",
                         out_meas = "OR")

## forest plot
forest1(datafile = srh_bin_m, 
        datafile_lab = "S6.1a_srh_bin",
        h = 300, w = 960,
        textline = "
                      
                           
(a) Male poor self-rated health as a binary outcome")

forest2(datafile = srh_bin_m, 
        datafile_lab = "S6.1a_srh_bin",
        textline = "
                      
                           
(a) Male poor self-rated health as a binary outcome")

rm(df_temp)

### (b) Poor self-rated health as a binary outcome (female) 
df_temp <- females_bin

## meta-analysis
srh_bin_f <- ungrouped_bin(exposure_lab = "binary", 
                           outcome_lab = "Self-assessed health",
                           out_meas = "OR")

## forest plot
forest1(datafile = srh_bin_f, 
        datafile_lab = "S6.1b_srh_bin",
        h = 300, w = 960,
        textline = "
                      
                           
(b) Female poor self-rated health as a binary outcome")

forest2(datafile = srh_bin_f, 
        datafile_lab = "S6.1b_srh_bin",
        textline = "
                      
                           
(b) Female poor self-rated health as a binary outcome")

rm(df_temp)


### (c) All-cause mortality (male)
df_temp <- males_bin

## meta-analysis
all_mort_m <- three_level_bin(exposure_lab = "binary", 
                            outcome_lab = "All-cause mortality",
                            out_meas = "OR")

## forest plot
forest1(datafile = all_mort_m, datafile_lab = "S6.1c_all_mort",
        textline = "
                           

(c) Male all-cause mortality",
        h = 300)

forest2(datafile = all_mort_m, datafile_lab = "S6.1c_all_mort",
        textline = "
                           

(c) Male all-cause mortality")


rm(df_temp)

### (d) All-cause mortality (female)
df_temp <- females_bin

## meta-analysis
all_mort_m <- three_level_bin(exposure_lab = "binary", 
                              outcome_lab = "All-cause mortality",
                              out_meas = "OR")

## forest plot
forest1(datafile = all_mort_m, datafile_lab = "S6.1d_all_mort",
        textline = "
                           

(d) Female all-cause mortality",
        h = 300)

forest2(datafile = all_mort_m, datafile_lab = "S6.1d_all_mort",
        textline = "
                           

(d) Female all-cause mortality")

rm(df_temp)


#### Figure S6.2: mental health ------------------------------------------------

### (a) Poor mental health as a binary outcome (male)
df_temp <- males_bin

## meta-analysis
mh_bin_m <- three_level_bin(exposure_lab = "binary", 
                          outcome_lab = "Mental health symptoms",
                          out_meas = "OR")

## forest plot
forest1(datafile = mh_bin_m, datafile_lab = "S6.2a_mh_bin",
        textline = "
                           

(a) Male poor mental health as a binary outcome",
        h = 560)

forest2(datafile = mh_bin_m, datafile_lab = "S6.2a_mh_bin",
        textline = "
                           

(a) Male poor mental health as a binary outcome")

rm(df_temp)


### (b) Poor mental health as a binary outcome (female)
df_temp <- females_bin

## meta-analysis
mh_bin_f <- three_level_bin(exposure_lab = "binary", 
                            outcome_lab = "Mental health symptoms",
                            out_meas = "OR")

## forest plot
forest1(datafile = mh_bin_f, datafile_lab = "S6.2b_mh_bin",
        textline = "
                           

(b) Female poor mental health as a binary outcome",
        h = 600)

forest2(datafile = mh_bin_f, datafile_lab = "S6.2b_mh_bin",
        textline = "
                           

(b) Female poor mental health as a binary outcome")

## forest plot

rm(df_temp)

#### Figure S6.3: physical health ----------------------------------------------
### (a) Cholesterol level (male)
df_temp2 <- males_cont

## meta-analysis
cholesterol_m <- three_level_cont(exposure_lab = "binary", 
                                outcome_lab = "Cholesterol",
                                out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = cholesterol_m, datafile_lab = "S6.3a_cholesterol",
        textline = "
                           

(a) Male cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)",
        h = 340)

forest2(datafile = cholesterol_m, datafile_lab = "S6.3a_cholesterol",
        textline = "
                           

(a) Male cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)")

rm(df_temp2)

### (b) Cholesterol level (female)

df_temp2 <- females_cont

## meta-analysis
cholesterol_f <- three_level_cont(exposure_lab = "binary", 
                                  outcome_lab = "Cholesterol",
                                  out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = cholesterol_f, datafile_lab = "S6.3b_cholesterol",
        textline = "
                           

(b) Female cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)",
        h = 340)

forest2(datafile = cholesterol_f, datafile_lab = "S6.3b_cholesterol",
        textline = "
                           

(b) Female cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)")

rm(df_temp2)

### (c) Diastolic blood pressure (male)
df_temp2 <- males_cont

## meta-analysis
diastolic_m <- three_level_cont(exposure_lab = "binary", 
                              outcome_lab = "Diastolic blood pressure",
                              out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = diastolic_m, datafile_lab = "S6.3c_diastolic",
        textline = "
                           

(c) Male diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)",
        h = 340)

forest2(datafile = diastolic_m, datafile_lab = "S6.3c_diastolic",
        textline = "
                           

(c) Male diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)")


rm(df_temp)

### (d) Diastolic blood pressure (female)
df_temp2 <- females_cont

## meta-analysis
diastolic_f <- three_level_cont(exposure_lab = "binary", 
                                outcome_lab = "Diastolic blood pressure",
                                out_meas = "Adjusted mean difference")

## forest plot

forest1(datafile = diastolic_f, datafile_lab = "S6.3d_diastolic",
        textline = "
                           

(d) Male diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)",
        h = 340)

forest2(datafile = diastolic_f, datafile_lab = "S6.3d_diastolic",
        textline = "
                           

(d) Male diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)")

rm(df_temp)

#### Figure S6.4: health behaviours --------------------------------------------

### (a) Harmful alcohol consumption (male)
df_temp <- males_bin

## meta-analysis
alcohol_m <- three_level_bin(exposure_lab = "binary", 
                           outcome_lab = "Alcohol consumption",
                           out_meas = "OR")

## forest plot
forest1(datafile = alcohol_m, datafile_lab = "S6.4a_alcohol",
        textline = "


(a) Male harmful alcohol consumption",
        h=330)

forest2(datafile = alcohol_m, datafile_lab = "S6.4a_alcohol",
        textline = "


(a) Male harmful alcohol consumption")

rm(df_temp)

### (b) Harmful alcohol consumption (female)
df_temp <- females_bin

## meta-analysis
alcohol_f <- three_level_bin(exposure_lab = "binary", 
                             outcome_lab = "Alcohol consumption",
                             out_meas = "OR")

## forest plot
forest1(datafile = alcohol_f, datafile_lab = "S6.4b_alcohol",
        textline = "


(b) Female harmful alcohol consumption",
        h=335)

forest2(datafile = alcohol_f, datafile_lab = "S6.4b_alcohol",
        textline = "


(b) Female harmful alcohol consumption")

rm(df_temp2)

### (c) Body mass index (male)
df_temp2 <- males_cont

## meta-analysis
bmi_m <- three_level_cont(exposure_lab = "binary", outcome_lab = "BMI",
                        out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = bmi_m, datafile_lab = "S6.4c_bmi",
        textline = "
                           

(c) Male body mass index",
        x_lab = "Adjusted mean difference in body mass index",
        h = 335)

forest2(datafile = bmi_m, datafile_lab = "S6.4c_bmi",
        textline = "
                           

(c) Male body mass index",
        x_lab = "Adjusted mean difference in body mass index")

rm(df_temp2)

### (d) Body mass index (female)
df_temp2 <- females_cont

## meta-analysis
bmi_f <- three_level_cont(exposure_lab = "binary", outcome_lab = "BMI",
                          out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = bmi_f, datafile_lab = "S6.4d_bmi",
        textline = "
                           

(d) Female body mass index",
        x_lab = "Adjusted mean difference in body mass index",
        h = 335)

forest2(datafile = bmi_f, datafile_lab = "S6.4d_bmi",
        textline = "
                           

(d) Female body mass index",
        x_lab = "Adjusted mean difference in body mass index")

rm(df_temp)

### (e) Current smoking status (male)
df_temp <- males_bin

## meta-analysis
smoking_m <- three_level_bin(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                           out_meas = "OR")


## forest plot
forest1(datafile = smoking_m, datafile_lab = "S6.4e_smoking",
        textline = "


(e) Male current smoking status",
        h=335)

forest2(datafile = smoking_m, datafile_lab = "S6.4e_smoking",
        textline = "


(e) Male current smoking status")

rm(df_temp)

### (f) Current smoking status (female)
df_temp <- females_bin

## meta-analysis
smoking_f <- three_level_bin(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                             out_meas = "OR")


## forest plot
forest1(datafile = smoking_f, datafile_lab = "S6.4f_smoking",
        textline = "


(f) Female current smoking status",
        h=335)

forest2(datafile = smoking_f, datafile_lab = "S6.4f_smoking",
        textline = "


(f) Female current smoking status")

rm(df_temp)



#------------------------------------------------------------------------------#
##### Sensitivity analyses - publication versions  
#------------------------------------------------------------------------------#
## excludes high risk of bias studies

## load data

# binary outcomes
df_temp <-  read.csv("./data/working/sa_ma_bin.csv")
# continuous outcomes
df_temp2 <- read.csv("./data/working/sa_ma_cont.csv")

#### Figure S7.1(a) - Self-rated health binary outcome -------------------------

## meta-analysis
srh_bin_sa <- ungrouped_bin(exposure_lab = "binary", 
                         outcome_lab = "Self-assessed health",
                         out_meas = "OR")

## forest plot
forest1(datafile = srh_bin_sa, 
        datafile_lab = "S7.1a_srh_bin",
        h = 300, w = 960,
        textline = "
                      
                           
(a) Poor self-rated health as a binary outcome")

forest2(datafile = srh_bin_sa, 
        datafile_lab = "S7.1a_srh_bin",
        textline = "
                      
                           
(a) Poor self-rated health as a binary outcome")

#### Figure 1(b) - Self-rated health continuous outcome ------------------------

## meta-analysis
srh_cont_sa <- three_level_cont(exposure_lab = "binary", outcome_lab = "Self-assessed health",
                             out_meas = "Regression coefficient") 

## forest plot
forest1(datafile = srh_cont_sa, datafile_lab = "S7.1b_srh_cont", 
        textline = "
                           
                           
(b) Self-rated health as a continuous scale",
        x_lab = "Difference in five-point self-rated health scale",
        w = 1100, h = 360,
        lab_left = "Favours unexposed", 
        lab_right = "Favours exposed")

forest2(datafile = srh_cont_sa, datafile_lab = "S7.1b_srh_cont", 
        textline = "
                           
                           
(b) Self-rated health as a continuous scale",
        x_lab = "Difference in five-point self-rated health scale",
        lab_left = "Favours unexposed", 
        lab_right = "Favours exposed")



#### Figure 2(a) Poor mental health as a binary outcome ------------------------

## meta-analysis
mh_bin_sa <- three_level_bin(exposure_lab = "binary", 
                          outcome_lab = "Mental health symptoms",
                          out_meas = "OR")

## forest plot
forest1(datafile = mh_bin_sa, datafile_lab = "S7.2a_mh_bin",
        textline = "
                           

(a) Poor mental health as a binary outcome",
        h = 560)


forest2(datafile = mh_bin_sa, datafile_lab = "S7.2a_mh_bin",
        textline = "
                           

(a) Poor mental health as a binary outcome")


#### Figure 3(a) Cholesterol level ---------------------------------------------

## meta-analysis
cholesterol_sa <- three_level_cont(exposure_lab = "binary", 
                                outcome_lab = "Cholesterol",
                                out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = cholesterol_sa, datafile_lab = "S7.3a_cholesterol",
        textline = "
                           

(a) Cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)",
        h = 300)

forest2(datafile = cholesterol_sa, datafile_lab = "S7.3a_cholesterol",
        textline = "
                           

(a) Cholesterol level",
        x_lab = "Adjusted mean difference in cholesterol (mmol)")

#### Figure 3(b) Diastolic blood pressure ----------------------------------------------

## meta-analysis

diastolic_sa <- three_level_cont(exposure_lab = "binary", 
                              outcome_lab = "Diastolic blood pressure",
                              out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = diastolic_sa, datafile_lab = "S7.3b_diastolic",
        textline = "
                           

(b) Diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)",
        h = 300)

forest2(datafile = diastolic_sa, datafile_lab = "S7.3b_diastolic",
        textline = "
                           

(b) Diastolic blood pressure",
        x_lab = "Adjusted mean difference in diastolic blood pressure (mmHg)")


#### Figure 4(a) Harmful alcohol consumption -----------------------------------

## meta-analysis
alcohol_sa <- three_level_bin(exposure_lab = "binary", 
                           outcome_lab = "Alcohol consumption",
                           out_meas = "OR")

## forest plot
forest1(datafile = alcohol_sa, datafile_lab = "S7.4a_alcohol",
        textline = "


(a) Harmful alcohol consumption",
        h=300)

forest2(datafile = alcohol_sa, datafile_lab = "S7.4a_alcohol",
        textline = "


(a) Harmful alcohol consumption")

#### Figure 4(b) Body mass index -----------------------------------------------

## meta-analysis
bmi_sa <- three_level_cont(exposure_lab = "binary", outcome_lab = "BMI",
                        out_meas = "Adjusted mean difference")

## forest plot
forest1(datafile = bmi_sa, datafile_lab = "S7.4b_bmi",
        textline = "
                           

(b) Body mass index",
        x_lab = "Adjusted mean difference in body mass index",
        h = 300)

forest2(datafile = bmi_sa, datafile_lab = "S7.4b_bmi",
        textline = "
                           

(b) Body mass index",
        x_lab = "Adjusted mean difference in body mass index")

#### Figure 4(c) Current smoking status ----------------------------------------

## meta-analysis
smoking_sa <- three_level_bin(exposure_lab = "binary", 
                              outcome_lab = "Tobacco consumption",
                              out_meas = "OR")


## forest plot
forest1(datafile = smoking_sa, datafile_lab = "S7.4c_smoking",
        textline = "


(c) Current smoking status",
        h=300)

forest2(datafile = smoking_sa, datafile_lab = "S7.4c_smoking",
        textline = "


(c) Current smoking status")

rm(df_temp, df_temp2)


#------------------------------------------------------------------------------#
##### Harvest plots
#------------------------------------------------------------------------------#

## remove any existing objects from global environment except functions
rm(list = setdiff(ls(), lsf.str()))

# see Cochrane handbook ch12 and Ogilvie et al (2008) 
# ext_primary df adapted to include harvest_dir var 08/02/2021:
#     1 = negative outcome
#     -1 = positive outcome

# open harvest df
harvest_df <- read.csv("./data/working/prepared_primary_harvest.csv") 

#### general health ------------------------------------------------------------
harvest1(outcome_topic = "General health", harvest_lab = "S4.1_gen_health",
         h = 400) 

#### mental health -------------------------------------------------------------
harvest1(outcome_topic = "Mental health", harvest_lab = "S4.2_mental_health", 
         h = 200) 

#### physical health -----------------------------------------------------------
harvest1(outcome_topic = "Physical health", harvest_lab = "S4.3_physical_health",
         h = 960) 


#### health behaviours ---------------------------------------------------------
harvest1(outcome_topic = "Health behaviours", harvest_lab = "S4.4_health_behav",
         h = 480, w = 960) 


