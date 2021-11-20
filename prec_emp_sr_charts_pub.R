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
  df_temp <- ma_bin %>% filter(exposure_type == exposure_lab &
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
  df_temp <- ma_bin %>% filter(exposure_type == exposure_lab &
                                 outcome_cat==outcome_lab)
  ma_temp <- metagen(TE = ln_est, seTE = se2, sm = paste(out_meas), 
                     studlab = paste(study), 
                     data = df_temp,
                     id = comp_id,
                     fixed = FALSE, random = TRUE)
} # end of function ----

#### Continuous outcomes -------------------------------------------------------
### Main function for three-level MA with continuous outcome ----
### Function for MA/forest plots to be included in paper ----
three_level_cont <- function(exposure_lab, outcome_lab, out_meas,
                             w = 960, h = h, type){
  df_temp2 <- ma_cont %>% filter(exposure_type == exposure_lab &
                                    outcome_cat==outcome_lab)
  ma_temp2 <- metagen(TE = estimate, seTE = se2, sm = paste(out_meas), 
                       studlab = paste(study),
                       subgroup = exposure_topic,
                       subgroup.name = "Exposure topic",
                       data = df_temp2,
                       id = comp_id,
                       fixed = FALSE, random = TRUE)
 
} ## end of function ----





#produce and save forest plot
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


#  # produce and save funnel plot
#  #funnel(ma_temp)
#} # end of function ----






#------------------------------------------------------------------------------#
##### General health forest plots - publication versions  
#------------------------------------------------------------------------------#

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

## funnel plot



#### Figure 1(b) - Self-rated health continuous outcome ------------------------

## meta-analyis
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

## funnel plot

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


## funnel plot


#------------------------------------------------------------------------------#
##### Mental health forest plots - publication versions  
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
        h = 800)


## funnel plot

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


#------------------------------------------------------------------------------#
##### Physical health forest plots - publication versions  
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

## funnel plot

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

## funnel plot

#------------------------------------------------------------------------------#
##### Physical health forest plots - publication versions  
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

## funnel plot

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

## funnel plot

#### (c) Current smoking status ------------------------------------------------

## meta-analysis
smoking <- three_level_bin(exposure_lab = "binary", outcome_lab = "Tobacco consumption",
                           out_meas = "OR")


## forest plot
forest1(datafile = smoking, datafile_lab = "4c_smoking",
        textline = "


(c) Current smoking status",
        h=400)

## funnel plot

  # produce and save funnel plot
#  png(file = paste0("./charts/funnel_plots/binary_outcomes/",outcome_lab,"_",exposure_lab,"_exp.png"),
#      width = 400, height = 400)
#  funnel(ma_temp)
#  dev.off()
#
 