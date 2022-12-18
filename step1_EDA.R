####load pacakges####
#install.packages("here")
library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(skimr)
rm(list = ls())
#####set path to data#####
path_dat <- "/Users/zpeng/OneDrive/Desktop/ABCD/Analysis data/ADHD_informant_race/"##change this for your environment
#path_dat <- "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/ABCD/"
here()
####Load separate data and select relevant variables####
####race-ethnicity load from ABCD-invariance project####
load(file.path(here(),"cbcldemo_step1.RData"))
##select only white, black, hispanic groups##
allHispanic_id <- c(otherhispanic_subID, Hispanic_black_subID,Hispanic_white_subID)##these IDs were generated in step1_loadexplore.R
##create a Race variable, 0=hispanic, 1=white, 2=black
abcd_cbcldemo <- abcd_cbcldemo %>%
  mutate(Race_three = ifelse(abcd_cbcldemo$subjectkey %in%blackOnly_subID,2,
                             ifelse(abcd_cbcldemo$subjectkey %in%whiteOnly_subID,1,
                                    ifelse(abcd_cbcldemo$subjectkey %in% allHispanic_id,0,3))))
##select cases where race equals to 0,1,2
abcd_cbcldemo <- abcd_cbcldemo %>%
  filter(Race_three %in% c(0,1,2))
table(abcd_cbcldemo$Race_three)

##remove all variables except for race-ethnicity
abcd_race <- abcd_cbcldemo %>%
  select(Race_three, subjectkey,hispanic)
rm(list = c("abcd_cbcldemo","abcd_anthro","abcd_medServ"))###these are not relevant for this study but loaded together with cbcldemo_step1.RData. Delete

#####Demographic#####
abcd_pdemo <- fread(paste0(path_dat, "pdem02.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(-c("eventname","dataset_id","collection_title","interview_date","collection_id","src_subject_id","sex")) %>%
  dplyr::select("subjectkey","demo_prim","demo_brthdat_v2","demo_ed_v2","demo_sex_v2","interview_age",
                starts_with("demo_race_a_p"),"demo_ethn_v2","demo_ethn2_v2",
                "demo_prnt_age_v2","demo_prnt_gender_id_v2","demo_prnt_marital_v2","demo_prnt_ed_v2",
                "demo_prnt_empl_v2","demo_prnt_empl_time","demo_prnt_income_v2","demo_prnt_prtnr_v2",
                "demo_prnt_prtnr_bio","demo_prtnr_ed_v2","demo_prtnr_empl_v2","demo_prtnr_empl_time",
                "demo_prtnr_income_v2","demo_comb_income_v2",starts_with("demo_fam_exp")) %>%
  na_if("777") %>%
  na_if("999")

abcd_pdemo[,2:ncol(abcd_pdemo)] <- as.data.frame(lapply(abcd_pdemo[,2:ncol(abcd_pdemo)], as.numeric))

##remove 3 from sex (intersex-male, only 3 endorsed, out of ~10000)
abcd_pdemo$demo_sex_v2 <- abcd_pdemo$demo_sex_v2 %>%
  na_if("3")
##change data type to numeric####
abcd_pdemo[,2:ncol(abcd_pdemo)] <- as.data.frame(lapply(abcd_pdemo[,2:ncol(abcd_pdemo)], as.numeric))

#####medical history and service utilization --> only at 1 year follow up and 2 year follow up#####
abcd_medServ <- fread(paste0(path_dat, "abcd_lpmh01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(-c("eventname","dataset_id","collection_title")) %>%
  dplyr::select("subjectkey","medhx_1a_l") %>%
  na_if(6)

abcd_medServ[,2:ncol(abcd_medServ)] <- as.data.frame(lapply(abcd_medServ[,2:ncol(abcd_medServ)], as.numeric))


#####Teacher form####
#https://nda.nih.gov/data_structure.html?short_name=abcd_ssbpmtf01
abcd_pmonitor <- fread(paste0(path_dat, "abcd_ssbpmtf01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(-c("eventname","dataset_id","collection_title")) %>%
  dplyr::select(subjectkey, bpm_t_scr_attention_r
                #,bpm_t_scr_external_r
                )

abcd_pmonitor[,2:ncol(abcd_pmonitor)] <- as.data.frame(lapply(abcd_pmonitor[,2:ncol(abcd_pmonitor)], as.numeric))

#####CBCL syndrome scales####
abcd_cbcls <- fread(paste0(path_dat, "abcd_cbcls01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(-c("eventname","dataset_id","collection_title")) %>%
  dplyr::select(subjectkey, cbcl_scr_syn_anxdep_t,cbcl_scr_syn_withdep_t,cbcl_scr_syn_somatic_t,
                cbcl_scr_syn_social_t,cbcl_scr_syn_thought_t,cbcl_scr_syn_attention_t,
                cbcl_scr_syn_rulebreak_t,cbcl_scr_syn_aggressive_t,cbcl_scr_syn_internal_t,
                cbcl_scr_syn_external_t,cbcl_scr_syn_totprob_t
                #cbcl_scr_dsm5_depress_t,cbcl_scr_dsm5_anxdisord_t,cbcl_scr_dsm5_somaticpr_t,cbcl_scr_dsm5_adhd_t,cbcl_scr_dsm5_opposit_t,cbcl_scr_dsm5_conduct_t,
                #cbcl_scr_07_sct_t,cbcl_scr_07_ocd_t,cbcl_scr_07_stress_t
  )

abcd_cbcls[,2:ncol(abcd_cbcls)] <- as.data.frame(lapply(abcd_cbcls[,2:ncol(abcd_cbcls)], as.numeric))


#####Parent KSADS####
#https://nda.nih.gov/data_structure.html?short_name=abcd_ksad01

abcd_ksads <- fread(paste0(path_dat, "abcd_ksad01.txt")) %>%   ###should be abcd_ksad01
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(-c("eventname","dataset_id","collection_title")) %>%
  mutate(checkpoint = 2, id = row_number()) %>%
  dplyr::select(subjectkey, ksads_14_398_p, ksads_14_403_p, #ksads_14_856_p, ksads_14_853_p, no diagnosis data
                ksads_14_405_p, ksads_14_406_p, ksads_14_76_p, ksads_14_396_p, ksads_14_397_p, ksads_14_404_p,
                ksads_14_85_p, ksads_14_77_p, ksads_14_84_p, ksads_14_401_p, ksads_14_80_p, ksads_14_81_p,
                ksads_14_400_p, ksads_14_88_p, ksads_14_399_p, ksads_14_408_p, ksads_14_394_p, ksads_14_395_p,
                ksads_14_407_p, ksads_14_402_p, ksads_14_409_p) %>% 
  na_if(555) %>%
  na_if(888)

colnames(abcd_ksads)
abcd_ksads[,2:ncol(abcd_ksads)] <- as.data.frame(lapply(abcd_ksads[,2:ncol(abcd_ksads)], as.numeric))


####merge into a single data object####
abcd_merged <- list(abcd_cbcls, abcd_ksads, abcd_medServ, abcd_pdemo, abcd_pmonitor, abcd_race) %>%
  reduce(full_join, by = "subjectkey")

####check data ####
##check data type (chr or numeric), mean, range, NA, missing data##
skim_tee(abcd_merged)

####save to RData####
setwd(path_dat)
#saveRDS(abcd_merged, "abcd_merged.RData")
write.csv(abcd_merged, "abcd_merged.csv", row.names = FALSE)
