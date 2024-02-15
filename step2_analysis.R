library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(skimr)
library(broom)
library(purrr)
options(scipen=999)
####Read data####
#path_dat <- "C:/Users/zpeng/OneDrive/Desktop/ABCD/Analysis data/ADHD_informant_race"
path_dat <- "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/ADHD_informant_race"
data <- read.csv(file.path(path_dat, "abcd_merged.csv"))
data <- data %>% filter(!is.na(Race_three))


####Descriptive statistics#### 
## simply skim use the skim() or do the following... :(
data_mean <- data %>% select(-c("subjectkey")) %>% lapply(., function(x) mean(x, na.rm = T)) %>% as.data.frame() %>% t()
data_sd <- data %>% select(-c("subjectkey")) %>% lapply(., function(x) sd(x, na.rm = T)) %>% as.data.frame() %>% t()
data_min <- data %>% select(-c("subjectkey")) %>% lapply(., function(x) min(x, na.rm = T)) %>% as.data.frame() %>% t()
data_max <- data %>% select(-c("subjectkey")) %>% lapply(., function(x) max(x, na.rm = T)) %>% as.data.frame() %>% t()
data_range <- data %>% select(-c("subjectkey")) %>% lapply(., function(x) range(x, na.rm = T)) %>% as.data.frame() %>% t()

data_descriptive_stats <- cbind(data_mean, data_sd, data_min, data_max, data_range) %>% as.data.frame() %>% rename(mean = V1, sd = V2, min = V3, max = V4, range = V5)


####sum ksads core symp score####
data$adhd_core_sum <- data %>% 
  select(c("ksads_14_77_p", "ksads_14_76_p", "ksads_14_88_p","ksads_14_80_p", "ksads_14_81_p")) %>% ##why these five? Because they are the first questions? All are missing 151, which are the lowest missing value
  rowSums(na.rm = F)

####Within each racial-ethnic group across informant####
##compare t-score 
#hispanic
list_cbcl_hispanic <- data %>% filter(Race_three == "0") %>% dplyr::select("cbcl_scr_syn_attention_t", subjectkey) %>% rename(attention = cbcl_scr_syn_attention_t)
list_cbcl_hispanic$level <- 0
list_bpm_hispanic <- data %>% filter(Race_three == "0") %>% dplyr::select("bpm_t_scr_attention_t", subjectkey) %>% rename(attention = bpm_t_scr_attention_t)
list_bpm_hispanic$level <- 1
hispanic <- rbind(list_bpm_hispanic, list_cbcl_hispanic)
hispanic$race <- 0
t_test_attention_hispanicxinfo <- rbind(list_bpm_hispanic, list_cbcl_hispanic) %>% t.test(attention ~ factor(level), data = ., alt = "two.sided", na.rm = T) %>%
  print()

#white
list_cbcl_white <- data %>% filter(Race_three == "1") %>% dplyr::select("cbcl_scr_syn_attention_t", subjectkey) %>% rename(attention = cbcl_scr_syn_attention_t)
list_cbcl_white$level <- 0
list_bpm_white <- data %>% filter(Race_three == "1") %>% dplyr::select("bpm_t_scr_attention_t", subjectkey) %>% rename(attention = bpm_t_scr_attention_t)
list_bpm_white$level <- 1
white <- rbind(list_bpm_white, list_cbcl_white)
white$race <- 1

t_test_attention_whitexinfo <- rbind(list_bpm_white, list_cbcl_white) %>% t.test(attention ~ factor(level), data = ., alt = "two.sided", na.rm = T) %>%
  print()

#black
list_cbcl_black <- data %>% filter(Race_three == "2") %>% dplyr::select("cbcl_scr_syn_attention_t", subjectkey) %>% rename(attention = cbcl_scr_syn_attention_t)
list_cbcl_black$level <- 0
list_bpm_black<- data %>% filter(Race_three == "2") %>% dplyr::select("bpm_t_scr_attention_t", subjectkey) %>% rename(attention = bpm_t_scr_attention_t)
list_bpm_black$level <- 1
black <- rbind(list_bpm_black, list_cbcl_black)
black$race <- 2
t_test_attention_blackxinfo <- rbind(list_bpm_black, list_cbcl_black) %>% t.test(attention ~ factor(level), data = ., alt = "two.sided", na.rm = T) %>%
  print()

#combined 
library(fastDummies)
combined <- rbind(hispanic, white, black)
combined <- merge(combined, data %>% dplyr::select(subjectkey, demo_prnt_ed_v2, demo_sex_v2, interview_age, demo_comb_income_v2), by = "subjectkey")

combined$parent_edu_recoded <-
  case_when(
    combined$demo_prnt_ed_v2 %in% c(1:8) ~ 1,
    combined$demo_prnt_ed_v2 %in% c(9:12) ~ 2,
    combined$demo_prnt_ed_v2 %in% 13 ~ 3,
    combined$demo_prnt_ed_v2 %in% 14 ~ 4,
    combined$demo_prnt_ed_v2 %in% 15 ~ 5,
    combined$demo_prnt_ed_v2 %in% c(16,17) ~ 6,
    combined$demo_prnt_ed_v2 %in% 18 ~ 7,
    combined$demo_prnt_ed_v2 %in% 19 ~ 8,
    combined$demo_prnt_ed_v2 %in% c(20,21) ~ 9
  )

##interaction between informant and Race-ethnicity
library(lme4)
interaction_combined_lmer <- lmer(attention ~ factor(level) * dummy(race)
                                          + factor(demo_sex_v2) + scale(interview_age) + 
                                          scale(demo_comb_income_v2) + scale(parent_edu_recoded) +
                                          (1|subjectkey), data = combined)
summary(interaction_combined_lmer)
####performance of the mixed model
performance::performance(interaction_combined_lmer)
library(merTools)
####fixed and random effect visualization
fesm <- FEsim(interaction_combined_lmer,100)
plotFEsim(fesm)
resm <- REsim(interaction_combined_lmer, 100)
plotREsim(resm)
####Between racial-ethnic group and within informant####

#####look at the mean of adhd_core_sum ... in each group####
data %>% group_by(Race_three) %>% 
  summarise(mean_adhd = mean(adhd_core_sum, na.rm = T), sd_adhd = sd(adhd_core_sum, na.rm = T),
            mean_cbcl_attention = mean(cbcl_scr_syn_attention_t, na.rm = T), sd_cbcl_attention= sd(cbcl_scr_syn_attention_t, na.rm = T), 
            mean_bpm_attention = mean(bpm_t_scr_attention_t, na.rm = T), sd_bpm_attention = sd(bpm_t_scr_attention_t, na.rm = T),
            mean_cbcl_external = mean(cbcl_scr_syn_external_t, na.rm = T), sd_cbcl_external = sd(cbcl_scr_syn_external_t, na.rm = T),
            mean_cbcl_totprob = mean(cbcl_scr_syn_totprob_t, na.rm = T), sd_cbcl_totprob = sd(cbcl_scr_syn_totprob_t, na.rm = T)) ##what about cbcl_scr_syn_attention_t?

#####distribution of adhd core sum in each racial/ethnic group####
ggplot(data, aes(x = factor(Race_three), y = adhd_core_sum, mean = T)) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

ggplot(data, aes(x = factor(Race_three), y = cbcl_scr_syn_attention_t, mean = T)) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

ggplot(data, aes(x = factor(Race_three), y = bpm_t_scr_attention_t, mean = T)) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

ggplot(data, aes(x = factor(Race_three), y = cbcl_scr_syn_totprob_t, mean = T)) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

ggplot(data, aes(x = factor(Race_three), y = cbcl_scr_syn_external_t, mean = T)) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

#####anova among racial-ethnic groups####
data %>% filter(adhd_core_sum != "NA") %>% aov(adhd_core_sum ~ factor(Race_three), .) %>% summary()
data %>% filter(cbcl_scr_syn_attention_t != "NA") %>% aov(cbcl_scr_syn_attention_t ~ factor(Race_three), .) %>% summary()
data %>% filter(bpm_t_scr_attention_t != "NA") %>% aov(bpm_t_scr_attention_t ~ factor(Race_three), .) %>% summary() 

data %>% filter(cbcl_scr_syn_totprob_t != "NA") %>% aov(cbcl_scr_syn_totprob_t ~ factor(Race_three), .) %>% summary()
data %>% filter(cbcl_scr_syn_external_t != "NA") %>% aov(cbcl_scr_syn_external_t ~ factor(Race_three), .) %>% summary()
###??why use rnorm() for each outcome ?

#####t-test between racial-enthnic groups#####  ----> better to do ANOVA and compare the three groups; and then do post-hoc analyses for pair-wise comparison. i.e., TukeyHSD() in R, and put the ANOVA object as the argument for this function
Hispanic_white <- data %>% filter(Race_three == "0"|Race_three =="1")
black_white <- data %>% filter(Race_three == "2"|Race_three =="1") 
hispanic_black <- data %>% filter(Race_three == "0"|Race_three =="2") 

Hispanic_white_ttest <- lapply(Hispanic_white %>% select(adhd_core_sum, cbcl_scr_syn_attention_t, bpm_t_scr_attention_t, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ Hispanic_white$Race_three, alt = "two.sided"))
Black_white_ttest <- lapply(black_white %>% select(adhd_core_sum, cbcl_scr_syn_attention_t, bpm_t_scr_attention_t, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ black_white$Race_three, alt = "two.sided"))
Hispanic_black_ttest <- lapply(hispanic_black %>% select(adhd_core_sum, cbcl_scr_syn_attention_t, bpm_t_scr_attention_t, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ hispanic_black$Race_three, alt = "two.sided"))

  
map_df(Hispanic_white_ttest, tidy)[, -1] %>% rev() %>% rename(mean_white = estimate2, mean_hispanic = estimate1)
map_df(Black_white_ttest, tidy)[, -1] %>% rev() %>% rename(mean_white = estimate2, mean_black = estimate1)
map_df(Hispanic_black_ttest, tidy)[, -1] %>% rev() %>% rename(mean_black = estimate2, mean_hispanic = estimate1)


#####TukeyHSD
data %>% filter(cbcl_scr_syn_attention_t != "NA") %>% aov(cbcl_scr_syn_attention_t ~ factor(Race_three), .) %>% TukeyHSD() %>% plot()
data %>% filter(bpm_t_scr_attention_t != "NA") %>% aov(bpm_t_scr_attention_t ~ factor(Race_three), .) %>% TukeyHSD() %>% plot()
data %>% filter(adhd_core_sum != "NA") %>% aov(adhd_core_sum ~ factor(Race_three), .) %>% TukeyHSD() %>% plot()

