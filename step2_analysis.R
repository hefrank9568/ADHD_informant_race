library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(skimr)
library(broom)
library(purrr)
####Read data####
data <- read.csv("C:/Users/zpeng/OneDrive/Desktop/ABCD/Analysis data/ADHD_informant_race/abcd_merged.csv")
data <- data %>% filter(Race_three != "NA")
skim(data)

data$adhd_core_sum <- data %>% 
  select(c("ksads_14_77_p", "ksads_14_76_p", "ksads_14_88_p","ksads_14_80_p", "ksads_14_81_p")) %>%
  rowSums(na.rm = F)


####Within each racial-ethnic group across informant####
#compare t-score 
#data %>% filter(Race_three == "0")

#data %>% filter(Race_three == "1")

#data %>% filter(Race_three == "2")
####Between racial-ethnic group and within informant####

#####look at the mean of adhd_core_sum in each group####
data %>% filter(adhd_core_sum != "NA") %>% group_by(Race_three) %>% summarise_at(vars(adhd_core_sum), list(~ mean(., na.rm = T)))
data %>% filter(cbcl_scr_syn_external_t != "NA") %>% group_by(Race_three) %>% summarise_at(vars(cbcl_scr_syn_external_t), list(~ mean(., na.rm = T)))
data %>% filter(cbcl_scr_syn_totprob_t != "NA") %>% group_by(Race_three) %>% summarise_at(vars(cbcl_scr_syn_totprob_t), list(~ mean(., na.rm = T)))

#####distribution of adhd core sum in each racial/ethnic group####
ggplot(data, aes(x = factor(Race_three), y = rnorm(adhd_core_sum, mean = T))) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

ggplot(data, aes(x = factor(Race_three), y = rnorm(cbcl_scr_syn_totprob_t, mean = T))) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

ggplot(data, aes(x = factor(Race_three), y = rnorm(cbcl_scr_syn_external_t, mean = T))) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95))

#####anova among racial-ethnic groups####
data %>% filter(adhd_core_sum != "NA") %>% aov(rnorm(adhd_core_sum, mean = T) ~ factor(Race_three), .) %>% summary()
data %>% filter(cbcl_scr_syn_totprob_t != "NA") %>% aov(rnorm(cbcl_scr_syn_totprob_t, mean = T) ~ factor(Race_three), .) %>% summary()
data %>% filter(cbcl_scr_syn_external_t != "NA") %>% aov(rnorm(cbcl_scr_syn_external_t, mean = T) ~ factor(Race_three), .) %>% summary()

#####t-test between racial-enthnic groups####
Hispanic_white <- data %>% filter(Race_three == "0"|Race_three =="1")
black_white <- data %>% filter(Race_three == "2"|Race_three =="1") 
hispanic_black <- data %>% filter(Race_three == "0"|Race_three =="2") 


Hispanic_white_ttest <- lapply(Hispanic_white %>% select(adhd_core_sum, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ Hispanic_white$Race_three, alt = "two.sided"))
Black_white_ttest <- lapply(black_white %>% select(adhd_core_sum, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ black_white$Race_three, alt = "two.sided"))
Hispanic_black_ttest <- lapply(hispanic_black %>% select(adhd_core_sum, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ hispanic_black$Race_three, alt = "two.sided"))

  
map_df(Hispanic_white_ttest, tidy)[, -1] %>% rev() %>% rename(mean_white = estimate2, mean_hispanic = estimate1)
map_df(Black_white_ttest, tidy)[, -1] %>% rev() %>% rename(mean_white = estimate2, mean_black = estimate1)
map_df(Hispanic_black_ttest, tidy)[, -1] %>% rev() %>% rename(mean_black = estimate2, mean_hispanic = estimate1)



