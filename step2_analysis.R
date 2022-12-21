library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(skimr)
library(broom)
library(purrr)
####Read data####
path_dat <- "C:/Users/zpeng/OneDrive/Desktop/ABCD/Analysis data/ADHD_informant_race"
#path_dat <- "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/ADHD_informant_race"
data <- read.csv(file.path(path_dat, "abcd_merged.csv"))
data <- data %>% filter(Race_three != "NA")


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
list_cbcl <- data %>% filter(Race_three == "0") %>% select("cbcl_scr_syn_attention_t") %>% rename(attention = cbcl_scr_syn_attention_t)
list_cbcl$level <- 0
list_bpm <- data %>% filter(Race_three == "0") %>% select("bpm_t_scr_attention_t") %>% rename(attention = bpm_t_scr_attention_t)
list_bpm$level <- 1

t_test_attention_hispanicxinfo <- rbind(list_bpm, list_cbcl) %>% t.test(rnorm(attention) ~ factor(level), data = ., alt = "two.sided", na.rm = T) %>%
  print()

#white
list_cbcl <- data %>% filter(Race_three == "1") %>% select("cbcl_scr_syn_attention_t") %>% rename(attention = cbcl_scr_syn_attention_t)
list_cbcl$level <- 0
list_bpm <- data %>% filter(Race_three == "1") %>% select("bpm_t_scr_attention_t") %>% rename(attention = bpm_t_scr_attention_t)
list_bpm$level <- 1

t_test_attention_whitexinfo <- rbind(list_bpm, list_cbcl) %>% t.test(rnorm(attention) ~ factor(level), data = ., alt = "two.sided", na.rm = T) %>%
  print()

#black
list_cbcl <- data %>% filter(Race_three == "2") %>% select("cbcl_scr_syn_attention_t") %>% rename(attention = cbcl_scr_syn_attention_t)
list_cbcl$level <- 0
list_bpm <- data %>% filter(Race_three == "2") %>% select("bpm_t_scr_attention_t") %>% rename(attention = bpm_t_scr_attention_t)
list_bpm$level <- 1

t_test_attention_blackxinfo <- rbind(list_bpm, list_cbcl) %>% t.test(rnorm(attention) ~ factor(level), data = ., alt = "two.sided", na.rm = T) %>%
  print()


#data %>% filter(Race_three == "2")

####Between racial-ethnic group and within informant####

#####look at the mean of adhd_core_sum ... in each group####
data %>% group_by(Race_three) %>% 
  summarise(mean_adhd = mean(adhd_core_sum, na.rm = T), sd_adhd = sd(adhd_core_sum, na.rm = T),
            mean_cbcl_external = mean(cbcl_scr_syn_external_t, na.rm = T), sd_cbcl_external = sd(cbcl_scr_syn_external_t, na.rm = T),
            mean_cbcl_totprob = mean(cbcl_scr_syn_totprob_t, na.rm = T, sd_cbcl_totprob = sd(cbcl_scr_syn_totprob_t, na.rm = T))) ##what about cbcl_scr_syn_attention_t?

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

#####t-test between racial-enthnic groups####  ----> better to do ANOVA and compare the three groups; and then do post-hoc analyses for pair-wise comparison. i.e., TukeyHSD() in R, and put the ANOVA object as the argument for this function
Hispanic_white <- data %>% filter(Race_three == "0"|Race_three =="1")
black_white <- data %>% filter(Race_three == "2"|Race_three =="1") 
hispanic_black <- data %>% filter(Race_three == "0"|Race_three =="2") 


Hispanic_white_ttest <- lapply(Hispanic_white %>% select(adhd_core_sum, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ Hispanic_white$Race_three, alt = "two.sided"))
Black_white_ttest <- lapply(black_white %>% select(adhd_core_sum, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ black_white$Race_three, alt = "two.sided"))
Hispanic_black_ttest <- lapply(hispanic_black %>% select(adhd_core_sum, cbcl_scr_syn_external_t, cbcl_scr_syn_totprob_t), function(x)t.test(x ~ hispanic_black$Race_three, alt = "two.sided"))

  
map_df(Hispanic_white_ttest, tidy)[, -1] %>% rev() %>% rename(mean_white = estimate2, mean_hispanic = estimate1)
map_df(Black_white_ttest, tidy)[, -1] %>% rev() %>% rename(mean_white = estimate2, mean_black = estimate1)
map_df(Hispanic_black_ttest, tidy)[, -1] %>% rev() %>% rename(mean_black = estimate2, mean_hispanic = estimate1)



