## The purpose of this script is to 
##
## 1. Visualize distributions of ice on and off trends
## 2. Test if distributions are normal 
## 3. Perform t-test on the distributions to determine if trends are different from 0

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(here)
library(nortest) #for Anderson-Darling test
library(rstatix) #for Wilcoxon test
library(coin) #for Wilcoxon effect size
library(ggpubr)
library(ggridges)
library(patchwork)

# 2. Import data----------------------------------------------------------------

mk_sen_test <- read_csv(here('data/n_american_ice_phenology/results/mk_results.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  )

rs_results_tbl_ice_on <- read_csv(here('data/n_american_ice_phenology/results/rs_results_tbl_ice_on.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  ) %>% 
  rename(
    class = .pred_class
  )

rs_results_tbl_ice_off <- read_csv(here('data/n_american_ice_phenology/results/rs_results_tbl_ice_off.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  ) %>% 
  rename(
    class = .pred_class
  )

#Combine datasets
ice_on_trend <- mk_sen_test %>% 
  filter(
    event == 'ice_on', 
    #p.value<=0.05
    ) %>% 
  left_join(rs_results_tbl_ice_on) %>% 
  filter(class == 'suitable',
         pour_lat >= 35)


ice_off_trend <- mk_sen_test %>% 
  filter(
    event == 'ice_off', 
    #p.value<=0.05
    ) %>% 
  left_join(rs_results_tbl_ice_off) %>% 
  filter(class == 'suitable',
         pour_lat >= 35)

# 3. Visualize distribution-----------------------------------------------------

ice_on_distribution <- ggplot()+
  geom_histogram(data = ice_on_trend, aes(x = sen))+
  theme_classic()+
  xlab('Ice On Sen Slope')+
  ylab('')

ice_off_distribution <- ggplot()+
  geom_histogram(data = ice_off_trend, aes(x = sen))+
  theme_classic()+
  xlab('Ice Off Sen Slope')+
  ylab('')

#boxplot of both trends
ice_on_distribution <- ggplot()+
  geom_boxplot(data = ice_off_trend, aes(y = sen, x = 'Off'))+
  geom_boxplot(data = ice_on_trend, aes(y = sen, x = 'On'))+
  geom_hline(yintercept = 0)+
  theme_classic()+
  xlab('Ice On Sen Slope')+
  ylab('')
ice_on_distribution

#Both look somewhat normal, but with a right skew
# Look at Q-Q plot

ice_on_qq <- ggplot(data = ice_on_trend, aes(sample = sen))+
  stat_qq() #Roughly along a straight line

ice_off_qq <- ggplot(data = ice_off_trend, aes(sample = sen))+
  stat_qq() #Roughly along a straight line

# 4. Test ice on and off sen slope with stat test for normality-----------------

#Ice on SW test

#Shapiro-Wilk Test
shapiro_test(ice_on_trend$sen) 
  
shapiro.test(ice_off_trend$sen)

#both tests lower than 0.05 threshold. Likely not normally distributed

#Kolmogorov-Smirnov Test
ks.test(ice_on_trend$sen ,'pnorm')

ks.test(ice_off_trend$sen ,'pnorm')

#both tests lower than 0.05 threshold. Likely not normally distributed

#Anderson-Darling Test
ad.test(ice_on_trend$sen)

ad.test(ice_off_trend$sen)

#both tests lower than 0.05 threshold. Likely not normally distributed

# 5. Use nonparametric Wilcoxon test ------------------------------------------

# 5a. Wilcoxon test for ice on trends----
wilcox_test_on <- ice_on_trend %>% 
  rstatix::wilcox_test(sen ~ 1, mu = 0)
wilcox_test_on

ice_on_trend %>% 
  wilcox_effsize(sen ~ 1, mu = 0) #Effect size of 0.148 (so 0.148 days per year or 1.5 days per decade)

#density plot with p-value
ice_on_trend_plt <- ggdensity(ice_on_trend, x = 'sen', rug = TRUE, fill = 'lightgray')+
  scale_x_continuous(limits = c(-2, 2))+
  stat_central_tendency(type = 'median', color = 'black', linetype = 'dashed', size = 1.1)+
  geom_vline(xintercept = 0, color = 'black', linetype = 'solid', size = 1.1)+
  #labs(subtitle = get_test_label(wilcox_test_on))+ #, detailed = TRUE
  #labs(subtitle = create_test_label(p = '<0.001'))+ #, detailed = TRUE
  labs(x = '', y = '', subtitle = 'Ice Formation')+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.subtitle = element_text(size = 22, vjust = -8, hjust = 0.05))

#ggsave(here('output/wilcoxon_tests/ice_on_all_years_revised.jpeg'), dpi = 300, width = 15, height = 10, units = 'in')

median(ice_on_trend$sen) #0.08 (i.e., 0.08 days per year, 0.8 days per decade, 8.0 days per century)
mean(ice_on_trend$sen)   #0.05 (i.e., 0.05 days per year, 0.5 days per decade, 5.0 days per century)

# 5b. Wilcox test for ice off trends----
wilcox_test_off <- ice_off_trend %>% 
  rstatix::wilcox_test(sen ~ 1, mu = 0)
wilcox_test_off

ice_off_trend %>% 
  wilcox_effsize(sen ~ 1, mu = 0) #Effect size of 0.266 (so 0.266 days per year or 2.7 days per decade)

#density plot with p-value
ice_off_trend_plt <- ggdensity(ice_off_trend, x = 'sen', rug = TRUE, fill = 'lightgray')+
  scale_x_continuous(limits = c(-2, 2))+
  stat_central_tendency(type = 'median', color = 'black', linetype = 'dashed', size = 1.1)+
  geom_vline(xintercept = 0, color = 'black', linetype = 'solid', size = 1.1)+
  #labs(subtitle = get_test_label(wilcox_test_off, detailed = TRUE))+
  labs(x = '', y = '', subtitle = 'Ice Breakup')+
  theme(axis.text = element_text(size = 20),
        #axis.title = element_text(size = 25),
        plot.subtitle = element_text(size = 22, vjust = -8, hjust = 0.05))

#ggsave(here('output/wilcoxon_tests/ice_off_all_years_revised.jpeg'), dpi = 300, width = 6, height = 4, units = 'in')


median(ice_off_trend$sen) #-0.07 (i.e., 0.07 days per year, 0.7 days per decade, 7.0 days per century)
mean(ice_off_trend$sen)   #-0.1 (i.e., 0.1 days per year, 1.0 days per decade, 10 days per century)


