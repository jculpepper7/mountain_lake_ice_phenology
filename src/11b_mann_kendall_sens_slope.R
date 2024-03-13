#Run Mann Kendall and Sen's Slope on Sierra lakes data
library(tidyverse)
library(Kendall)
library(openair)
library(wql)

remote_iceOn <- read_csv(here("data/remote/sierra_lake_ice_data/sierra_ice_on.csv"))

remote_iceOff <- read_csv(here("data/remote/sierra_lake_ice_data/sierra_ice_off.csv"))

hydro_lakes <- read_csv(here('data/remote/sierra_lake_ice_data/HydroLAKES_N_America.csv')) %>%
  clean_names() %>%
  select(-system_index) %>%
  arrange(hylak_id) %>%
  select(hylak_id, pour_lat, pour_long, elevation, lake_area)
head(hydro_lakes)

# 1. ICE ON---------------------------------------------------------------------

ice_on_mk_test <- iceOn_full_data %>%
  na.omit() %>%
  group_by(hylak_id) %>%
  filter(elevation >= 1500,
         n() >= 20) %>% #filtering out any lakes with fewer than 10 rows of data
  mutate(ice_on_yday = yday(date),
         yday_w_year = ifelse(ice_on_yday >= 274, ice_on_yday - 273, ice_on_yday + 92)) %>%
  summarize(ice_on_sen = mannKen(yday_w_year)$sen.slope,
            ice_on_rel_slope = mannKen(yday_w_year)$sen.slope.rel,
            ice_on_p.value = mannKen(yday_w_year)$p.value) %>%
  ungroup

unique(ice_on_mk_test$hylak_id) #filtering out any lakes with fewer than 10 rows of data leaves 407 lakes greater than 1500m in elevation ## 284 if we filter to only 20 years of data

ice_on_mk_test_pos <- ice_on_mk_test %>%
  filter(ice_on_sen >0,
         ice_on_p.value<0.05) %>%  #62 with positive trend (so later in the year ice on) (57%) #update: 4.20.2022 - 305 lakes with pos trend (later ice formation)
  summarise(mean_ice_on_trend = median(ice_on_sen),
            mean_ice_on_relative_slope = mean(ice_on_rel_slope))
ice_on_mk_test_neg <- ice_on_mk_test %>%
  filter(ice_on_tau <0) %>%  #41 with negative trend (so earlier in the year ice on) (38%) #update: 4.20.2022 - 99 with neg trend (earlier ice formation)
  summarise(mean_ice_on_trend = mean(ice_on_tau))
ice_on_mk_test_zero <- ice_on_mk_test %>%
  filter(ice_on_tau ==0) #5 with 0 so no trend (5%) #update: 4.20.2022 - 3 lake with no trend

ice_on_mk_test %>% 
  filter(ice_on_tau > 0,
         ice_on_p.value <0.05) 

# 2. ICE ON---------------------------------------------------------------------

iceOff_full_data <- remote_iceOff %>%
  left_join(hydro_lakes, by = 'hylak_id') %>%
  select(-median_val, -median_iceFrac) 

#try mann-kendall test for ice off
ice_off_mk_test <- iceOff_full_data %>%
  na.omit() %>%
  group_by(hylak_id) %>%
  filter(elevation >= 1500,
         n() >= 20) %>% 
  mutate(ice_off_yday = yday(date),
         yday_w_year = ifelse(ice_off_yday >= 274, ice_off_yday - 273, ice_off_yday + 92)) %>%
  summarize(ice_off_tau = MannKendall(yday_w_year)$tau,
          ice_off_p.value = MannKendall(yday_w_year)$sl,
          ice_off_sen = mannKen(yday_w_year)$sen.slope,
          ice_off_rel_slope = mannKen(yday_w_year)$sen.slope.rel) %>%
  ungroup

unique(ice_off_mk_test$hylak_id)

ice_off_mk_test_pos <- ice_off_mk_test %>%
  filter(ice_off_tau >0) %>%  #5 with positive trend (so later in the year ice off) (4%) #update 4.20.2022: 22 lakes with positive (later ice off) - trend = 0.12 days per year
  summarise(mean_ice_off_tau = mean(ice_off_tau),
            mean_ice_off_trend = median(ice_off_sen),
            mean_ice_off_relative_slope = mean(ice_off_rel_slope))

ice_off_mk_test_neg <- ice_off_mk_test %>%
  filter(ice_off_tau < 0) #%>%  #109 with negative trend (so earlier in the year ice off) (95%) #update 4.20.2022: 380 (95%)
  summarise(mean_ice_off_tau = mean(ice_off_tau),
          mean_ice_off_trend = median(ice_off_sen),
          mean_ice_off_relative_slope = mean(ice_off_rel_slope))

  ice_off_mk_test_zero <- ice_off_mk_test %>%
  filter(ice_off_tau ==0) #1 with 0 so no trend (1%)


  
# 3. ICE DURATION---------------------------------------------------------------  
  
ice_on_grouping <- iceOn_full_data %>%
    na.omit() %>%
    group_by(hylak_id) %>%
    filter(elevation >= 1500,
           n() >= 20) %>% #filtering out any lakes with fewer than 10 rows of data
    mutate(ice_on_yday = yday(date),
           yday_w_year = ifelse(ice_on_yday >= 274, ice_on_yday - 273, ice_on_yday + 92)) %>%
    ungroup %>% 
    select(hylak_id, water_year = ice_on_water_year, elevation, ice_on_yday = yday_w_year)
  

ice_off_grouping <- iceOff_full_data %>%
    na.omit() %>%
    group_by(hylak_id) %>%
    filter(elevation >= 1500,
           n() >= 20) %>% 
    mutate(ice_off_yday = yday(date),
           yday_w_year = ifelse(ice_off_yday >= 274, ice_off_yday - 273, ice_off_yday + 92)) %>%
    ungroup %>% 
    select(hylak_id, water_year = ice_off_water_year, elevation, ice_off_yday = yday_w_year)

#Group ice on and ice off for DURATION

grouped_ice <- ice_on_grouping %>% 
  inner_join(ice_off_grouping) %>% 
  mutate(
    duration = ice_off_yday - ice_on_yday
  ) %>% 
  filter(duration > 14)

unique(grouped_ice$hylak_id)  


duration_trend <- grouped_ice %>% 
  group_by(hylak_id) %>%
  summarize(duration_tau = MannKendall(duration)$tau,
            duration_p.value = MannKendall(duration)$sl,
            duration_sen = mannKen(duration)$sen.slope,
            duration_rel_slope = mannKen(duration)$sen.slope.rel) %>%
  ungroup

unique(duration_trend$hylak_id)

mean_duration_trend <- duration_trend %>% 
  select(duration_tau, duration_sen, duration_rel_slope) %>% 
  summarise(tau_mean = mean(duration_tau),
            sen_mean = mean(duration_sen),
            sen_median = median(duration_sen),
            rel_sen_mean = mean(duration_rel_slope))


duration_trend %>% 
  filter(duration_tau < 0)
unique(mean_duration_trend)
# With 20 years of data, a total of 284 lakes
# Total of 206 with pos trend which is 72% of lakes with 20 years of data
# Average loss of 0.17 per year or 1.7 days per decade for 
# 
# 402 lakes have 20 years of data for ice off / 380 with a negative trend
# 0.55 days per dacade later ice formation
# 5.5 days per decade


