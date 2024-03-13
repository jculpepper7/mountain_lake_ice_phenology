require(tidyverse)
require(lubridate)
require(here)


# 1. Set up remote and in situ data ---------------------------------------


rs = read_csv(here("data/combined/modis_allLakes_output_Terra_Aqua_merged_raw.csv")) #raw MODIS data without Norwegian lakes
nor_lakes <- read_csv(here('data/combined/modis_norwegian_lakes.csv'))
# rs = read_csv(here('data/remote/aqua_terra_merged_clean_outlier_removed.csv'))%>%
#   rename(iceModis = full_merge)
ref = read_csv(here("data/combined/all_insitu_water_year_1.csv")) 


rs = rs %>%
  bind_rows(nor_lakes) #%>% 
#filter(source == 'MODISAqua')

## remote sensing data

rs = rs %>%
  mutate(date = as.Date(date),
         lakename = as.factor(lakename),
         source = as.factor(source))

rs = rs %>% 
  mutate(date = as.Date(date),
         lakename = as.factor(lakename))

rs %>%
  filter(date >= "2014-01-01",
         !is.na(iceModis),
         cloudModis <= 0.1) %>%
  ggplot(aes(x = date, y = iceModis)) +
  geom_line(alpha = 0.2) +
  geom_point(aes(color = cloudModis), size = 0.2) +
  scale_color_viridis_c() +
  facet_wrap(~lakename)


## format date correctly for in situ data
ref = ref %>% 
  mutate(
    ice_on_insitu = as.Date(ice_on_insitu, format = "%m/%d/%Y"),
    ice_off_insitu = as.Date(ice_off_insitu, format = "%m/%d/%Y")) %>% 
  select(lakename, ice_on_insitu, ice_off_insitu) %>% 
  rename(ice_on = ice_on_insitu,
         ice_off = ice_off_insitu) %>% 
  pivot_longer(cols = c(ice_on, ice_off), names_to = "event", values_to = "date") %>% 
  filter(!is.na(date)) %>% 
  mutate(lakename = as.factor(lakename),
         event = as.factor(event))

summary(ref)

ref %>% 
  ggplot() +
  geom_histogram(aes(x = yday(date)), color = "white") +
  facet_grid(lakename~event)


# 2. lake ice on off date estimate from rs --------------------------------

rs_clean = rs %>% filter(!is.na(iceModis))

rs_clean = rs_clean %>% 
  filter(cloudModis <= 0.1) %>% 
  mutate(
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1)) %>%
  #water_year = if_else(month(date) >= 10, year(date), year(date) - 1)) %>%
  filter(lakename %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver", "Muressan", "bergsjo", "bygdin", "elgsjoen", "fundin", "kaldfjorden", "leirvatnet", "marsjoen", "nedreHeimdalsvatn", "ovreHeimdalsvatnet", "rodungen", "tyin", "vinstern", "aursjo"))

ref_clean = ref %>% 
  mutate(
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1),
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1),
    sdoy = date - as.Date(paste0(water_year, "-08-01"))) %>%
  # water_year = if_else(month(date) >= 10, year(date), year(date) - 1),
  # water_year = if_else(month(date) >= 10, year(date), year(date) - 1),
  # sdoy = date - as.Date(paste0(water_year, "-10-01"))) %>%
  rename(oldname = lakename) %>% 
  right_join(tibble(
    oldname = c("albion", "castle", "lunz", "morskie_oko", "silver", "muressan", "bergsjo", "bygdin", "elgsjoen", "fundin", "kaldfjorden", "leirvatnet", "marsjoen", "nedreHeimdalsvatn", "ovreHeimdalsvatnet", "rodungen", "tyin", "vinstern", "aursjo"),
    lakename = c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver", "Muressan", "bergsjo", "bygdin", "elgsjoen", "fundin", "kaldfjorden", "leirvatnet", "marsjoen", "nedreHeimdalsvatn", "ovreHeimdalsvatnet", "rodungen", "tyin", "vinstern", "aursjo")
  ), by = "oldname")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

require(gganimate)

t1 = 2016
t2 = 2016

test = rs_clean %>% 
  filter(
    water_year >= t1,
    water_year <= t2) %>% 
  filter(lakename %in% c("Castle")) %>% 
  mutate(sdoy = date - as.Date(paste0(water_year, "-08-01")),
         #sdoy = date - as.Date(paste0(water_year, "-10-01")),
         ice_cover = as.integer(iceModis >= 0.8)) %>% 
  ggplot(aes(x = sdoy, y = iceModis, group = water_year)) +
  #binomial_smooth(formula = y ~ splines::ns(x, 2), aes(color = "fitted"), se = T, alpha = 0.3) +
  geom_vline(data = ref_clean %>% filter(water_year >= t1, water_year <= t2, lakename == "Castle"), aes(xintercept = sdoy, color = event), lwd = 1) +
  geom_point(alpha = 0.5, pch = 8) +
  # scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent_format()) +
  #facet_grid(lakename~water_year) +
  #facet_grid(~water_year)+
  labs(
    x = "Number of days since 08/01",
    y = "Lake ice coverage",
    color = ""
  ) + 
  ggtitle('Castle Lake Ice Fraction')+
  theme_bw()+
  theme(
    text = element_text(size = 30),
    legend.position = 'bottom'
  )

test


# 3. Extract ice phenology ------------------------------------------------

calc_crossings = function(y, h = 0.5) {
  n = length(y)
  y_shifted = y - h
  crossings = which(y_shifted[1:(n - 1)] * y_shifted[2:n] < 0)
  
  if (length(crossings) == 0) {
    x_index = c(NA, NA)
    trend = NA
  } else {
    x_index = crossings
    trend = diff(y)[crossings]
  }
  
  return(list(x_index, trend))
}

pdates = rs_clean %>% 
  #filter(lakename %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver")) %>% 
  # filter(water_year > 1999, water_year < 2019) %>% 
  mutate(sdoy = as.integer(date - as.Date(paste0(water_year, "-08-01")))) %>%
  #mutate(sdoy = as.integer(date - as.Date(paste0(water_year, "-10-01")))) %>%
  group_by(lakename, water_year) %>% 
  do({
    dat = .
    fit = glm(iceModis~splines::ns(sdoy, df = 2), family = "binomial", data = dat)
    sdoy_range = c(min(dat$sdoy): max(dat$sdoy))
    pred = predict(fit, newdata = tibble(sdoy = sdoy_range), type = "response", se.fit = T)
    
    temp = calc_crossings(pred$fit, h = 0.5)
    crossings_index = temp[[1]]
    trend = temp[[2]] >= 0
    # crossings_plus_index = calc_crossings(pred$fit + pred$se.fit, h = 0.5)
    # crossings_minus_index = calc_crossings(pred$fit - pred$se.fit, h = 0.5)
    
    tibble(sdoy_fit = sdoy_range[crossings_index], trend = trend)
    
  }) %>% ungroup() %>% 
  distinct() %>% 
  mutate(event = factor(trend, levels = c(T, F), labels = c("ice_on", "ice_off")))

pdates %>% 
  filter(lakename %in% c('Albion', 'Morskie_Oko')) %>% 
  ggplot +
  geom_point(aes(x = water_year, y = sdoy_fit), color = "black", size = 5) +
  geom_point(data = ref_clean %>% filter(lakename %in% c('Albion', 'Morskie_Oko')), aes(x = water_year, y = sdoy, color = event), size = 5) +
  facet_wrap(~lakename) +
  theme_bw()+
  ylab('Day of Year')+
  xlab('Water Year')+
  theme(
    text = element_text(size = 25)
  )


merged_pdates = pdates %>% 
  inner_join(ref_clean, by = c("lakename", "water_year", "event")) %>% 
  mutate(date_modis = as.Date(paste0(water_year, "-08-01")) + sdoy_fit)
#mutate(date_modis = as.Date(paste0(water_year, "-10-01")) + sdoy_fit)

#write_csv(merged_pdates, here('data/combined/merged_pdates.csv'))


# 4. Get error metrics ----------------------------------------------------

merged_pdates %>% 
  mutate(dif = sdoy_fit - as.numeric(sdoy)) %>%
  group_by(lakename, event) %>% 
  summarise(
    MBS = mean(dif),
    MAE = mean(abs(dif)),
    RMSE = sqrt(mean(dif^2)),
    N = n()) %>% 
  ungroup() %>% 
  arrange(event, MAE)

event.labs <-  c('Ice On', 'Ice Off')
names(event.labs) <- c('ice_on', 'ice_off')

merged_pdates %>% 
  ggplot(aes(x = sdoy_fit, y = sdoy)) + #, pch = lakename
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, color = "1:1 line")) +
  #facet_grid(lakename~event, scales = "free") +
  facet_grid(~event, scales = "free", labeller = labeller(event = event.labs)) +
  theme_bw() +
  labs(x = "Days since 8/1 from MODIS",
       y = "Days since 8/1 from in situ",
       color = "Lake name" #,
  ) + #pch = "Lake name"
  theme(
    text = element_text(size = 16),
    legend.position = 'none',
    aspect.ratio = 1
  )


validation = merged_pdates %>% 
  ggplot(aes(x = sdoy_fit, y = sdoy, color = lakename)) + #pch = lakename, 
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0), color = "black") +
  facet_wrap(~event, scales = "free") +
  theme_classic() +
  labs(x = "Days since 8/1 from MODIS",
       y = "Days since 8/1 from in situ",
       color = "Lake name",
       pch = "Lake name")

