## The goal of this script is to:
##
## 1) Get summary of cloudy images,
## 2) Merge ice on and ice off validation data from Mountain Lake ice validation
##    with topographic roughness estimation from GEE script ______ and cloudy
##    images,
## 3) Visualize and get estimates for linear regression values from ice on and
##    ice off data (MAE) against various error-causing variables (e.g. clouds),
## 4) Multiple linear regression for all error causing variables against
##    ice on and off validation values (MAE). 
##

# load libraries----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vip)
library(lubridate)
library(here)
library(modelr)
library(lme4)
library(ggpubr)
library(cowplot)
library(randomForest)
library(rpart)
library(rpart.plot)

# 1. Get cloudy summary data----------------------------------------------------

# Import remotely sensed data from GEE script _________________

# rs <- read_csv(here('data/remote/aqua_terra_merged_clean_outlier_removed.csv'))%>%
#   rename(iceModis = full_merge) %>%
#   mutate(
#     season = 
#       ifelse(month(date) >= 12 | month(date) < 3, 'winter', 
#              ifelse(month(date) >= 3 & month(date) < 6, 'spring',
#                     ifelse(month(date) >= 6 & month(date) < 9, 'summer', 'fall'))),
#     water_year = if_else(month(date) >= 8, year(date), year(date) - 1)
#   )

rs = read_csv(here("data/combined/modis_allLakes_output_Terra_Aqua_merged_raw.csv")) #raw MODIS data without Norwegian lakes
nor_lakes <- read_csv(here('data/combined/modis_norwegian_lakes.csv'))

rs = rs %>%
  bind_rows(nor_lakes)

rs = rs %>%
  mutate(date = as.Date(date),
         lakename = as.factor(lakename),
         source = as.factor(source))

rs = rs %>% 
  mutate(date = as.Date(date),
         lakename = as.factor(lakename))

# rs %>%
#   filter(date >= "2014-01-01",
#          !is.na(iceModis),
#          cloudModis <= 0.1) %>%
#   ggplot(aes(x = date, y = iceModis)) +
#   geom_line(alpha = 0.2) +
#   geom_point(aes(color = cloudModis), size = 0.2) +
#   scale_color_viridis_c() +
#   facet_wrap(~lakename)

rs_clean = rs %>% filter(!is.na(iceModis))

rs_clean = rs_clean %>% 
  filter(cloudModis <= 0.1) %>%
  mutate(
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1),
    season =
            ifelse(month(date) >= 12 | month(date) < 3, 'winter',
                   ifelse(month(date) >= 3 & month(date) < 6, 'spring',
                          ifelse(month(date) >= 6 & month(date) < 9, 'summer', 'fall')))) %>%
  filter(lakename %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver", 
                         "Muressan", "bergsjo", "bygdin", "elgsjoen", "fundin", 
                         "kaldfjorden", "leirvatnet", "marsjoen", "nedreHeimdalsvatn", 
                         "ovreHeimdalsvatnet", "rodungen", "tyin", "vinstern", "aursjo"))

# Import in situ reference (ref) data from script ________

ref = read_csv(here("data/combined/all_insitu_water_year_1.csv")) 

## format date correctly for in situ data
ref = ref %>% 
  mutate(
    ice_on_insitu = as.Date(ice_on_insitu, format = "%m/%d/%Y"),
    ice_off_insitu = as.Date(ice_off_insitu, format = "%m/%d/%Y")) %>% 
  select(lakename, ice_on_insitu, ice_off_insitu, latitude, longitude, area, elevation) %>% 
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

ref_clean = ref %>% 
  mutate(
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1),
    #water_year = if_else(month(date) >= 8, year(date), year(date) - 1),
    sdoy = date - as.Date(paste0(water_year, "-08-01"))) %>%
  # water_year = if_else(month(date) >= 10, year(date), year(date) - 1),
  # water_year = if_else(month(date) >= 10, year(date), year(date) - 1),
  # sdoy = date - as.Date(paste0(water_year, "-10-01"))) %>%
  rename(oldname = lakename) %>% 
  right_join(tibble(
    oldname = c("albion", "castle", "lunz", "morskie_oko", "silver", "muressan", "bergsjo", "bygdin", "elgsjoen", "fundin", "kaldfjorden", "leirvatnet", "marsjoen", "nedreHeimdalsvatn", "ovreHeimdalsvatnet", "rodungen", "tyin", "vinstern", "aursjo"),
    lakename = c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver", "Muressan", "bergsjo", "bygdin", "elgsjoen", "fundin", "kaldfjorden", "leirvatnet", "marsjoen", "nedreHeimdalsvatn", "ovreHeimdalsvatnet", "rodungen", "tyin", "vinstern", "aursjo")
  ), by = "oldname") %>% 
  mutate(
    lakename = tolower(lakename)
  ) %>% 
  select(-c(sdoy, date, oldname))


# Total number of cloudy and noncloudy images by water year
# summary_cloudy_images <- rs_clean %>%
#   mutate(
#     lakename = tolower(lakename)
#   ) %>% 
#   group_by(lakename, water_year) %>%
#   summarise(
#     total_yearly_cloudy_images = 365 - n(),
#     total_yearly_noncloudy_images = n()
#   ) %>%
#   ungroup() 

# Total number of cloudy images by season
summary_seasonal_cloudy_images <- rs_clean %>% 
  mutate(
    lakename = tolower(lakename)
  ) %>% 
  group_by(lakename, water_year, season) %>% 
  summarise(
    seasonal_noncloudy_images = n()
  )

#mean cloudy images per lake
# mean_cloudy_per_lake <- summary_cloudy_images %>% 
#   group_by(lakename) %>% 
#   summarise(
#     mean_cloudiness = mean(total_yearly_cloudy_images),
#     mean_images = mean(total_yearly_noncloudy_images)
#   ) %>% 
#   arrange(mean_cloudiness)

# 2. Merge ice validation with topographic roughness----------------------------

# Valication data from script 'Mountain Lake ice validaton_August.R
error_pdates <- read_csv(here('data/combined/error_pdates_2022.04.18.csv')) %>%
  mutate(
    lakename = tolower(lakename)
  )

# Topographic roughness data from GEE script _______________
topo_roughness <- read_csv(here('data/combined/lake_topo_roughness.csv')) %>%
  mutate(
    lakename = tolower(lakename)
  )

# Merge validation and topo_roughness data frames
data_for_regression <- ref_clean %>%
  full_join(topo_roughness, by = c('lakename')) %>%
  #full_join(summary_cloudy_images, by = c('lakename', 'water_year')) %>% 
  full_join(summary_seasonal_cloudy_images, by = c('lakename', 'water_year')) %>% 
  full_join(error_pdates)

# 3. Visualize relationships between error and ind. variables-------------------

# Error ~ area

ggplot(na.omit(data_for_regression))+
  geom_point(aes(x = area, y = MAE))+
  geom_smooth(aes(x = area, y = MAE), method = 'lm', se = F)+
  theme_classic()+
  facet_wrap(~event)

ggdensity(data_for_regression$area, fill = 'lightgray') #not normal
ggqqplot(data_for_regression$area) #non normal

mae_by_area = lm(data = data_for_regression, formula = MAE~area)

#plot(mae_by_area) #residuals vs fitted have a pattern. regression may not be appropriate

summary(mae_by_area)

# Error ~ elevation

ggplot(na.omit(data_for_regression))+
  geom_point(aes(x = elevation, y = MAE))+
  geom_smooth(aes(x = elevation, y = MAE), method = 'lm', se = F)+
  theme_classic()+
  facet_wrap(~event)

ggdensity(data_for_regression$elevation, fill = 'lightgray') #not normal

mae_by_elevation = lm(data = data_for_regression, formula = MAE~elevation)

#plot(mae_by_elevation)

summary(mae_by_elevation)

# Error ~ latitude

ggplot(na.omit(data_for_regression))+
  geom_point(aes(x = latitude, y = MAE))+
  geom_smooth(aes(x = latitude, y = MAE), method = 'lm', se = F)+
  theme_classic()+
  facet_wrap(~event)

ggdensity(data_for_regression$latitude, fill = 'lightgray') #not normal

mae_by_latitude = lm(data = data_for_regression, formula = MAE~latitude)

#plot(mae_by_latitude)

summary(mae_by_latitude)

# Error ~ longitude

ggplot(na.omit(data_for_regression))+
  geom_point(aes(x = longitude, y = MAE))+
  geom_smooth(aes(x = longitude, y = MAE), method = 'lm', se = F)+
  theme_classic()+
  facet_wrap(~event)

ggdensity(data_for_regression$longitude, fill = 'lightgray') #not normal

mae_by_longitude = lm(data = data_for_regression, formula = MAE~longitude)

#plot(mae_by_longitude)

summary(mae_by_longitude)

#Error ~ topographic roughness

ggplot(na.omit(data_for_regression))+
  geom_point(aes(x = maxRoughness, y = MAE))+
  geom_smooth(aes(x = maxRoughness, y = MAE), method = 'lm', se = F)+
  theme_classic()+
  facet_wrap(~event)

ggdensity(data_for_regression$maxRoughness, fill = 'lightgray') #not normal

mae_by_maxRoughness = lm(data = data_for_regression, formula = MAE~maxRoughness)

#plot(mae_by_maxRoughness)

summary(mae_by_maxRoughness) # r^2 = 0.42

# Error ~ cloudy days

# ggplot(na.omit(data_for_regression))+
#   geom_point(aes(x = total_yearly_cloudy_images, y = MAE))+
#   geom_smooth(aes(x = total_yearly_cloudy_images, y = MAE), method = 'lm', se = F)+
#   theme_classic()+
#   facet_wrap(~event)
# 
# mae_by_total_yearly_cloudy_images = lm(data = data_for_regression, formula = MAE~total_yearly_cloudy_images)
# 
# summary(mae_by_total_yearly_cloudy_images)

# Error ~ noncloudy days

# ggplot(na.omit(data_for_regression))+
#   geom_point(aes(x = total_yearly_noncloudy_images, y = MAE))+
#   geom_smooth(aes(x = total_yearly_noncloudy_images, y = MAE), method = 'lm', se = F)+
#   theme_classic()+
#   facet_wrap(~event)
# 
# mae_by_total_yearly_noncloudy_images = lm(data = data_for_regression, formula = MAE~total_yearly_noncloudy_images)
# 
# summary(mae_by_total_yearly_noncloudy_images)

# Error ~ seasonal noncloudy images

ggplot(na.omit(data_for_regression))+
  geom_point(aes(x = seasonal_noncloudy_images, y = MAE))+
  geom_smooth(aes(x = seasonal_noncloudy_images, y = MAE), method = 'lm', se = F)+
  theme_classic()+
  facet_wrap(~event+season)

mae_by_seasonal_noncloudy_images = lm(data = data_for_regression, formula = MAE~seasonal_noncloudy_images)

summary(mae_by_seasonal_noncloudy_images)

# 4. Multiple linear regression-------------------------------------------------

mlr_pdates_error = lm(data = data_for_regression, formula = MAE~area+elevation+latitude+longitude+maxRoughness+total_yearly_cloudy_images)
summary(mlr_pdates_error)

#plot(mlr_pdates_error)

# 5. Random forest model--------------------------------------------------------

set.seed(42)

data_for_regression_ice_on = data_for_regression %>%
  na.omit() %>%
  filter(event == "ice_on") %>% 
  select(
    lakename,
    MAE, 
    latitude, 
    longitude, 
    area, 
    elevation, 
    maxRoughness, 
    #total_yearly_cloudy_images, 
    #total_yearly_noncloudy_images, 
    season, 
    seasonal_noncloudy_images
    ) %>% 
  filter(season == 'winter') %>%
  mutate(
    suitability = ifelse(
      MAE <= 14, 0, 1
    ),
    suitability = ifelse(
      suitability == 0, yes = 'suitable', no = 'unsuitable'
    ),
    suitability = as.factor(suitability)
  )  
  #select(-lakename, -longitude, -suitability)
  #select(-season, -seasonal_noncloudy_images)

# %>%
#   group_by(lakename) %>%
#   sample_n(size = 1) %>%
#   ungroup() %>% 
#   select( -lakename)

str(data_for_regression_ice_on)

#data.imputed <- rfImpute(MAE~., data = na.omit(data_for_regression), iter = 6) #this line estmiates values for any NA values

model_ice_on <- randomForest(MAE~., data = data_for_regression_ice_on, proximity = TRUE, ntree = 1000)

model_ice_on

which.min(model_ice_on$mse) #find number of trees that produce lowest test MSE

sqrt(model_ice_on$mse[which.min(model_ice_on$mse)])

plot(model_ice_on)

varImpPlot(model_ice_on)

#with rpart
model_on <- rpart(suitability ~ ., data = data_for_regression_ice_on)
model_on
rpart.plot(model_on, type = 3, digits = 3, fallen.leaves = TRUE)

#logistic regression model for ice on

## GLM model--------------------------------------------------------------------

# logistic_model_ice_on <- glm(data = data_for_regression_ice_on, formula = suitability ~ maxRoughness + total_yearly_cloudy_images, family = 'binomial') #+ total_yearly_cloudy_images
# summary(logistic_model_ice_on)
logistic_model_ice_on <- glm(data = data_for_regression_ice_on, formula = suitability ~ maxRoughness + seasonal_noncloudy_images, family = 'binomial') #+ total_yearly_cloudy_images
summary(logistic_model_ice_on)

#This may give us the suitability
#predict(model)

ll.null <- logistic_model_ice_on$null.deviance/-2
ll.proposed <- logistic_model_ice_on$deviance/-2

(ll.null - ll.proposed) / ll.null #gives a pseudo r^2 value

1-pchisq(2*(ll.proposed - ll.null), df=(length(logistic_model_ice_on$coefficients)-1)) #gives p value for the pseudo r^2
#with winter noncloudy days, gives a r^2 value of 0.66 (improvement from 0.44 with total cloudy days)

#visualize logistic regression
predicted.data <- data.frame(
  probability.of.suitability=logistic_model_ice_on$fitted.values,
  suitability=data_for_regression_ice_on$suitability)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.suitability, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.suitability)) +
  geom_point(aes(color=suitability), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of suitability")+
  ggtitle('Logistic regression: Suitability ~ Topographic Roughness + Winter Non-Cloudy Days')

#ggsave(here('output/logistic_regression_results/suitability_v_winter_noncloudy_days.png'), dpi = 300)

#Use logistic regressino model to predict ice on suitability--------------------

# prediction_data_ice_on <- data_for_regression_ice_on %>% 
#   select(lakename, maxRoughness, MAE, suitability) %>%
#   group_by(lakename) %>% 
#   slice(1)
# 
# prediction_class_test <- predict(logistic_model_ice_on, newdata = prediction_data_ice_on, type = "class")
# 
# summary(prediction)

#tidymodel version of logistic regression for ice on----------------------------

set.seed(42)
splits <- data_for_regression_ice_on %>% 
  initial_split(prop = 0.80)

splits

model_fit_glm_ice_on <- logistic_reg() %>% 
  set_engine("glm") %>% 
  #fit(suitability ~ area+maxRoughness+total_yearly_cloudy_images, data = training(splits))
  fit(suitability ~ maxRoughness+seasonal_noncloudy_images, data = training(splits))

model_fit_glm_ice_on

prediction_class_test_ice_on <- predict(model_fit_glm_ice_on, new_data = testing(splits), type = 'class')

prediction_prob_test_ice_on <- predict(model_fit_glm_ice_on, new_data = testing(splits), type = 'prob')


results_tbl_ice_on <- bind_cols(
  prediction_class_test,
  prediction_prob_test,
  testing(splits)
)

results_tbl_ice_on %>% 
  roc_auc(suitability, .pred_suitable)

results_tbl_ice_on %>% 
  roc_curve(suitability, .pred_suitable) %>% 
  autoplot(
    options = list(
      smooth = TRUE
    )
  ) 

model_fit_glm_ice_on$fit %>% 
  vip(
    num_features = 5,
    geom = 'point',
    aesthetics = list(
      size = 4,
      color = '#18bc9c'
    )
  )+
  theme_minimal(base_size = 18)+
  labs(title = "Logistic Regression: Feature Importance Ice On")

# Random forest for ice off-----------------------------------------------------

set.seed(42)

data_for_regression_ice_off = data_for_regression %>%
  na.omit() %>%
  filter(event == "ice_off") %>% 
  select(
    lakename,
    MAE, 
    latitude, 
    longitude, 
    area, 
    elevation, 
    maxRoughness, 
    #total_yearly_cloudy_images, 
    #total_yearly_noncloudy_images, 
    season, 
    seasonal_noncloudy_images
  ) %>% 
  filter(season == 'spring') %>% 
  mutate(
    suitability = ifelse(
      MAE <= 10, 0, 1
    ),
    suitability = ifelse(
      suitability == 0, yes = 'suitable', no = 'unsuitable'
    ),
    suitability = as.factor(suitability)
  ) #%>% 
  #select(-lakename, -longitude, -suitability)
  #select(-lakename, -longitude, -MAE)


## GLM model
#logistic_model_ice_off <- glm(formila = suitability ~ maxRoughness+cloudiness+...., family = binomial)
#This may give us the suitability
#predict(model)


model_ice_off <- randomForest(MAE~., data = data_for_regression_ice_off, proximity = TRUE, ntree = 1000)

model_ice_off

which.min(model_ice_off$mse) #find number of trees that produce lowest test MSE

sqrt(model_ice_off$mse[which.min(model_ice_off$mse)])

plot(model_ice_off)

varImpPlot(model_ice_off)

#with rpart
model_off <- rpart(suitability ~ ., data = data_for_regression_ice_off)
model_off
rpart.plot(model_off, type = 3, digits = 3, fallen.leaves = TRUE)

## GLM model--------------------------------------------------------------------

# logistic_model_ice_off <- glm(data = data_for_regression_ice_off, formula = suitability ~ maxRoughness + total_yearly_cloudy_images, family = 'binomial') #+ total_yearly_cloudy_images
# summary(logistic_model_ice_on)
logistic_model_ice_off <- glm(data = data_for_regression_ice_off, formula = suitability ~ maxRoughness + seasonal_noncloudy_images, family = 'binomial') #+ total_yearly_cloudy_images
summary(logistic_model_ice_on)

#This may give us the suitability
#predict(model)

ll.null_off <- logistic_model_ice_off$null.deviance/-2
ll.proposed_off <- logistic_model_ice_off$deviance/-2

(ll.null_off - ll.proposed_off) / ll.null_off #gives a pseudo r^2 value = 0.42

1-pchisq(2*(ll.proposed_off - ll.null_off), df=(length(logistic_model_ice_off$coefficients)-1)) #gives p value for the pseudo r^2 = 0, so significant

#visualize logistic regression
predicted.data_off <- data.frame(
  probability.of.suitability=logistic_model_ice_off$fitted.values,
  suitability=data_for_regression_ice_off$suitability)

predicted.data_off <- predicted.data_off[
  order(predicted.data_off$probability.of.suitability, decreasing=FALSE),]
predicted.data_off$rank <- 1:nrow(predicted.data_off)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data_off, aes(x=rank, y=probability.of.suitability)) +
  geom_point(aes(color=suitability), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of suitability")+
  ggtitle('Logistic regression Ice Off: Suitability ~ Topographic Roughness + Total Cloudy Days')

ggsave(here('output/logistic_regression_results/suitability_v_roughness_cloudy_days_ice_off.png'), dpi = 300)

#tidymodel version of logistic regression for ice off----------------------------

set.seed(42)
splits_off <- data_for_regression_ice_off %>% 
  initial_split(prop = 0.80)

splits_off

model_fit_glm_ice_off <- logistic_reg() %>% 
  set_engine("glm") %>% 
  #fit(suitability ~ area+elevation+maxRoughness+total_yearly_cloudy_images, data = training(splits_off))
  fit(suitability ~ maxRoughness+seasonal_noncloudy_images, data = training(splits_off))

model_fit_glm_ice_off

prediction_class_test_ice_off <- predict(model_fit_glm_ice_off, new_data = testing(splits_off), type = 'class')

prediction_prob_test_ice_off <- predict(model_fit_glm_ice_off, new_data = testing(splits_off), type = 'prob')


results_tbl_ice_off <- bind_cols(
  prediction_class_test,
  prediction_prob_test,
  testing(splits_off)
)

results_tbl_ice_off %>% 
  roc_auc(suitability, .pred_suitable)

results_tbl_ice_off %>% 
  roc_curve(suitability, .pred_suitable) %>% 
  autoplot(
    options = list(
      smooth = TRUE
    )
  ) 

model_fit_glm$fit %>% 
  vip(
    num_features = 5,
    geom = 'point',
    aesthetics = list(
      size = 4,
      color = '#18bc9c'
    )
  )+
  theme_minimal(base_size = 18)+
  labs(title = "Logistic Regression: Feature Importance Ice Off")
