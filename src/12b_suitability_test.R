## The goal of this script is to:
##
## 1. Import, clean, and obtain seasonal non-cloudy images for N. American lakes
## 2. Import and join topographic roughness data
## 3. Use suitability model from script 12_error_validation.R to assess N. American remotely sensed suitability
## 4. Export that dataset to be used in the N. American lake visualization.
##
##

#1. Import remotely sensed data with cloud fraction-----------------------------

df <- list.files(path = here('data/remote/north_american_data_by_tile'), full.names = TRUE) %>% 
  map_dfr(read_csv) %>% 
  select(-h, -iceModis)

head(df)

# Clean data and add seasonal variable
df_clean <- df %>% 
  filter(cloudModis <= 0.1) %>% 
  mutate(date = as.Date(date),
         Hylak_id = as.factor(Hylak_id),
         source = as.factor(source)
         ) %>% 
  mutate(
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1),
    season =
      ifelse(month(date) >= 12 | month(date) < 3, 'winter',
             ifelse(month(date) >= 3 & month(date) < 6, 'spring',
                    ifelse(month(date) >= 6 & month(date) < 9, 'summer', 'fall'))))

# Obtain number of non-cloudy images by season
rs_noncloudy_data <- df_clean %>% 
  group_by(Hylak_id, water_year, season) %>% 
  summarise(
    seasonal_noncloudy_images = n()
  ) %>% 
  filter(season == 'spring' | season == 'winter')

str(rs_noncloudy_data)
# Average number of cloud images per year

# avg_cloudy_images <- df_clean %>% 
#   group_by(Hylak_id, water_year) %>% 
#   summarise(
#     avg_cloud_days = ifelse(water_year %% 4 == 0, n()/366, n()/365)
#   ) %>% 
#   slice(1)

# The above code is just for a general assessment of the number of cloudy images
# per year per lake. It won't be used in the suitability assessment

# 2. Import and join topo roughness---------------------------------------------

topo_roughness_rs <- read_csv(here('data/n_american_ice_phenology/lake_topo_roughness.csv')) %>% #topo roughness for all remotely sensed lakes without in situ data
  mutate(
    Hylak_id = as.factor(Hylak_id),
    maxRoughness = as.numeric(maxRoughness)
  ) %>% 
  arrange(Hylak_id)

# Take a look

str(topo_roughness_rs)

# Combine non-cloudy and topo roughness data

rs_test_data <- rs_noncloudy_data %>% 
  inner_join(topo_roughness_rs, by = 'Hylak_id') %>% 
  na.omit

# 3. Use prediction model on rs_test-data---------------------------------------

# Ice on model

rs_test_ice_on <- rs_test_data %>% 
  filter(season == 'winter') %>% 
  group_by(Hylak_id) %>% 
  summarise(
    seasonal_noncloudy_images = mean(seasonal_noncloudy_images),
    maxRoughness = mean(maxRoughness)
  )

# Logistic regression model from script 12

rs_prediction_class_test_ice_on <- predict(model_fit_glm_ice_on, new_data = rs_test_ice_on, type = 'class')

rs_prediction_prob_test_ice_on <- predict(model_fit_glm_ice_on, new_data = rs_test_ice_on, type = 'prob')

# Combine class and probability results with rs_test_ice_on
# This gives class and probability suitability to Hylak_id

rs_results_tbl_ice_on <- bind_cols(
  rs_prediction_class_test_ice_on,
  rs_prediction_prob_test_ice_on,
  rs_test_ice_on
)

# Write the csv into the N. America results section

#write_csv(rs_results_tbl_ice_on, here('data/n_american_ice_phenology/rs_results_tbl_ice_on.csv'))


# Ice off model

rs_test_ice_off <- rs_test_data %>% 
  filter(season == 'spring') %>% 
  group_by(Hylak_id) %>% 
  summarise(
    seasonal_noncloudy_images = mean(seasonal_noncloudy_images),
    maxRoughness = mean(maxRoughness)
  )

# Logistic regression model from script 12

rs_prediction_class_test_ice_off <- predict(model_fit_glm_ice_off, new_data = rs_test_ice_off, type = 'class')

rs_prediction_prob_test_ice_off <- predict(model_fit_glm_ice_off, new_data = rs_test_ice_off, type = 'prob')

# Combine class and probability results with rs_test_ice_on
# This gives class and probability suitability to Hylak_id

rs_results_tbl_ice_off <- bind_cols(
  rs_prediction_class_test_ice_off,
  rs_prediction_prob_test_ice_off,
  rs_test_ice_off
)

# Write the csv into the N. America results section

write_csv(rs_results_tbl_ice_off, here('data/n_american_ice_phenology/rs_results_tbl_ice_off.csv'))
