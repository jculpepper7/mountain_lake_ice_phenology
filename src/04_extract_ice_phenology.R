require(tidyverse)
require(lubridate)
require(here)



#problem with Sierra lakes tiles 3, 7: 'Error in qr.default(t(const)): NA/NaN/Inf in foreign function call (arg 1)'


# 1. Import data ----------------------------------------------------------

#You can do this for a single. "tile" at a time or for all files within a folder in the "path"

#All files within folder:
# file_path <- list.files(path = here('data/remote/north_american_data_by_tile/sierra_test'))
# 
# rs_gee <- do.call(bind_rows, lapply(file_path, function(x) read_csv(file = x)))

#One file at a time:
rs_gee = read_csv(here("data/remote/north_american_data_by_tile/sierra_test/modis_SierraLakes_output_Terra_Aqua_merged_tile_3.csv"))

#head(rs_gee)

## remote sensing data

# 2. Clean remote sensing data --------------------------------------------

rs_gee = rs_gee %>%
  mutate(date = as.Date(date),
         Hylak_id = as.factor(Hylak_id),
         source = as.factor(source))


rs_gee_clean = rs_gee %>% 
  filter(!is.na(iceModis),
         !is.nan(iceModis),
         !is.infinite(iceModis))

rs_gee_clean = rs_gee_clean %>% 
  filter(cloudModis <= 0.1) %>% 
  mutate(
    water_year = if_else(month(date) >= 8, year(date), year(date) - 1)
  ) 


# 3. Binomial function ----------------------------------------------------

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

## extract ice phenology

# 4. Extract Ice phenology ------------------------------------------------

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

pdates = rs_gee_clean %>% 
  mutate(sdoy = as.integer(date - as.Date(paste0(water_year, "-08-01")))) %>%
  group_by(Hylak_id, water_year) %>% 
  do({
    dat = .
    fit = glm(iceModis~splines::ns(sdoy, df = 2), family = "binomial", data = dat)
    sdoy_range = c(min(dat$sdoy): max(dat$sdoy))
    pred = predict(fit, newdata = tibble(sdoy = sdoy_range), type = "response", se.fit = T)
    
    temp = calc_crossings(pred$fit, h = 0.5)
    crossings_index = temp[[1]]
    trend = temp[[2]] >= 0
    
    tibble(sdoy_fit = sdoy_range[crossings_index], trend = trend)
    
  }) %>% ungroup() %>% 
  distinct() %>% 
  mutate(event = factor(trend, levels = c(T, F), labels = c("ice_on", "ice_off")))

#unique(pdates$Hylak_id)

# 5. Write combined CSV ---------------------------------------------------

#This step can also be done 

#write_csv(pdates, here('data/n_american_ice_phenology/sierra_lakes_tile_3.csv'))

