## The objective of this script is to:
## 1. Read in the raw data of in situ validated ice phenology. 
##    For information on the acquisition of ice phenology data, see script 00_get_ice_data.R
## 2. Make column headers uniform
## 3. Bind separate data frames into a single data frame for further analysis. 
## 4. Add water year for ice off and ice on
## 5. Write a new csv file with the aggregated data for further analysis in other scripts.


# Load packages --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# 1. Read in data ------------------------------------------------------

# For data download location and data citations see script 00_get_ice_data

albion <- read_csv(here("data/in_situ/albion_in_situ.csv"))
castle <- read_csv(here("data/in_situ/castle_in_situ.csv"))
lunz <- read_csv(here("data/in_situ/lunz_in_situ.csv"))
morskie_oko <- read_csv(here("data/in_situ/morskie_oko_in_situ.csv"))
silver <- read_csv(here("data/in_situ/silver_in_situ.csv"))
san_murezzan <- read_csv(here("data/in_situ/murezzan_in_situ.csv"))
norwegian_lakes <- read_csv(here("data/in_situ/Data_for_publication-Dryad.csv"))

# 2. Make data frames uniform ------------------------------------------

# Clean Albion data 

albion_insitu <- albion %>%
  clean_names() %>%
  mutate(ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu),
         # latitude = ,
         # longitude = ,
         # elevation = ,
         # 
         ) %>%
  select(-ice_on, -ice_off) #remove columns with incorrect formatting

#Take a look
head(albion_insitu)

# ----------------------------------------------------------------------

# Clean Castle data

castle_insitu <- castle %>%
  clean_names() %>%
  rename(ice_off_insitu_yday = ice_off,
         ice_on_insitu_yday = ice_on) %>%
  mutate(ice_off_insitu = make_date(year) + ice_off_insitu_yday - 1, #add the actual date rather than just the day of year
         ice_on_insitu = make_date(year) + ice_on_insitu_yday - 1,
         lake = c("castle"),
         # elevation = 1660, #meters
         # area = 0.2
         ) %>%  #add the ordinal day for ice off
  select(-name) %>% #remove unnecessary column
  select(lake, year, ice_on_insitu, ice_off_insitu, ice_on_insitu_yday, ice_off_insitu_yday) %>% #reorder columns to be uniform with other dataframes
  filter(year >= 2000) #filter out rows that cannot be used in analysis (MODIS only dates back to 2000)
  
#Take a look
head(castle_insitu)

#issue with ice on value for 1/6/2018. Should be 1/6/2019. 
#will change it manually
castle_insitu[19,3] = as.Date('2019-01-06')

# ----------------------------------------------------------------------

# Clean Lunz data

lunz_insitu <- lunz %>%
  clean_names() %>%
  mutate(lake = c("lunz"),
         ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off, -days) %>% #remove columns with incorrect formatting
  select(lake, year, ice_on_insitu, ice_off_insitu, ice_on_insitu_yday, ice_off_insitu_yday) #reorder columns to be uniform with other dataframes

#Take a look
head(lunz_insitu)

# ----------------------------------------------------------------------

# Clean Morskie Oko data

morskie_oko_insitu <- morskie_oko %>%
  clean_names() %>%
  mutate(ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off) #remove columns with incorrect formatting

#Take a look
head(morskie_oko_insitu)

# ----------------------------------------------------------------------

# Clean Silver data

silver_insitu <- silver %>%
  clean_names() %>%
  mutate(ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off) #remove columns with incorrect formatting

#Take a look
head(silver_insitu)

# ----------------------------------------------------------------------

# Clean Murezzan data

#replace -999 with NA
san_murezzan[san_murezzan == -999] <- NA

san_murezzan_insitu <- san_murezzan %>%
  clean_names() %>%
  filter(iceoff_year >= 2000) %>%
  arrange(iceoff_year) %>%
  mutate(
    lakename = 'san_murezzan',
    ice_on_insitu = make_date(year = iceon_year, month = iceon_month, day = iceon_day),
    ice_off_insitu = make_date(year = iceoff_year, month = iceoff_month, day = iceoff_day),
    ice_on_insitu_yday = yday(ice_on_insitu),
    ice_off_insitu_yday = yday(ice_off_insitu),
    year = season + 1
  ) %>%
  select(lake = lakename, year, ice_on_insitu, ice_off_insitu, ice_on_insitu_yday, ice_off_insitu_yday)

# ----------------------------------------------------------------------

# Clean data from Norwegian lakes 

nor_lakes <- norwegian_lakes %>%
  clean_names() %>%
  select(
    lake, 
    elevation = altitude_m_asl, 
    area_km2, 
    impounded, 
    year, 
    ice_off_insitu_yday = break_up, 
    ice_on_insitu_yday = freeze_up
  ) %>%
  filter(elevation >= 1000 & area_km2 >= 0.1 & year >= 2000) %>%
  mutate(
    ice_off_insitu = parse_date_time(paste(year, ice_off_insitu_yday), orders = 'yj'), #This line (and next) takes the ordinal day and converts it to the date
    ice_on_insitu = parse_date_time(paste(year, ice_on_insitu_yday), orders = 'yj') #see solution from 'rrr' here: https://stackoverflow.com/questions/41030937/is-there-an-inverse-of-the-yday-lubridate-function
  ) %>%
  select(lake, year, ice_on_insitu, ice_off_insitu, ice_on_insitu_yday, ice_off_insitu_yday)




# 3. Bind separate data frames ------------------------------------------

all_insitu_ice_data <- bind_rows(albion_insitu, castle_insitu, lunz_insitu, morskie_oko_insitu, silver_insitu, san_murezzan_insitu, nor_lakes)

# 4. Add water year to both the ice on and ice off dates

all_insitu_w_water_year <- all_insitu_ice_data %>%
  rename(lakename = lake) %>%
  mutate(ice_on_month = month(ice_on_insitu),
         ice_off_month = month(ice_off_insitu),
         ice_on_year = year(ice_on_insitu),
         ice_off_year = year(ice_off_insitu)) %>%
  mutate(ice_on_water_year = ifelse(ice_on_month >= 10, ice_on_year+1, ice_on_year),
         ice_off_water_year = ifelse(ice_off_month >= 10, ice_off_year+1, ice_off_year)) %>%
  select(-ice_on_month, -ice_off_month, -year, -ice_on_year, -ice_off_year)

# 5. Write CSV with new data --------------------------------------------

write_csv(all_insitu_ice_data, here("data/combined/all_insitu_ice_data.csv"))
write_csv(all_insitu_w_water_year, here("data/combined/all_insitu_water_year.csv"))
