## The goal of this script is to: 
##
## 1. Import MODIS Aqua and Terra data
## 2. Merge Aqua and Terra
## 3. Create cleaned csv file for further analysis 

# Import libraries--------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(plotly)
library(zoo)
library(here)
library(janitor)
library(padr)
library(data.table)

# 1. Import data----------------------------------------------------------------

#Import all raw Google Earth Engine (GEE) files

# path = here('data/remote/sierra_lake_ice_data') #specify the path of the folder where the GEE files are located 
# 
# 
GEE_files_raw <- list.files(path, pattern = ".csv", full.names = TRUE) %>% #use list.files with the specified path, give the pattern (CSV), then include full.names = T, so that the file path will be read in addition to the file title.
  map_df(~fread(.))                                                        #found this code here: https://stackoverflow.com/a/40943207/11969269
                                                                           #and here: https://stackoverflow.com/a/58841390/11969269

# 2. Merge Aqua and Terra data--------------------------------------------------

# 2a. Separate Aqua and Terra data for merge------------------------------------

separate_aqua_terra <- GEE_files_raw %>%
  #improves column names from GEE output
  clean_names() %>%
  #Focusing on these lakes for now
  #filter(hylak_id %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver")) %>%
  filter(cloud_modis < 0.2) %>%
  #pivot wider and give h, cloud, and ice variables their own columns with pivot wider
  pivot_wider(names_from = source, names_sep = ".", values_from = c(ice_modis, cloud_modis, h)) 

# 2b. Prep Aqua and Terra for merge into a single column------------------------

aqua_terra_initial <- separate_aqua_terra %>%
  arrange(hylak_id, date) %>%
  #Add a difference column to detect sensor overlap (both sensors off an image on the same day)
  mutate(modis_diff_no_overlap = ice_modis.MODISTerra - ice_modis.MODISAqua) %>%
  #Filter to have only observations with NA in difference column, which will 
  #give a dataframe with only observations that don't overlap between Aqua and Terra
  filter(is.na(modis_diff_no_overlap)) %>%
  #Create a new column for merged Aqua and Terr observations (for non-overlapping observations)
  mutate(merged_remote_no_overlap = ifelse(is.na(ice_modis.MODISTerra), ice_modis.MODISAqua, ice_modis.MODISTerra))

# 2c. Amend overlapping data to merge with dataframe from step 2b---------------

#Evaluate Aqua data without NA values
check_aqua <- separate_aqua_terra %>%
  #select(-MODISTerra, -h) %>%
  select(-ice_modis.MODISTerra, -h.MODISTerra, -cloud_modis.MODISTerra) %>%
  na.omit() %>%
  group_by(hylak_id) %>%
  mutate(diff_ice_modis_aqua = c(ice_modis.MODISAqua[1], diff(ice_modis.MODISAqua)),
         diff_nonzero = ifelse(diff_ice_modis_aqua !=0, TRUE, FALSE),
         ice_modis_aqua_new = ifelse(diff_nonzero == TRUE & dplyr::lag(ice_modis.MODISAqua)==dplyr::lead(ice_modis.MODISAqua), lag(ice_modis.MODISAqua), ice_modis.MODISAqua)) %>%
  ungroup

double_check_aqua <- check_aqua %>%
  mutate(diff_dbl_chk_aqua = ice_modis_aqua_new - ice_modis.MODISAqua) %>%
  mutate(which_dates_diff_aqua = ifelse(diff_dbl_chk_aqua != 0, FALSE, TRUE)) %>%
  filter(which_dates_diff_aqua == FALSE)

#Evaluate Terra data without NA values
check_terra <- separate_aqua_terra %>%
  #select(-MODISTerra, -h) %>%
  select(-ice_modis.MODISAqua, -h.MODISAqua, -cloud_modis.MODISAqua) %>%
  na.omit() %>%
  group_by(hylak_id) %>%
  mutate(diff_ice_modis_terra = c(ice_modis.MODISTerra[1], diff(ice_modis.MODISTerra)),
         diff_nonzero = ifelse(diff_ice_modis_terra !=0, TRUE, FALSE),
         ice_modis_terra_new = ifelse(diff_nonzero == TRUE & dplyr::lag(ice_modis.MODISTerra)==dplyr::lead(ice_modis.MODISTerra), lag(ice_modis.MODISTerra), ice_modis.MODISTerra)) %>%
  ungroup

double_check_terra <- check_terra %>%
  mutate(diff_dbl_chk_terra = ice_modis_terra_new - ice_modis.MODISTerra) %>%
  mutate(which_dates_diff_terra = ifelse(diff_dbl_chk_terra != 0, FALSE, TRUE)) %>%
  filter(which_dates_diff_terra == FALSE)

#Merge Aqua and Terra data without NA values to detect overlapping data 
#(i.e reflectance values on the same day from the different sensors)
check_merge <- full_join(check_aqua, check_terra) %>%
  arrange(hylak_id, date) %>%
  #eliminate NA values
  na.omit() %>%
  #The diff of the sensor columns will tell us where the sensors disagree.
  mutate(modis_diff = ice_modis_aqua_new - ice_modis_terra_new) 

#Look at the basic stats to check for discrepancies when the two sensors have images on the same day
mean(check_merge$modis_diff) #0.002472321
max(check_merge$modis_diff) #1
median(check_merge$modis_diff) #0
min(check_merge$modis_diff) #-1
# These values indicate that generally, the sensors agree, but at times are opposed. 
# Proceed by eliminating large opposing values (>0.5 or <-0.5) and taking average values between smaller opposing values

#Get a dataframe with only equivalent sensor data
non_opposing_values <- check_merge %>%
  filter(modis_diff == 0) %>%
  #select(date, hylak_id, cloud_modis, modis_diff)
  select(date, hylak_id, ice_modis.MODISAqua, ice_modis.MODISTerra, modis_diff)

#Get a dataframe with only senor data that disagree, eliminate large disagreements, 
#and take the average of the remaining disagreements
opposing_values <- check_merge %>%
  filter(modis_diff != 0) %>% #eliminate non-opposing values #421 total observations
  filter(modis_diff < 0.5 & modis_diff > -0.5) %>% #eliminate values too
  mutate(modis_mean = (ice_modis.MODISAqua + ice_modis.MODISTerra)/2) %>%
  select(date, hylak_id, modis_mean)

#Combine the non_opposing and amended opposing data
merge_aqua_terra_small <- full_join(non_opposing_values, opposing_values) %>% 
  arrange(hylak_id, date) %>%
  mutate(merge_small = ifelse(is.na(modis_mean), ice_modis.MODISTerra, modis_mean)) %>%
  select(date, hylak_id, merge_small)
#calling this "merge_aqua_terra_small" b/c I will add this to the full data set 
#for a merge of all usable data

# 2d. Merge non-overlapping and overlapping data from 2b & 2c-------------------

modis_full_merge <- full_join(aqua_terra_initial, merge_aqua_terra_small) %>%
  arrange(hylak_id, date) %>%
  mutate(full_merge = ifelse(is.na(merge_small), merged_remote_no_overlap, merge_small)) %>%
  select(date, hylak_id, full_merge)
#select(date, hylak_id, cloud_modis.MODISTerra, cloud_modis.MODISAqua, full_merge)

# 3. Write cleaned data file for ice phenology date selection in next script----

write_csv(modis_full_merge, here('data/remote/aqua_terra_merged_clean.csv'))

