# The goal of this script is to:
# 1. Load and separate ice on and ice off data by trends
# 2. Create a map with points representing lakes and colored by trend
# 3. Create "zoomed in" maps on the northern Rockies and Sierras
# 4. Combine the large map and zoomed maps

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

#Require the developer version to get the ne_states() function to work.
# devtools::install_github("ropenscilabs/rnaturalearth")
# devtools::install_github("ropenscilabs/rnaturalearthdata")
# install.packages("rnaturalearthhires",
#                  repos = "http://packages.ropensci.org",
#                  type = "source")

# 2. Load data and map ---------------------------------------------------------

#Found mapping code here: https://evamaerey.github.io/flipbooks/geom_sf/geom_sf.html#32

#Prepare N. American map
theme_set(theme_bw())

#Load map data
na <- rnaturalearth::ne_states(
  returnclass = "sf") 

#Load Mann-Kendall / Sen's slope data
mk_sen_test <- read_csv(here('data/n_american_ice_phenology/results/mk_results.csv'))

#Load HydroLAKES data
hydro_lakes <- read_csv(here('data/remote/sierra_lake_ice_data/hydroLAKES_N_America.csv')) %>% 
  clean_names() %>%
  filter(continent == "North America") %>% 
  select(-1, -23) %>% 
  mutate(
    hylak_id = as.factor(hylak_id)
    ) %>% 
  rename(Hylak_id = hylak_id)


# 3. Add lat and long to trend data---------------------------------------------

trend_plotting_data <- mk_sen_test %>%
  mutate(
    Hylak_id = as.factor(Hylak_id)
  ) %>% 
  inner_join(hydro_lakes)

# 4. Import ice on trend data---------------------------------------------------
rs_results_tbl_ice_on <- read_csv(here('data/n_american_ice_phenology/results/rs_results_tbl_ice_on.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    .pred_class = as.factor(.pred_class)
  ) %>% 
  select(Hylak_id, class = .pred_class, prob_suitable = .pred_suitable, prob_unsuitable = .pred_unsuitable)

# 5. Join results table with ice on plotting data-------------------------------

ice_on_trend_plotting_data <- trend_plotting_data %>%  #2377 lakes with 20 years of ice off data (above 35 degrees lat)
  filter(event == 'ice_on',
         pour_lat > 35) %>% 
  inner_join(rs_results_tbl_ice_on) %>% 
  filter(class == 'suitable')                          

#723 suitable for ice on (30% of total lakes with 20 yrs data)
#604 != 0 (84%), 304 with sen > 0 (63%), 265 with sen < 0 (44%) (contrary to significant results)  
#63 with a significant MK p value (~9% of suitable lakes), 38 with sen < 0 (60%), 25 with sen > 0 (40%)
#103 at p value <= 0.1 (14%) 48 later, 55 earlier
#181 at p value <= 0.2 (25%)
#253 at p value <= 0.3 (35%)

# 6. Ice on map-----------------------------------------------------------------

# 6a. Full ice on map----
full_map_on <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-148, -103), ylim = c(34, 64), expand = FALSE)+
  geom_point(data = ice_on_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 2, pch =21, stroke = 0.5)+
  geom_point(data = ice_on_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), stroke = 0.5, pch = 24, size = 2)+
  scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'A')+
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.4, 'in'),
    aspect.ratio = 1
    #plot.margin = unit(c(0, 0, 0, 1), 'in')
  )

  #save a copy of the ice on plot
#ggsave(here("output/maps/ice_on_map_2022.08.11.jpeg"), dpi = 300, width = 6, height = 6, units = "in") #, width = 15, height = 15, units = "in"

# 6b. Sierra ice on map----

sierra_map_on <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-120.5, -118.3), ylim = c(36.3, 39.1))+ #, expand = FALSE #NOTE: expand = F caused a problem. Unsure why.
  geom_point(data = ice_on_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch =21, stroke = 0.5)+
  geom_point(data = ice_on_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 24, stroke = 0.5)+
  scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'B')+
  theme(
    legend.position = 'none',
    aspect.ratio = 1,
    axis.ticks = element_blank(),
    axis.text = element_blank()#,
    #plot.margin = unit(c(0, 0, 0, -5), 'in')
  )

#save a copy of the ice on plot
#ggsave(here("output/maps/ice_on_map_sierra_no_axes_2022.08.11.jpeg"), dpi = 300, width = 3, height = 3, units = "in") #, width = 15, height = 15, units = "in"

# 6c. Rockies ice on map----

rockies_map_on <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-122, -115), ylim = c(49.1, 55))+ #, expand = FALSE #NOTE: expand = F caused a problem. Unsure why.
  geom_point(data = ice_on_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch =21, stroke = 0.5)+
  geom_point(data = ice_on_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 24, stroke = 0.5)+
  scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'C')+
  theme(
    legend.position = 'none',
    axis.ticks = element_blank(),
    axis.text = element_blank()#,
    #plot.margin = unit(c(0, 0, 0, 0), 'in') #plot.margin = unit(c(top,right,bottom,left))
  )

#save a copy of the ice on plot
#ggsave(here("output/maps/ice_on_map_rockies_no_axes_2022.08.01.jpeg"), dpi = 300, width = 8, height = 8, units = "in") #, width = 15, height = 15, units = "in"

# 6d. Combine into one plot----

on_map_combined <- full_map_on / (sierra_map_on | rockies_map_on)

#ggsave(here('output/maps/ice_on_map_combined_2022.08.02.jpeg'), dpi = 300, width = 7.5, height = 7.5, units = 'in')

# 7. Import ice off trend data--------------------------------------------------

# Add the suitability dataset to give opacity as suitability probability

rs_results_tbl_ice_off <- read_csv(here('data/n_american_ice_phenology/results/rs_results_tbl_ice_off.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    .pred_class = as.factor(.pred_class)
  ) %>% 
  select(Hylak_id, class = .pred_class, prob_suitable = .pred_suitable, prob_unsuitable = .pred_unsuitable)

# 8. Join results table with ice off plotting data------------------------------

ice_off_trend_plotting_data <- trend_plotting_data %>%   #1,675 lakes with 20 years of ice off data
  filter(event == 'ice_off',                             
         pour_lat > 35) %>% 
  inner_join(rs_results_tbl_ice_off) %>% 
  filter(class == 'suitable')                           #1,548 suitable for ice off (92% of total lakes with 20 yrs data)
                              
#1,372 != 0, 809 are negative slope (58%), 563 are positive slope (41%), 176 with 0 slope (13%) 
#74 with significant MK p value (~5% of suitable lakes), 62 with negative slope (84%), 12 with pos slope (16%), None with 0 slope                          
#137 at p value <= 0.1 (9%)
#282 at p value <= 0.2 (18%)
#444 at p value <= 0.3 (29%)

# 8. Ice off map----------------------------------------------------------------

# 8a. Full ice on map----
full_map_no_trend <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-148, -103), ylim = c(34, 64), expand = FALSE)+
  geom_point(data = ice_off_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 2, pch =21, stroke = 0.5)+
  geom_point(data = ice_off_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 2, pch =24, stroke = 0.5)+
  scale_fill_gradient2(midpoint = 0, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'A')+
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.4, 'in'),
    aspect.ratio = 1
    #plot.margin = unit(c(0, 0, 0, 0), 'in')
  )

#save a copy of the ice on plot
#ggsave(here("output/maps/ice_off_map_2022.08.11.jpeg"), dpi = 300, width = 6, height = 6, units = "in") #, width = 15, height = 15, units = "in"

# 8b. Sierra ice off map----

sierra_map_off <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-120.5, -118.3), ylim = c(36.3, 39.1))+ #, expand = FALSE #NOTE: expand = F caused a problem. Unsure why.
  geom_point(data = ice_off_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 21, stroke = 0.5)+
  geom_point(data = ice_off_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 24, stroke = 0.5)+
  scale_fill_gradient2(midpoint = 0, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'B')+
  theme(
    legend.position = 'none',
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), 'in') #plot.margin = unit(c(top,right,bottom,left))
  )

#save a copy of the ice on plot
#ggsave(here("output/maps/ice_off_map_sierra_2022.08.01.jpeg"), dpi = 300, width = 8, height = 8, units = "in") #, width = 15, height = 15, units = "in"

# 8c. Rockies ice on map----

rockies_map_off <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-122, -115), ylim = c(49.1, 55))+ #, expand = FALSE #NOTE: expand = F caused a problem. Unsure why.
  geom_point(data = ice_off_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch =21, stroke = 0.5)+
  geom_point(data = ice_off_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 24, stroke = 0.5)+
  scale_fill_gradient2(midpoint = 0, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'C')+
  theme(
    legend.position = 'none',
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), 'in') #plot.margin = unit(c(top,right,bottom,left))
  )

#save a copy of the ice on plot
#ggsave(here("output/maps/ice_off_map_rockies_2022.08.01.jpeg"), dpi = 300, width = 8, height = 8, units = "in") #, width = 15, height = 15, units = "in"

# 8d. Combine into one plot----

off_map_combined <- full_map_off | (sierra_map_off / rockies_map_off)

#ggsave(here('output/maps/ice_off_map_combined_2022.08.02.jpeg'), dpi = 300, width = 7.5, height = 7.5, units = 'in')

# 9. No trend map----------------------------------------------------------------

#Combine ice on and off trends that show neither positive nor negative trends

ice_off_no_trend <- ice_off_trend_plotting_data %>% 
  select(Hylak_id, event, sen) %>% 
  filter(sen == 0)

ice_on_no_trend <- ice_on_trend_plotting_data %>% 
  select(Hylak_id, event, sen) %>% 
  filter(sen == 0)

no_trend <- ice_off_no_trend %>% 
  inner_join(ice_on_no_trend) %>% 
  filter(sen == 0)
#This tells us that there are no lakes that experience neither increase nor 
#decrease in trend for both ice on and ice off

# 9a. Full no trend map----
full_map_no_trend_on <- ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-148, -103), ylim = c(34, 64), expand = FALSE)+
  #geom_point(data = ice_off_trend_plotting_data %>% filter(sen == 0), aes(x = pour_long, y = pour_lat, fill = sen), size = 2, pch =21, stroke = 0.5)+
  geom_point(data = ice_on_trend_plotting_data %>% filter(sen == 0), aes(x = pour_long, y = pour_lat, fill = sen), size = 2, pch =21, stroke = 0.5)+
  scale_fill_gradient2(midpoint = 0, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'A')+
  theme(
    legend.position = 'none',
    legend.key.height = unit(0.4, 'in'),
    aspect.ratio = 1
  )

#save a copy of the ice on plot
ggsave(here("output/maps/ice_on_no_trend_map.jpeg"), dpi = 300, width = 6, height = 6, units = "in") #, width = 15, height = 15, units = "in"

# 9b. Sierra no trend map----

# sierra_map_off <- ggplot() +
#   ggplot2::geom_sf(data = na) +
#   coord_sf(xlim = c(-120.5, -118.3), ylim = c(36.3, 39.1))+ #, expand = FALSE #NOTE: expand = F caused a problem. Unsure why.
#   geom_point(data = ice_off_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 21, stroke = 0.5)+
#   geom_point(data = ice_off_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 24, stroke = 0.5)+
#   scale_fill_gradient2(midpoint = 0, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
#   #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
#   xlab("")+
#   ylab("")+
#   #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
#   labs(fill = '', tag = 'B')+
#   theme(
#     legend.position = 'none',
#     axis.ticks = element_blank(),
#     axis.text = element_blank(),
#     plot.margin = unit(c(0, 0, 0, 0), 'in') #plot.margin = unit(c(top,right,bottom,left))
#   )
# 
# #save a copy of the ice on plot
# #ggsave(here("output/maps/ice_off_map_sierra_2022.08.01.jpeg"), dpi = 300, width = 8, height = 8, units = "in") #, width = 15, height = 15, units = "in"
# 
# # 9c. Rockies no trend map----
# 
# rockies_map_off <- ggplot() +
#   ggplot2::geom_sf(data = na) +
#   coord_sf(xlim = c(-122, -115), ylim = c(49.1, 55))+ #, expand = FALSE #NOTE: expand = F caused a problem. Unsure why.
#   geom_point(data = ice_off_trend_plotting_data %>% filter(p.value >= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch =21, stroke = 0.5)+
#   geom_point(data = ice_off_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), size = 1, pch = 24, stroke = 0.5)+
#   scale_fill_gradient2(midpoint = 0, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
#   #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
#   xlab("")+
#   ylab("")+
#   #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
#   labs(fill = '', tag = 'C')+
#   theme(
#     legend.position = 'none',
#     axis.ticks = element_blank(),
#     axis.text = element_blank(),
#     plot.margin = unit(c(0, 0, 0, 0), 'in') #plot.margin = unit(c(top,right,bottom,left))
#   )
# 
# #save a copy of the ice on plot
# #ggsave(here("output/maps/ice_off_map_rockies_2022.08.01.jpeg"), dpi = 300, width = 8, height = 8, units = "in") #, width = 15, height = 15, units = "in"
# 
# # 9d. Combine into one plot----
# 
# off_map_combined <- full_map_off | (sierra_map_off / rockies_map_off)

#ggsave(here('output/maps/ice_off_map_combined_2022.08.02.jpeg'), dpi = 300, width = 7.5, height = 7.5, units = 'in')


  
  