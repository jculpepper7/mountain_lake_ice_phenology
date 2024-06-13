#Below is the link to the Google Earth Engine (GEE) data
#
#https://code.earthengine.google.com/?scriptPath=users%2Fjoshuaculpepper7%2FMountain_ice_phenology%3Anorth_america_ice_phenology_2024.03.12
#
#The data combines MODIS imagery from both Aqua and Terra satellites 
#
#Cloud images are removed and a mean value is taken from pixels intersecting the lake polygon
#
#Lake polygons come from the HydoLAKES dataset (Messager et al., 2016), located here: https://www.hydrosheds.org/page/hydrolakes 
#
#After combing the two sensors, it smoothes the data with a moving window median of TK_14 days_TK
#
#For information on the determination of the moving window, see the script in the src folder, entitled ""
#
#The smoothed data was then downloaded from GEE
