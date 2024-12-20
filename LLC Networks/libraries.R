
#Basic R

library(tidyverse)
library(sf)

#Accessing Data
library(RPostgreSQL)
library(dbplyr)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(rio)
library(openxlsx)
library(data.table)

#Spatial
library(arcgis)
library(arcgisbinding)
 arc.check_product()
 arc.check_portal()
library(tidygeocoder)
library(nngeo)


#DataViz
library(leaflet)
library(viridis)
library(ggplot2)
##ggplot
library(ggspatial)
library(ggthemes)
library(ggExtra)
library(extrafont)
library(directlabels)
windowsFonts(TwCenMT = windowsFont("Tw Cen MT"))
library(viridisLite)



#Spatial Reference
mass_mainland<-"+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
lat_lon_CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

