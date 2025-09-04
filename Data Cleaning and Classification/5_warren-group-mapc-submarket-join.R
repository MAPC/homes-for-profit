### Warren Group MAPC Housing Submarket spatial join
### Author: Seleeke Flingai
### Date: 04/29/2020
### Purpose: Identifies which MAPC Housing Submarket a given property resides. Addition to also
### identify 2010 census tract and legislative district for each purchase
#Note - previously script 6; takes 13 minutes to run

rm(list=ls())
gc()
pacman::p_load(tidyverse, data.table, bit64, sf, sp, reshape, assertr, assertthat, udpipe, mapcdatakeys, arcgis)
options('scipen' = 10)

#data paths
submarket_path = "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Housing Submarket Typologies/Maps/merged submarkets/"
data <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"

### projections
lat_lon_CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
mass_mainland<-"+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
setwd(submarket_path)

### load in data ############
#list.files()
submarket_shp = st_read("20200109_mapc-housing-submarkets.shp") |> 
  select(class_reor) 

setwd(data)
#list.files()
#changed from fread to csv because data was being lost
warren <- read_csv("20250903_warren_speculative-investment-buyer-sort-analysis-dataset.csv") 

# warren_id <- warren |> 
#   #create unique ID to join on - when using data inclusive of 2023 on this field should already exist
#   mutate(unique_id = unique_identifier(warren, fields = c(everything(warren))))
# 
# rm(warren)
# gc()

#making warren data spatial points
warren_pts <- warren |> 
  select(id, lat, lon) |> 
  #removing any missing latitudes as st_as_sf cannot run if lat or long is missing
  filter(complete.cases(lat, lon)) |> #check how many data points are missing lat/long data
  st_as_sf(coords = c('lon', 'lat'),
           crs = st_crs(lat_lon_CRS),
           remove = FALSE) |> 
  #setting crs to match submarket shapefile
  st_transform(mass_mainland, crs = st_crs(submarket_shp))

#subsetting points to those within submarket to save time
warren_pts_mapc <- warren_pts[submarket_shp,]

#place warren pts in submarkets
warren_submarket <- warren_pts_mapc |> 
  st_join(submarket_shp[, 'class_reor']) |> 
  dplyr::rename(mapc_submarket = class_reor) |> 
  st_drop_geometry() |> 
  select(id, mapc_submarket)

#add submarket data back in to full data frame
warren_submarket_final <- left_join(warren, warren_submarket, by = 'id')
rm(warren, warren_submarket, submarket_shp, warren_pts_mapc)
gc()

#place warren pts in 2010 census tracts
ct_2010 <- mapcdatakeys::tract_sf(2010)

warren_ct2010 <- warren_pts |> 
  st_join(ct_2010[, 'ct10_id']) |> 
  st_drop_geometry() |> 
  select(id, ct10_id)

#add census tract data back in to full data frame
warren_ct2010_final <- left_join(warren_submarket_final, warren_ct2010, by = 'id')
rm(warren_submarket_final, warren_ct2010, ct_2010)

#place warren pts in house districts
house_shp <- st_read("HOUSE2021/HOUSE2021/HOUSE2021_POLY.shp")

warren_house <- warren_pts |> 
  st_join(house_shp[, 'REP_DIST']) |> 
  dplyr::rename(house_dist = REP_DIST) |>
  st_drop_geometry() |> 
  select(id, house_dist)

#add house data back in to full data frame
warren_house_final <- left_join(warren_ct2010_final, warren_house, by = 'id')
rm(warren_ct2010_final, warren_house, house_shp)

#place warren pts in senate districts
senate_shp <- st_read("SENATE2021/SENATE2021_POLY.shp")

warren_senate <- warren_pts |> 
  st_join(senate_shp[, 'SEN_DIST']) |> 
  dplyr::rename(sen_dist = SEN_DIST) |>
  st_drop_geometry() |> 
  select(id, sen_dist)

#add house data back in to full data frame
warren_join_final <- left_join(warren_house_final, warren_senate, by = 'id')
rm(warren_house_final, warren_senate, senate_shp)


########################

# output csv
setwd(data)
fwrite(warren_join_final, "20250903_warren_speculative-investment-analysis-dataset-w-submarket.csv")

########### archive ########
##only need 4 year buy horizon
# warren_2yr = fread("20210607_warren_speculative-investment-analysis-dataset_repeat-buyer_2yr-window.csv", stringsAsFactors = F)
# warren_3yr = fread("20210607_warren_speculative-investment-analysis-dataset_repeat-buyer_3yr-window.csv", stringsAsFactors = F)
# warren_5yr = fread("20210607_warren_speculative-investment-analysis-dataset_repeat-buyer_5yr-window.csv", stringsAsFactors = F)
#
# summary(warren_2yr$lat)
#
# warren_2yr = warren_2yr %>%
#   filter(complete.cases(lat))
#
# warren_3yr = warren_3yr %>%
#   filter(complete.cases(lat))
#
# warren_5yr = warren_5yr %>%
#   filter(complete.cases(lat))
#
# warren_2yr_smkt = submarket_join(warren_2yr, submarket_shp)
# warren_3yr_smkt = submarket_join(warren_3yr, submarket_shp)
# warren_5yr_smkt = submarket_join(warren_5yr, submarket_shp)
#
# rm(warren_2yr, warren_3yr, warren_5yr)
#
#fwrite(warren_2yr_smkt, "20210607_warren_speculative-investment-analysis-dataset-w-submarket_2yr-window.csv")
#fwrite(warren_3yr_smkt, "20210607_warren_speculative-investment-analysis-dataset-w-submarket_3yr-window.csv")
#fwrite(warren_5yr_smkt, "20210607_warren_speculative-investment-analysis-dataset-w-submarket_5yr-window.csv")
