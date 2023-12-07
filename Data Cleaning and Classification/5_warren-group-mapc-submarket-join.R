### Warren Group MAPC Housing Submarket spatial join
### Author: Seleeke Flingai
### Date: 04/29/2020
### Purpose: Identifies which MAPC Housing Submarket a given property resides.
#Note - previously script 6

rm(list=ls())
#work
#submarket_path = "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Housing Submarket Typologies/Maps/merged submarkets/"
#data <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"

#home
submarket_path = "S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Housing Submarket Typologies/Maps/merged submarkets/"
data <- 'S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/'


#install.packages("pacman")
pacman::p_load(tidyverse, data.table, bit64, reshape, rgdal, rgeos, assertr, assertthat)
options('scipen' = 10)

### load in data ############
setwd(submarket_path)
list.files()
submarket_shp = readOGR(dsn = '.',
                        layer = "20200109_mapc-housing-submarkets",
                        stringsAsFactors = F,
                        encoding = 'latin1')

setwd(data)
list.files()
#changed from fread to csv because data was being lost
warren = read.csv("20230912_warren_speculative-investment-buyer-sort-analysis-dataset.csv", stringsAsFactors = F)

warren = warren %>%
  filter(complete.cases(latitude))

submarket_join = function(df, shp){

  crs_new = CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
                +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  crs_transform = "+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
        +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  WCS1984_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  proj4string(shp) = crs_new
  shp <- spTransform(submarket_shp, CRS(crs_transform))


  warren_pts = SpatialPoints(data.frame(latitude = df$longitude,
                                        longitude = df$latitude),
                             proj4string = CRS(WCS1984_crs))


  #reproject the points to NAD83
  warren_pts <- spTransform(warren_pts, CRS(crs_transform))
  proj4string(warren_pts)

  #overlay any of the boundaries with the listing records
  warren_pts_submarket <- over(warren_pts, shp)

  #extract the names from each overlay
  try (
    if("class_reor" %in% colnames(warren_pts_submarket))
      mapc_submarket <- warren_pts_submarket$class_reor
    else
      stop("Error! No such variable detected in the submarket shapefile!")
  )


  warren_w_submarket <- cbind(df, mapc_submarket)

  return(warren_w_submarket)
}

warren_smkt = submarket_join(warren, submarket_shp)
rm(warren)

# output csv
setwd(data)
fwrite(warren_smkt, "20230912_warren_speculative-investment-analysis-dataset-w-submarket.csv")

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
