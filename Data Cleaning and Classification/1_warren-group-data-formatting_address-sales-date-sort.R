### Warren Group Data Sort for address and transaction date (in order to identify flips)
### Author: Taylor Perez (modified by Seleeke Flingai, 02/17/2020) (modified by Alexa DeRosa, 01/31/2023. 10/24/2024)
### Date: 11/12/2019 (Taylor Perez)
### Purpose: Sorts the data by address and transaction date in order to ready the dataset for flip identification

rm(list=ls())
#.libPaths("H:/Data/RLibrary")

#install.packages("pacman")
pacman::p_load(tidyverse, data.table)

# Data Paths
wd <- "K:/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified/"
data_path <- 'K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/'

### load in data #######
setwd(wd)
list.files()
#change file name here
warren <- fread("20241220_warren_group_2000_2023_residential_final.csv",
                header=TRUE,
                stringsAsFactors=FALSE,
                colClasses=c('ct_id'='character'))

warren_df <- warren %>%
  mutate(fiscal_flag = ifelse(fy > year,
                              1, # greater than the sale year
                              ifelse(fy == year,
                                     0, # equal to the sale year
                                     -1)),
         address = as.factor(str_to_upper(address))
  )


# table(warren_df$fiscal_flag)
# table(warren_df$year, warren_df$fy)
# table(warren_df$year)

### sort dataframe by formatted address & date
warren_df$address <- as.factor(str_to_upper(warren_df$address)) # uppercase address and make factor
warren_df_sort <- warren_df[order(warren_df$address, warren_df$municipal, warren_df$date),]

### reorder dataframe
warren_df_final <- warren_df_sort %>%
  dplyr::select(ct_id, ct20_id, censustrct, muni_id, county_id, date, month, year, address, street, municipal,
                county, state, zipcode, price, price_adj,mortgage, mortgage2, buyer1, buyer2, seller1, seller2, 
                proptype, everything())

### write sorted output for analysis
fwrite(warren_df_final,
       paste(data_path,
            # gsub("-",'',Sys.Date()),
             '20241220_warren_speculative-investment-analysis-dataset.csv',
             sep=''))
