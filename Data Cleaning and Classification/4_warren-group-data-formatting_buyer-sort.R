### Warren Group Data -- Formatting data for buyer/purchaser analysis
### Author: Taylor Perez/Seleeke Flingai
### Date: 01/27/2020 (based on November 2019 code from Taylor Perez; edited on 02/06/2020 by Seleeke Flingai; edited 3/3/2023 by Alexa DeRosa)
### Purpose: Sorts Warren Group data by buyer and date to facilitate analysis of "repeat" or "quick" buyers
# this analysis will help separate regular property purchases from investors, who we are assuming purchase property more regularly than non-investors

rm(list=ls())
gc()
# .libPaths("H:/Data/RLibrary")
#install.packages("pacman")
pacman::p_load(tidyverse, data.table, bit64)

#office
data_path <- 'K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/'

### load in data #########
setwd(data_path)
list.files()
warren <- fread("20241025_warren_speculative-investment-analysis-dataset_adjusted_detailed.csv", stringsAsFactors=FALSE,
                   colClasses=c('ct_id'='character')) # load in raw data

### sort dataframe by buyer1 & date
warren$buyer1_adj <- as.factor(warren$buyer1_adj) # make buyer1 a factor
warren_sort <- warren[order(warren$buyer1_adj, warren$date),]

### adjust date and price variable ############
warren_sort$date <- as.Date(warren_sort$date)
warren_distinct <- distinct(warren_sort)

### write sorted output for analysis
fwrite(warren_distinct, '20241025_warren_speculative-investment-buyer-sort-analysis-dataset.csv')
