### Warren Group Investor Identification
### Author: Seleeke Flingai
### Updated by: Sarah Philbrick
### Last update date: 05/10/21
### Date: 02/03/2020
### Purpose: Identifies investors and the type (small, medium, large, and institutional) of investor

rm(list=ls())
gc()
#install.packages("pacman")
pacman::p_load(tidyverse, data.table, readxl, lubridate)
options('scipen' = 10)

#Work
muni_path <- "K:/DataServices/Datasets/Data Keys"
data_path = "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"


###### load in muni id data keys
setwd(muni_path)

# loading municipal keys to help distinguish MAPC-specific transactions
muni_key <- read_excel("_datakeys_muni351.xlsx") %>%
  dplyr::select(muni_id, municipal, mapc, county_id, county)

### load in data ###
setwd(data_path)
#list.files()

#change file name here
warren_df <- read_csv('20240328_warren_speculative-investment-analysis-dataset-w-submarket.csv')

####### INVESTOR DEFINITION #1 -- derived from Allen et al., 2018, "Impact of Investors in Distressed Housing Markets" #######
# The definition of investor used in this analysis stems from Allen et al., 2018
# Whereas that paper focuses specifically on single-family houses, our data will maintain the full Warren Group dataset and simply subset when needed

# DEFINTIONS:
# investor = "grantees that purchased 3 or more single-family dwellings or purchased 1 single-family dwelling as an LLC, LP, etc. during the sample period"
# small investor = 3 purchases
# medium investor = 4-5 purchases
# large investor = 6-9 purchases
# institutional investor = 10+
# Allen et al. also define "institutional investors" as those who have purchased more than 10 properties in any calendar year.
##############
investor_count <- function(df, horizon_years){

  #column to identify end of buy horizon based on date of purchase
  df$buy_horizon <- df$date + years(horizon_years)

  temp = df %>%
    group_by(buyer1_adj) %>%
    mutate(prop_count = n()) %>%
    #limit to only buyers that have made at least 3 purchases over entire data period
    filter(prop_count >= 3) %>%
    #Conor's code - identifies number of purchases in moving 4 year window
    mutate(datelist = list(date)) %>%
    rowwise() %>%
    mutate(prop_count_horizon_window = sum(unlist(datelist) <= buy_horizon & unlist(datelist)>=date))

  investor = temp %>%
    filter(prop_count_horizon_window >= 3)

  counts = investor %>%
    group_by(buyer1_adj) %>%
    mutate(max_prop_count = max(prop_count_horizon_window)) %>%
    ungroup() %>%  # new
    mutate(
      investor_purchase_count = ifelse(max_prop_count >= 10, "Institutional",
                                            ifelse(max_prop_count >= 6 & max_prop_count < 10, "Large",
                                                   ifelse(max_prop_count == 4 | max_prop_count == 5, "Medium",
                                                          ifelse(max_prop_count == 3, "Small", "Non-count investor"))))
    )

  small_investor_names = filter(counts, investor_purchase_count == "Small") %>% select(buyer1_adj) %>% unique() %>% pull(buyer1_adj)
  med_investor_names = filter(counts, investor_purchase_count == "Medium") %>% select(buyer1_adj) %>% unique() %>% pull(buyer1_adj)
  large_investor_names = filter(counts, investor_purchase_count == "Large") %>% select(buyer1_adj) %>% unique() %>% pull(buyer1_adj)
  institutional_investor_names = filter(counts, investor_purchase_count == "Institutional") %>% select(buyer1_adj) %>% unique() %>% pull(buyer1_adj)

  #categorical variable that categorizes a BUYER as a small, medium, large, or institutional investor
  df$investor_type_purchase_count = ifelse(df$buyer1_adj %in% small_investor_names, 'Small',
                                           ifelse(df$buyer1_adj %in% med_investor_names, 'Medium',
                                                  ifelse(df$buyer1_adj %in% large_investor_names, 'Large',
                                                         ifelse(df$buyer1_adj %in% institutional_investor_names, 'Institutional', 'Non-count investor'))))
  # create a categorical variable that categorizes a SELLER as a small, medium, large, or institutional investor
  df$investor_type_sale_count = ifelse(df$seller1_adj %in% small_investor_names, 'Small',
                                       ifelse(df$seller1_adj %in% med_investor_names, 'Medium',
                                              ifelse(df$seller1_adj %in% large_investor_names, 'Large',
                                                     ifelse(df$seller1_adj %in% institutional_investor_names, 'Institutional', 'Non-count investor'))))

  # replace NAs in flip-term column with 'Non-flip'
  df$flip_term[is.na(df$flip_term)] = "Non-flip"

  return(df)
}

#warren_df_4yr_count <- investor_count(warren_df, 4)
warren_df_5yr_count <- investor_count(warren_df, 5)
#warren_df_6yr_count <- investor_count(warren_df, 6)
rm(warren_df)

####### INVESTOR DEFINITION 2 - Small LLCs
  #any purchase made by an LLC will be considered an investor purchase

investor_llc <- function(df){
  #Find LLCs and LLPs that have purchased property
  investor_buyers = df %>%
    filter(buyer1_adj %like% ' LLC' | buyer1_adj %like% ' LLP')

  investor_buyer_names = unique(investor_buyers$buyer1_adj)

  df$investor_type_purchase_llc = ifelse(df$buyer1_adj %in% investor_buyer_names, 'Small LLC', 'Non-Small LLC')

  #find LLCs and LLPs that have sold property
  investor_sellers = df %>%
    filter(seller1_adj %like% ' LLC' | seller1_adj %like% ' LLP')

  investor_seller_names = unique(investor_sellers$seller1_adj)

  df$investor_type_sale_llc = ifelse(df$seller1_adj %in% investor_seller_names, 'Small LLC', 'Non-Small LLC')

  # replace NAs in flip-term column with 'Non-flip'
  df$flip_term[is.na(df$flip_term)] = "Non-flip"

  return(df)
}

#warren_df_4yr_count_llc <- investor_llc(warren_df_4yr_count)
warren_df_5yr_count_llc <- investor_llc(warren_df_5yr_count)
#warren_df_6yr_count_llc <- investor_llc(warren_df_6yr_count)
rm(warren_df_5yr_count)

####### INVESTOR DEFINITION #3 -- based on size of building purchased #######
investor_building <- function(df){
  investor <- df %>%
    filter(restype=='APT' | restype=='MUR'| restype=='REO')

  investor_buyer_names = unique(investor$buyer1_adj)
  investor_seller_names = unique(investor$seller1_adj)

  df$investor_type_purchase_building = ifelse(df$buyer1_adj %in% investor_buyer_names, 'Building Investor', 'Non-building investor')

  df$investor_type_sale_building = ifelse(df$seller1_adj %in% investor_seller_names,'Building Investor', 'Non-building investor')

  # replace NAs in flip-term column with 'Non-flip'
  df$flip_term[is.na(df$flip_term)] = "Non-flip"

  return(df)
}

#warren_df_4yr_count_llc_build = investor_building(warren_df_4yr_count_llc)
warren_df_5yr_count_llc_build = investor_building(warren_df_5yr_count_llc)
#warren_df_6yr_count_llc_build = investor_building(warren_df_6yr_count_llc)
rm(warren_df_5yr_count_llc)

####### INVESTOR DEFINITION #4 -- based on total purchase value of buyer's transactions over entire time window #######

investor_value <- function(df){
  total_years <- (max(df$year) - min(df$year)) + 1

  warren_purchases_by_buyer = df %>%
    group_by(buyer1_adj) %>%
    mutate(
      #total_value = sum(price_adj),
      avg_annual_value = sum(price_adj)/total_years
      #log10_total_value = log10(total_value)
    ) %>%
    ungroup() %>%
    mutate(
      #create a categorical variable that categorizes a BUYER as a small, medium, large, or institutional investor
      investor_type_purchase_value = case_when(avg_annual_value < 150000 ~ 'Non-value investor',
                                               avg_annual_value >= 150000 & avg_annual_value < 225000 ~ 'Small',
                                               avg_annual_value >= 225000 & avg_annual_value < 450000 ~ 'Medium',
                                               avg_annual_value >= 450000 & avg_annual_value < 2300000 ~ 'Large',
                                               avg_annual_value >= 2300000  ~ 'Institutional'),
      # create a categorical variable that categorizes a SELLER as a small, medium, large, or institutional investor
      investor_type_sale_value = case_when(avg_annual_value < 150000 ~ 'Non-value investor',
                                           avg_annual_value >= 150000 & avg_annual_value < 225000 ~ 'Small',
                                           avg_annual_value >= 225000 & avg_annual_value < 450000 ~ 'Medium',
                                           avg_annual_value >= 450000 & avg_annual_value < 2300000 ~ 'Large',
                                           avg_annual_value >= 2300000  ~ 'Institutional')
    )

  return(warren_purchases_by_buyer)
}

#warren_df_4yr_count_llc_build_value = investor_value(warren_df_4yr_count_llc_build)
warren_df_5yr_count_llc_build_value = investor_value(warren_df_5yr_count_llc_build)
#warren_df_6yr_count_llc_build_value = investor_value(warren_df_6yr_count_llc_build)
rm(warren_df_5yr_count_llc_build)
gc()

#remove value investors that only bought R1F properties and only currently own 1 property ---------------------------

#table of most recent purchase of all properties in universe
owners <- warren_df_5yr_count_llc_build_value %>%
  group_by(address, municipal) %>%
  slice_max(date) %>%
  ungroup() %>%
  mutate(current_owner = 1) %>%
  select(buyer1_adj, address, municipal, date, current_owner)

#create unique ID columns to compare dfs
owners$ID <- paste(owners$buyer1_adj, owners$address, owners$municipal, owners$date)
warren_df_5yr_count_llc_build_value$ID <- paste(warren_df_5yr_count_llc_build_value$buyer1_adj, warren_df_5yr_count_llc_build_value$address,
                                                warren_df_5yr_count_llc_build_value$municipal, warren_df_5yr_count_llc_build_value$date)

warren_df_5yr_count_llc_build_value_clean <- warren_df_5yr_count_llc_build_value %>%
  mutate(
    # add column to identify whether a buyer is the current owner of the property or not
    current_owner = ifelse(ID %in% owners$ID, 1, 0),
    #index to identify R1F properties
    restype_R1F = ifelse(restype == "R1F", 1, 0)
    ) %>%
  group_by(buyer1_adj) %>%
  mutate(
    #calculate the total number of properties owned by each buyer
    tot_owned = sum(current_owner),
    #find count of total properties attached to buyer name
    buyer_purchases = n(),
    #find number of R1F purchases
    R1F_total = sum(restype_R1F),
    #remove value investors who 1)only purchased R1F properties and 2) currently own only 1 property
    investor_type_purchase_value = replace(investor_type_purchase_value, ((buyer_purchases == R1F_total) & (tot_owned < 2)),
                                          "Non-value investor")
  )

rm(owners, warren_df_5yr_count_llc_build_value)

#move definition of investor_type_purchase and investor_type_sale to new function
investor_overall <- function(df){
  ##create new category based on both value and count, whichever is a 'bigger' investor will win
  df$investor_type_purchase <- ifelse((df$investor_type_purchase_value=='Institutional' | df$investor_type_purchase_count=='Institutional'),
                                      'Institutional',
                                      ifelse((df$investor_type_purchase_value=='Large' | df$investor_type_purchase_count=='Large'),
                                             'Large',
                                             ifelse((df$investor_type_purchase_value=='Medium'| df$investor_type_purchase_count=='Medium'),
                                                    'Medium',
                                                    ifelse((df$investor_type_purchase_value=='Small'| df$investor_type_purchase_count=='Small' | df$investor_type_purchase_llc=='Small LLC' | df$investor_type_purchase_building=='Building Investor'),
                                                           'Small', 'Non-investor'))))

  df$investor_type_sale <- ifelse((df$investor_type_sale_value=='Institutional' | df$investor_type_sale_count=='Institutional'),
                                  'Institutional',
                                  ifelse((df$investor_type_sale_value=='Large' | df$investor_type_sale_count=='Large'),
                                         'Large',
                                         ifelse((df$investor_type_sale_value=='Medium'| df$investor_type_sale_count=='Medium'),
                                                'Medium',
                                                ifelse((df$investor_type_sale_value=='Small'| df$investor_type_sale_count=='Small' | df$investor_type_sale_llc=='Small LLC' | df$investor_type_sale_building=='Building Investor'),
                                                       'Small', 'Non-investor'))))

  df$inv_to_inv_value = ifelse(df$investor_type_purchase_value != 'Non-investor' & df$investor_type_sale_value != 'Non-investor', 1, 0)

  df$inv_to_inv <- ifelse((df$investor_type_purchase!='Non-investor' & df$investor_type_sale!='Non-investor'),1,0)

  # replace NAs in flip-term column with 'Non-flip'
  df$flip_term[is.na(df$flip_term)] = "Non-flip"

  return(df)
}

#warren_df_4yr_final = investor_overall(warren_df_4yr_count_llc_build_value_clean)
warren_df_5yr_final = investor_overall(warren_df_5yr_count_llc_build_value_clean)
#warren_df_6yr_final = investor_overall(warren_df_6yr_count_llc_build_value_clean)
rm(warren_df_5yr_count_llc_build_value_clean)
gc()

# filter on MAPC region
warren_df_5yr_final_mapc = warren_df_5yr_final %>%
  filter(mapc == 1)

######## output csvs
setwd(data_path)

#with foreclosures all
fwrite(warren_df_5yr_final, '20240328_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window.csv')
gc()

#with foreclosures - MAPC
fwrite(warren_df_5yr_final_mapc, '20240328_warren_speculative-investment-analysis-dataset_mapc_withforeclosure_5yr-window.csv')
gc()

#without foreclosures all
warren_df_5yr_final_fd <- warren_df_5yr_final %>%
  mutate(deedtype = ifelse(is.na(deedtype), 'UNKNOWN', deedtype)) %>% 
  filter(deedtype != 'FD')
fwrite(warren_df_5yr_final_fd, '20240328_warren_speculative-investment-analysis-dataset_withoutforeclosure_5yr-window.csv')

#without foreclosures - MAPC
warren_df_5yr_final_mapc_fd <- warren_df_5yr_final_mapc %>%
  mutate(deedtype = ifelse(is.na(deedtype), 'UNKNOWN', deedtype)) %>% 
  filter(deedtype != 'FD')
fwrite(warren_df_5yr_final_mapc_fd, '20240328_warren_speculative-investment-analysis-dataset_mapc_withoutforeclosure_5yr-window.csv')

########## archive #########
#only using 4 year horizon
# warren_df_2yr <- read_csv('20210607_warren_speculative-investment-analysis-dataset-w-submarket_2yr-window.csv')
# warren_df_3yr <- read_csv('20210607_warren_speculative-investment-analysis-dataset-w-submarket_3yr-window.csv')
# warren_df_5yr <- read_csv('20210607_warren_speculative-investment-analysis-dataset-w-submarket_5yr-window.csv')
#
# warren_df_2yr_count_nollc <- investor_count_label(warren_df_2yr)
# warren_df_3yr_count_nollc <- investor_count_label(warren_df_3yr)
# warren_df_5yr_count_nollc <- investor_count_label(warren_df_5yr)
# rm(warren_df_2yr, warren_df_3yr, warren_df_5yr)
#
# warren_df_2yr_count <- investor_count_label_llc(warren_df_2yr_count_nollc)
# warren_df_3yr_count <- investor_count_label_llc(warren_df_3yr_count_nollc)
# warren_df_5yr_count <- investor_count_label_llc(warren_df_5yr_count_nollc)
# rm(warren_df_2yr_count_nollc, warren_df_3yr_count_nollc, warren_df_4yr_count_nollc, warren_df_5yr_count_nollc)
#
# warren_df_2yr_build = investor_building_label(warren_df_2yr_count)
# warren_df_3yr_build = investor_building_label(warren_df_3yr_count)
# warren_df_5yr_build = investor_building_label(warren_df_5yr_count)
# rm(warren_df_2yr_count, warren_df_3yr_count, warren_df_5yr_count)
#
# warren_df_2yr_final = investor_value_label(warren_df_2yr_build)
# warren_df_3yr_final = investor_value_label(warren_df_3yr_build)
# warren_df_5yr_final = investor_value_label(warren_df_5yr_build)
# rm(warren_df_4yr_build)
#
#old code for grouping by total dollar amount
# value_bin = case_when(total_value < 500000 ~ 'Less than $500,000',
#                            total_value >= 500000 & total_value < 1000000 ~ '$500-999,000',
#                            total_value >= 1000000 & total_value < 3000000 ~ '$1-3 million',
#                            total_value >= 3000000 & total_value < 5000000 ~ '$3-5 million',
#                            total_value >= 5000000 & total_value < 10000000 ~ '$5-10 million',
#                            total_value >= 10000000 & total_value < 50000000 ~ '$10-49 million',
#                            total_value >= 50000000 & total_value < 100000000 ~ '$50-99 million',
#                            total_value >= 100000000 & total_value < 250000000 ~ '$100-249 million',
#                            total_value >= 250000000 & total_value < 500000000 ~ '$250-499 million',
#                            total_value >= 500000000 & total_value < 1000000000 ~ '$500-999 million',
#                            total_value >= 1000000000 ~ '$1+ billion')
#
# warren_df_2yr_final_mapc = warren_df_2yr_final %>%
#   filter(muni_id %in% muni_key$muni_id[muni_key$mapc==1]
#          & mapc_submarket %in% c(1:7)
#          & mapc == 1)
# warren_df_3yr_final_mapc = warren_df_3yr_final %>%
#   filter(muni_id %in% muni_key$muni_id[muni_key$mapc==1]
#          & mapc_submarket %in% c(1:7)
#          & mapc == 1)
# warren_df_5yr_final_mapc = warren_df_5yr_final %>%
#   filter(muni_id %in% muni_key$muni_id[muni_key$mapc==1]
#          & mapc_submarket %in% c(1:7)
#          & mapc == 1)
#
# fwrite(warren_df_2yr_final, '20210607_warren_speculative-investment-analysis-dataset_withforeclosure_2yr-window.csv')
# fwrite(warren_df_3yr_final, '20210607_warren_speculative-investment-analysis-dataset_withforeclosure_3yr-window.csv')
# fwrite(warren_df_5yr_final, '20210607_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window.csv')
# rm(warren_df_2yr_final, warren_df_3yr_final, warren_df_5yr_final)
#
# fwrite(warren_df_2yr_final_mapc, '20210607_warren_speculative-investment-analysis-dataset_mapc_withforeclosure_2yr-window.csv')
# fwrite(warren_df_3yr_final_mapc, '20210607_warren_speculative-investment-analysis-dataset_mapc_withforeclosure_3yr-window.csv')
# fwrite(warren_df_5yr_final_mapc, '20210607_warren_speculative-investment-analysis-dataset_mapc_withforeclosure_5yr-window.csv')
