### Warren Group Flip and Investor Identification
### Author: Taylor Perez
### Date: 11/22/2019 (edited 02/17/2020 by Seleeke Flingai, edited 2/10/2023 by Alexa DeRosa)
### Purpose: Identifies home flips and characteristics of the buyers, sellers, and sale itself (including price difference).

rm(list=ls())
gc()
#.libPaths("H:/Data/RLibrary")

# data paths
data_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"

#install.packages("pacman")
pacman::p_load(tidyverse, data.table, bit64, readxl, reshape)
options('scipen' = 10)

### load in data ############
setwd(data_path)
warren <- fread("20250903_warren_speculative-investment-analysis-dataset_adjusted.csv",
                header=TRUE,
                stringsAsFactors=FALSE,
                colClasses=c('ct_id'='character'))

# MAPC runs the price adjustment process below as part of our pre-processing of the warren data

# #change file name to reflect most recent year (only do this if the new file has complete data for the most recent year)
# cpi_index <- read_excel('K:/DataServices/Datasets/Economy/BLS - Consumer Price Index/Output/CPI_BostonMSA_1993-2023.xlsx',
#                           sheet = 'Data') %>%
#   dplyr::select(Year, Avg.)
# 
# cpi_index <- cpi_index %>%
#   mutate(year = as.integer(Year)) %>%
#   select(year, Avg.)
# 
# #variable for most recent year
# recent_year <- cpi_index %>%
#   pull(year) %>%
#   tail(n=1)
# 
# #variable to take Avg. from last row (most recent year) from the cpi_index
# inflation_adj <- cpi_index %>%
#   pull(Avg.) %>%
#   tail(n=1)
#  
# # adjust sales price for inflation to US dollars in most recent year
# warren_temp <- left_join(warren, cpi_index)
# 
# 
# warren_price_adj <- warren_temp %>%
#   mutate(price_adj = ifelse(year != recent_year, # if not the most recent year
#                              price * (inflation_adj / Avg.), # adjust the prices upwards
#                              price)) # otherwise keep the most recent year $ price

warren_buyer_seller_id = warren %>%
  mutate(buyer_llc_ind = ifelse(buyer1_adj %like% 'LLC' | buyer2_adj %like% 'LLC',
                                1,
                                0),
         buyer_llp_ind = ifelse(buyer1_adj %like% ' LLP' | buyer2_adj %like% ' LLP ',
                                1,
                                0),
         buyer_bus_ind = ifelse(buyer1_adj %like% ' INC'
                                | buyer1_adj %like% ' CORP'
                                | buyer1_adj %like% ' CO ' ### I would double check the output to make sure %like% does not pull names like 'Conor' with this string.
                                | buyer2_adj %like% ' INC'
                                | buyer2_adj %like% ' CORP'
                                | buyer2_adj %like% ' CO ',
                                1,
                                0),
         buyer_trst_ind = ifelse(buyer1_adj %like% ' RT'
                                 | buyer1_adj %like% ' FT'
                                 | buyer1_adj %like% ' NT'
                                 | buyer1_adj %like% ' LT'
                                 | buyer1_adj %like% ' TRUST'
                                 | buyer1_adj %like% ' RET'
                                 | buyer1_adj %like% ' IRT'
                                 | buyer2_adj %like% ' RT'
                                 | buyer2_adj %like% ' FT'
                                 | buyer2_adj %like% ' NT'
                                 | buyer2_adj %like% ' LT'
                                 | buyer2_adj %like% ' TRUST'
                                 | buyer2_adj %like% 'RET'
                                 | buyer2_adj %like% ' IRT',
                                 1,
                                 0),
         buyer_bnk_ind = ifelse(buyer1_adj %like% 'BK'
                                | buyer1_adj %like% ' BANK'
                                | buyer1_adj %like% '^BANK OF'
                                | buyer2_adj %like% 'BK'
                                | buyer2_adj %like% ' BANK'
                                | buyer2_adj %like% '^BANK OF',
                                1,
                                0),
         buyer_gse_ind = ifelse(buyer1_adj %in% c('FEDERAL NATIONAL MORTGAGE ASSOCIATION (FANNIE MAE)', 'FEDERAL HOME LOAN MORTGAGE CORPORATION (FREDDIE MAC)')
                                | buyer2_adj %in% c('FEDERAL NATIONAL MORTGAGE ASSOCIATION (FANNIE MAE)', 'FEDERAL HOME LOAN MORTGAGE CORPORATION (FREDDIE MAC)'),
                                1,
                                0),
         buyer_gov_ind = ifelse(buyer1_adj %in% c('MHFA', 'USA HUD')
                                | buyer1_adj %like% 'CITY OF'
                                | buyer2_adj %in% c('MHFA', 'USA HUD')
                                | buyer2_adj %like% 'CITY OF',
                                1,
                                0),
         seller_llc_ind = ifelse(seller1_adj %like% 'LLC' | seller2_adj %like% 'LLC',
                                 1,
                                 0),
         seller_llp_ind = ifelse(seller1_adj %like% ' LLP ' | seller2_adj %like% ' LLP ',
                                1,
                                0),
         seller_bus_ind = ifelse(seller1_adj %like% ' INC'
                                 | seller1_adj %like% ' CORP'
                                 | seller1_adj %like% ' CO ' ### I would double check the output to make sure %like% does not pull names like 'Conor' with this string.
                                 | seller2_adj %like% ' INC'
                                 | seller2_adj %like% ' CORP'
                                 | seller2_adj %like% ' CO ',
                                 1,
                                 0),
         seller_trst_ind = ifelse(seller1_adj %like% ' RT'
                                  | seller1_adj %like% ' FT'
                                  | seller1_adj %like% ' NT'
                                  | seller1_adj %like% ' LT'
                                  | seller1_adj %like% ' TRUST'
                                  | seller1_adj %like% ' RET'
                                  | seller1_adj %like% ' IRT'
                                  | seller2_adj %like% ' RT'
                                  | seller2_adj %like% ' FT'
                                  | seller2_adj %like% ' NT'
                                  | seller2_adj %like% ' LT'
                                  | seller2_adj %like% ' TRUST'
                                  | seller2_adj %like% ' RET'
                                  | seller2_adj %like% ' IRT',
                                  1,
                                  0),
         seller_bnk_ind = ifelse(seller1_adj %like% ' BK'
                                 | seller1_adj %like% ' BANK'
                                 | seller1_adj %like% '^BANK OF'
                                 | seller2_adj %like% ' BK'
                                 | seller2_adj %like% ' BANK'
                                 | seller2_adj %like% '^BANK OF',
                                 1,
                                 0),
         seller_gse_ind = ifelse(seller1_adj %in% c('FEDERAL NATIONAL MORTGAGE ASSOCIATION (FANNIE MAE)', 'FEDERAL HOME LOAN MORTGAGE CORPORATION (FREDDIE MAC)')
                                 | seller2_adj %in% c('FEDERAL NATIONAL MORTGAGE ASSOCIATION (FANNIE MAE)', 'FEDERAL HOME LOAN MORTGAGE CORPORATION (FREDDIE MAC)'),
                                 1,
                                 0),
         seller_gov_ind = ifelse(seller1_adj %in% c('MHFA', 'USA HUD')
                                 | seller1_adj %like% 'CITY OF'
                                 | seller2_adj %in% c('MHFA', 'USA HUD')
                                 | seller2_adj %like% 'CITY OF',
                                 1,
                                 0),
         prev_trans_fd = ifelse(address == lag(address)
                                & municipal == lag(municipal)
                                & lag(deedtype) == 'FD',
                                1,
                                0)
         )



warren_flip_horizon = warren_buyer_seller_id %>%
  #this df needs to be arranged by address only in order for the flip horizon code below to work correctly
  #this arrange() function is breaking the iDate format so I created date_int to use for calculations
  arrange(address, municipal) %>%
  mutate(date_int = as.integer(date),
    flip_horizon = ifelse(address == lag(address)
                               & (municipal == lag(municipal))
                               & seller_bnk_ind == 0 # don't consider bank sales
                               & seller_gse_ind == 0 # don't consider government sponsored enterprises (Fannie Mae/Freddie Mac)
                               & seller_gov_ind == 0 # don't consider local, state, or federal government agencies (e.g. HUD or Massachusetts Housing Finance Agency (MHFA))
                               & (deedtype != 'FD') # don't consider foreclosures
                               & prev_trans_fd == 0, # don't consider property resold by a lender, servicer or MI company that was acquired as a result of a distressed sale through foreclosure
                               (date_int - lag(date_int)),
                               NA))

warren_same_day <- warren_flip_horizon %>%
  mutate(same_day_sale = ifelse(flip_horizon == 0, 1, 0)) # flag same day sales

table(warren_same_day$same_day_sale)

warren_same_day$same_day_sale[is.na(warren_same_day$same_day_sale)] <- 0

warren_detailed = warren_same_day %>%
  filter(same_day_sale == 0) %>% # remove same day sales
  mutate(flip_ind = ifelse(flip_horizon <= (365 *2),
                           1,
                           0),
         buy_side_flip = ifelse(address == lead(address) & municipal == lead(municipal) & lead(flip_ind) == 1,
                                1, 0),
         sell_side_flip = ifelse(address == lag(address) & municipal == lag(municipal) & flip_ind == 1,
                                 1, 0),
         price_diff = ifelse(address == lag(address)
                             & municipal == lag(municipal),
                             price_adj - lag(price_adj),
                             NA),
         price_diff_pch = ifelse(address == lag(address)
                                 & municipal == lag(municipal),
                                 (round((price_adj - lag(price_adj)) / lag(price_adj), 4) * 100),
                                 NA))

table(warren_detailed$flip_ind)

warren_detailed$flip_ind[is.na(warren_detailed$flip_ind)] <- 0
warren_detailed$buy_side_flip[is.na(warren_detailed$buy_side_flip)] <- 0
warren_detailed$sell_side_flip[is.na(warren_detailed$sell_side_flip)] <- 0

table(warren_detailed$flip_ind)
table(warren_detailed$buy_side_flip)
table(warren_detailed$sell_side_flip)


# flip rate for all transactions
flip_rate = table(warren_detailed$flip_ind)[2]/(table(warren_detailed$flip_ind)[1] + table(warren_detailed$flip_ind)[2])

# create 'month horizon' variable that takes the flip horizon number (in days) and converts it to months
# append variable that indicates how many months after initial sale that the repeat sale took place
warren_detailed_final = warren_detailed %>%
  mutate(month_horizon = ifelse(flip_horizon < 30*1, '0',
                                ifelse(flip_horizon >= 30*1 & flip_horizon < 30*2, '1',
                                       ifelse(flip_horizon >= 30*2 & flip_horizon < 30*3, '2',
                                              ifelse(flip_horizon >= 30*3 & flip_horizon < 30*4, '3',
                                                     ifelse(flip_horizon >= 30*4 & flip_horizon < 30*5, '4',
                                                            ifelse(flip_horizon >= 30*5 & flip_horizon < 30*6, '5',
                                                                   ifelse(flip_horizon >= 30*6 & flip_horizon < 30*7, '6',
                                                                          ifelse(flip_horizon >= 30*7 & flip_horizon < 30*8, '7',
                                                                                 ifelse(flip_horizon >= 30*8 & flip_horizon < 30*9, '8',
                                                                                        ifelse(flip_horizon >= 30*9 & flip_horizon < 30*10, '9',
                                                                                               ifelse(flip_horizon >= 30*10 & flip_horizon < 30*11, '10',
                                                                                                      ifelse(flip_horizon >= 30*11 & flip_horizon < 30*12, '11',
                                                                                                             ifelse(flip_horizon >= 30*12 & flip_horizon < 30*13, '12',
                                                                                                                    ifelse(flip_horizon >= 30*13 & flip_horizon < 30*14, '13',
                                                                                                                           ifelse(flip_horizon >= 30*14 & flip_horizon < 30*15, '14',
                                                                                                                                  ifelse(flip_horizon >= 30*15 & flip_horizon < 30*16, '15',
                                                                                                                                         ifelse(flip_horizon >= 30*16 & flip_horizon < 30*17, '16',
                                                                                                                                                ifelse(flip_horizon >= 30*17 & flip_horizon < 30*18, '17',
                                                                                                                                                       ifelse(flip_horizon >= 30*18 & flip_horizon < 30*19, '18',
                                                                                                                                                              ifelse(flip_horizon >= 30*19 & flip_horizon < 30*20, '19',
                                                                                                                                                                     ifelse(flip_horizon >= 30*20 & flip_horizon < 30*21, '20',
                                                                                                                                                                            ifelse(flip_horizon >= 30*21 & flip_horizon < 30*22, '21',
                                                                                                                                                                                   ifelse(flip_horizon >= 30*22 & flip_horizon < 30*23, '22',
                                                                                                                                                                                          ifelse(flip_horizon >= 30*23 & flip_horizon < 30*24, '23',
                                                                                                                                                                                                 ifelse(flip_horizon >= 30*24 & flip_horizon < 30*25, 24,
                                                                                                                                                                                                        ifelse(flip_horizon >= 30*25, '24+', NA)))))))))))))))))))))))))))

# categorize short (< 6 months), medium (6 - 12 months), and long (12-24 months) term flips
warren_detailed_final$flip_term = ifelse(warren_detailed_final$month_horizon %in% c('0', '1', '2', '3', '4', '5'), 'Short-term',
                                         ifelse(warren_detailed_final$month_horizon %in% c('6', '7', '8', '9', '10', '11'), 'Medium-term',
                                                ifelse(warren_detailed_final$month_horizon %in% c('12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24'), 'Long-term', NA)))
# rm(cpi_index, w_back, warren, warren_detailed, warren_flip_horizon, warren_price_adj, warren_temp)
### output files ############
setwd(data_path)
# file name should match input file name with _detailed added
fwrite(warren_detailed_final, '20250903_warren_speculative-investment-analysis-dataset_adjusted_detailed.csv')

####Archive - Don't need to run
#warren$date <- as.Date(warren$date) (in adjust data and price variable section)
# # add quarter
#warren$quarter [warren$month <= 3] = "Q1"
#warren$quarter [(warren$month > 3 & warren$month <= 6)] = "Q2"
#warren$quarter [(warren$month > 6 & warren$month <= 9)] = "Q3"
#warren$quarter [(warren$month > 9 & warren$month <= 12)] = "Q4" (this section was before adjust sales price for inflation)
