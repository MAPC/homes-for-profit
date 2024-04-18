# File for aggregation of data to 2020 census tracts
# both for table to share and mapping

library(tidyverse)
library(sf)

#Office
data_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"

setwd(data_path)

#Data - 5yr window
warren <- read_csv("20240328_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window.csv")

warren_select <- warren %>% 
  select(muni_id, municipal, buyer1_adj, seller1_adj, latitude, longitude, year, cash_sale, price_adj, flip_horizon, flip_ind, buy_side_flip, sell_side_flip,
         price_diff, price_diff_pch, restype, mapc_submarket, investor_type_purchase_count, investor_type_sale_count, 
         investor_type_purchase_llc, investor_type_sale_llc, investor_type_purchase_building, investor_type_sale_building, 
         investor_type_purchase_value, investor_type_sale_value, avg_annual_value, ID, investor_type_purchase, investor_type_sale,
         buyer_gov_ind, seller_gov_ind, buyer_bnk_ind, seller_bnk_ind, buyer_gse_ind, seller_gse_ind)
rm(warren)
  
#statewide calculations
transactions_2000_2023 <- warren_select %>% 
  summarize(trans_0023 = n())

transactions_2002_2021 <- warren_select %>%
  filter(year >= 2002 & year <= 2021) |> 
  summarize(trans_0221 = n())

transactions_2004_2019 <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  summarize(trans_0419 = n())

investor_0419 <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(investor_type_purchase != 'Non-investor') %>% 
  summarize(inv_trans_0419 = n())

investor_0023 <- warren_select %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  summarize(inv_trans_0023 = n())

investor_small <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(investor_type_purchase == 'Small') %>% 
  summarize(sm_inv = n())

investor_medium <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(investor_type_purchase == 'Medium') %>% 
  summarize(med_inv = n())

investor_large <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(investor_type_purchase == 'Large') %>% 
  summarize(lrg_inv = n())

investor_inst <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(investor_type_purchase == 'Institutional') %>% 
  summarize(inst_inv = n())

investor_llc <- warren_select %>% 
  filter(investor_type_purchase_llc == 'Small LLC') %>% 
  summarize(llc_inv = n())

investor_value <- warren_select %>% 
  filter(investor_type_purchase_value != 'Non-value investor') %>% 
  summarize(value_inv = n())

investor_count <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(investor_type_purchase_count != 'Non-count investor') %>% 
  summarize(count_inv = n())

investor_building <- warren_select %>% 
  filter(investor_type_purchase_building == 'Building Investor') %>% 
  summarize(build_inv = n())

cash_buyers <- warren_select %>% 
  filter(cash_sale == 1) %>% 
  summarize(cash_trans = n())

#number of flips 
flips <- warren_select %>% 
  filter(year >= 2002 & year <= 2021) |> 
  filter(flip_ind == 1) %>% 
  summarize(flip_count = n())

#count and % of condo investor purchases
condos <- warren_select %>% 
  filter(restype == 'CON') %>% 
  mutate(con_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(con_inv = n(),
         con_inv_p = con_inv/con_trans) %>% 
  select(con_inv, con_inv_p) %>% 
  distinct()

#count and % of R1F investor purchases
single_fam <- warren_select %>%
  filter(year >= 2004 & year <= 2019) |> 
  filter(restype == 'R1F') %>% 
  mutate(sf_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(sf_inv = n(),
         sf_inv_p = sf_inv/sf_trans) %>% 
  select(sf_inv, sf_inv_p) %>% 
  distinct()

#count and % of R2F investor purchases
r2f <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(restype == 'R2F') %>% 
  mutate(r2f_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(r2f_inv = n(),
         r2f_inv_p = r2f_inv/r2f_trans) %>% 
  select(r2f_inv, r2f_inv_p) %>% 
  distinct()

#count and % of R3F investor purchases
r3f <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) |> 
  filter(restype == 'R3F') %>% 
  mutate(r3f_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(r3f_inv = n(),
         r3f_inv_p = r3f_inv/r3f_trans) %>% 
  select(r3f_inv, r3f_inv_p) %>% 
  distinct()

#joining summary tables
warren_ma <- cbind(transactions_2000_2023, transactions_2002_2021, transactions_2004_2019, 
                   investor_0419, investor_0023) %>% 
  cbind(investor_small) %>%
  cbind(investor_medium) %>%
  cbind(investor_large) %>%
  cbind(investor_inst) %>%
  cbind(investor_llc) %>%
  cbind(investor_value) %>%
  cbind(investor_count) %>% 
  cbind(investor_building) %>% 
  cbind(cash_buyers) %>% 
  cbind(flips) %>% 
  cbind(condos) |> 
  cbind(single_fam) %>% 
  cbind(r2f) %>% 
  cbind(r3f) %>% 
  #calculating %s
  mutate(inv_p = inv_trans_0419/trans_0419, #2004 - 2019 investor transactions/total transactions
         sm_inv_p = sm_inv/inv_trans_0419, #2004 - 2019 small investor transactions/total investor transactions
         med_inv_p = med_inv/inv_trans_0419, #2004 - 2019 medium investor transactions/total investor transactions
         lrg_inv_p = lrg_inv/inv_trans_0419, #2004 - 2019 large investor transactions/total investor transactions
         inst_inv_p = inst_inv/inv_trans_0419, #2004 - 2019 institutional investor transactions/total investor transactions
         li_inv = lrg_inv + inst_inv, #2004 - 2019 total large and instututional transactions
         li_inv_p = li_inv/inv_trans_0419, #2004 - 2019 lrg + inst investor transactions/total investor transactions
         llc_inv_p = llc_inv/inv_trans_0023, #2004 - 2019 llc investor transactions/total investor transactions
         val_inv_p = value_inv/inv_trans_0023, #2004 - 2019 value investor transactions/total investor transactions
         c_inv_p = count_inv/inv_trans_0419, #2004 - 2019 count investor transactions/total investor transactions
         bld_inv_p = build_inv/inv_trans_0023, #2004 - 2023 building investor transactions/total investor transactions
         cash_p = cash_trans/trans_0023, #2000 to 2023 cash transactions / total transactions
         flip_p = flip_count/trans_0221 #2002 to 2021 flip transactions / total transactions
  ) %>% 
  relocate("li_inv", .after = inst_inv) %>% 
  relocate(c("flip_count", "con_inv", "sf_inv", "r2f_inv", "r3f_inv"), .after = cash_trans) %>% 
  relocate(c("flip_p", "con_inv_p", "sf_inv_p", "r2f_inv_p", "r3f_inv_p"), .after = cash_p) #%>% 
  #rename_at(vars(-municipal, -muni_id), ~ paste0("muni_", .x)) 

#export ma table
write.csv(warren_ma, 'warren_ma_agg_2000_2023.csv')

#warren_ma %>% colnames()
# view(warren_ma)
# 
# #incorrect address/muni/coords
# warren %>% 
#   filter(ct20_id == '25001010206' & municipal == 'Bellingham') %>% 
#   select(ct20_id, municipal, latitude, longitude, date, address, street) %>% 
#   mutate(lat = as.character(latitude),
#          lon = as.character(longitude))
# 
# 
# warren %>% 
#   filter(ct20_id == '25001010208' & municipal == 'Plymouth') %>% 
#   select(ct20_id, municipal, latitude, longitude, date, address, street) %>% 
#   mutate(lat = as.character(latitude),
#          lon = as.character(longitude))
