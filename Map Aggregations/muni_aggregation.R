# File for aggregation of data to municipality level
# both for table to share and mapping

library(tidyverse)
library(sf)

#Office
data_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"
#export_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/visuals/inputs/"

#Home
# data_path <- "S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"
# export_path <- "S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/visuals/inputs/"

setwd(data_path)

#Data - 5yr window
warren <- read_csv("20240313_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window.csv")

warren_select <- warren %>% 
  select(muni_id, municipal, buyer1_adj, seller1_adj, latitude, longitude, year, cash_sale, price_adj, flip_horizon, flip_ind, buy_side_flip, sell_side_flip,
         price_diff, price_diff_pch, restype, mapc_submarket, investor_type_purchase_count, investor_type_sale_count, 
         investor_type_purchase_llc, investor_type_sale_llc, investor_type_purchase_building, investor_type_sale_building, 
         investor_type_purchase_value, investor_type_sale_value, avg_annual_value, ID, investor_type_purchase, investor_type_sale) %>% 
  mutate(municipal = case_when(
    municipal == 'Attleborough' ~ 'Attleboro',
    municipal == 'Manchester-by-the-Sea' ~ 'Manchester',
    TRUE ~ municipal
  )) %>% 
  #removing Devens - it is an army base not a municipality
  filter(municipal != 'Devens')

rm(warren)

#list of all municipalities to join to
warren_all <- warren_select %>% 
  select(muni_id, municipal) %>% 
  distinct()

#municipal level calculations
  #variables looking at investor purchases are filtered to the years 2004-2018
  #variables looking at flips are filtered to the years 2002-2020
muni_transactions_all <- warren_select %>% 
  group_by(municipal) %>% 
  summarize(trans_0023 = n())

muni_transactions_2002_2021 <- warren_select %>% 
  filter(year >= 2002 & year <= 2021) %>% 
  group_by(municipal) %>% 
  summarize(trans_0221 = n())

muni_transactions_2004_2019 <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>% 
  group_by(municipal) %>% 
  summarize(trans_0419 = n())

muni_investor_0419 <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase != 'Non-investor') %>% 
  group_by(municipal) %>% 
  summarize(inv_trans_0419 = n())

muni_investor_0023 <- warren_select %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  group_by(municipal) %>% 
  summarize(inv_trans_0023 = n())

muni_investor_small <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase == 'Small') %>% 
  group_by(municipal) %>% 
  summarize(sm_inv = n())

muni_investor_medium <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase == 'Medium') %>% 
  group_by(municipal) %>% 
  summarize(med_inv = n())

muni_investor_large <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase == 'Large') %>% 
  group_by(municipal) %>% 
  summarize(lrg_inv = n())

muni_investor_inst <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase == 'Institutional') %>% 
  group_by(municipal) %>% 
  summarize(inst_inv = n())

muni_investor_llc <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase_llc == 'Small LLC') %>% 
  group_by(municipal) %>% 
  summarize(llc_inv = n())

muni_investor_value <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase_value != 'Non-value investor') %>% 
  group_by(municipal) %>% 
  summarize(value_inv = n())

muni_investor_count <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase_count != 'Non-count investor') %>% 
  group_by(municipal) %>% 
  summarize(count_inv = n())

muni_investor_building <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(investor_type_purchase_building == 'Building Investor') %>% 
  group_by(municipal) %>% 
  summarize(build_inv = n())

muni_cash_buyers <- warren_select %>% 
  filter(cash_sale == 1) %>% 
  group_by(municipal) %>% 
  summarize(cash_trans = n())

#number of flips 
flips <- warren_select %>% 
  filter(year >= 2002 & year <= 2021) %>%
  filter(flip_ind == 1) %>% 
  group_by(municipal) %>% 
  summarize(flip_count = n())

#count and % of condo investor purchases
condos <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(restype == 'CON') %>% 
  group_by(municipal) %>%
  mutate(con_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(con_inv = n(),
         con_inv_p = con_inv/con_trans) %>% 
  select(municipal, con_inv, con_inv_p) %>% 
  distinct()

#count and % of R1F investor purchases
single_fam <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(restype == 'R1F') %>% 
  group_by(municipal) %>%
  mutate(sf_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(sf_inv = n(),
         sf_inv_p = sf_inv/sf_trans) %>% 
  select(municipal, sf_inv, sf_trans, sf_inv_p) %>% 
  distinct()

#count and % of R2F investor purchases
r2f <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(restype == 'R2F') %>% 
  group_by(municipal) %>%
  mutate(r2f_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(r2f_inv = n(),
         r2f_inv_p = r2f_inv/r2f_trans) %>% 
  select(municipal, r2f_inv, r2f_trans, r2f_inv_p) %>% 
  distinct()

#count and % of R3F investor purchases
r3f <- warren_select %>% 
  filter(year >= 2004 & year <= 2019) %>%
  filter(restype == 'R3F') %>% 
  group_by(municipal) %>%
  mutate(r3f_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(r3f_inv = n(),
         r3f_inv_p = r3f_inv/r3f_trans) %>% 
  select(municipal, r3f_inv, r3f_trans, r3f_inv_p) %>% 
  distinct()

#joining summary tables
warren_municipal <- full_join(warren_all, muni_transactions_all, by = 'municipal') %>% 
  full_join(muni_transactions_2002_2021, by = 'municipal') %>% 
  full_join(muni_transactions_2004_2019, by = 'municipal') %>% 
  full_join(muni_investor_0419, by = 'municipal') %>% 
  full_join(muni_investor_0023, by = 'municipal') %>% 
  full_join(muni_investor_small, by = 'municipal') %>%
  full_join(muni_investor_medium, by = 'municipal') %>%
  full_join(muni_investor_large, by = 'municipal') %>%
  full_join(muni_investor_inst,by = 'municipal') %>%
  full_join(muni_investor_llc, by = 'municipal') %>%
  full_join(muni_investor_value, by = 'municipal') %>%
  full_join(muni_investor_count, by = 'municipal') %>% 
  full_join(muni_investor_building, by = 'municipal') %>% 
  full_join(muni_cash_buyers, by = 'municipal') %>% 
  full_join(flips, by = 'municipal') %>% 
  full_join(condos, by = 'municipal') %>% 
  full_join(single_fam, by = 'municipal') %>% 
  full_join(r2f, by = 'municipal') %>% 
  full_join(r3f, by = 'municipal') %>% 
  #calculing %s
  mutate(inv_p = inv_trans/trans_0419, #2004 - 2019 investor transactions/total transactions
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
    relocate(c("flip_p", "con_inv_p", "sf_inv_p", "r2f_inv_p", "r3f_inv_p"), .after = cash_p) 
 # rename_at(vars(-municipal, -muni_id), ~ paste0("muni_", .x)) 

#view(warren_municipal)  

#export muni table
write.csv(warren_municipal, 'warren_municipal_agg_2000_2023.csv')

