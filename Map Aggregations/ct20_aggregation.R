# File for aggregation of data to 2020 census tracts
# both for table to share and mapping

library(tidyverse)
library(sf)

#Office
data_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"
export_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/visuals/inputs/"

#Home
# data_path <- "S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"
# export_path <- "S:/Network Shares/K Drive/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/visuals/inputs/"

setwd(data_path)

#Data - 5yr window
warren <- read_csv("20230131_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window.csv")

warren_select <- warren %>% 
  select(ct20_id, muni_id, municipal, buyer1_adj, seller1_adj, latitude, longitude, year, cash_sale, price_adj, flip_horizon, flip_ind, buy_side_flip, sell_side_flip,
         price_diff, price_diff_pch, restype, mapc_submarket, investor_type_purchase_count, investor_type_sale_count, 
         investor_type_purchase_llc, investor_type_sale_llc, investor_type_purchase_building, investor_type_sale_building, 
         investor_type_purchase_value, investor_type_sale_value, avg_annual_value, ID, investor_type_purchase, investor_type_sale)
rm(warren)

warren_all <- warren_select %>% 
  select(ct20_id) %>% 
  distinct()

#summarizing different variables to 2020 census tracts
ct_transactions <- warren_select %>% 
  group_by(ct20_id) %>% 
  summarize(tot_trans = n())

ct_investor_all <- warren_select %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  group_by(ct20_id) %>% 
  summarize(inv_trans = n())

ct_investor_small <- warren_select %>% 
  filter(investor_type_purchase == 'Small') %>% 
  group_by(ct20_id) %>% 
  summarize(sm_inv = n())

ct_investor_medium <- warren_select %>% 
  filter(investor_type_purchase == 'Medium') %>% 
  group_by(ct20_id) %>% 
  summarize(med_inv = n())

ct_investor_large <- warren_select %>% 
  filter(investor_type_purchase == 'Large') %>% 
  group_by(ct20_id) %>% 
  summarize(lrg_inv = n())

ct_investor_inst <- warren_select %>% 
  filter(investor_type_purchase == 'Institutional') %>% 
  group_by(ct20_id) %>% 
  summarize(inst_inv = n())

ct_investor_llc <- warren_select %>% 
  filter(investor_type_purchase_llc == 'Small LLC') %>% 
  group_by(ct20_id) %>% 
  summarize(llc_inv = n())

ct_investor_value <- warren_select %>% 
  filter(investor_type_purchase_value != 'Non-value investor') %>% 
  group_by(ct20_id) %>% 
  summarize(value_inv = n())

ct_investor_count <- warren_select %>% 
  filter(investor_type_purchase_count != 'Non-count investor') %>% 
  group_by(ct20_id) %>% 
  summarize(count_inv = n())

ct_investor_building <- warren_select %>% 
  filter(investor_type_purchase_building == 'Building Investor') %>% 
  group_by(ct20_id) %>% 
  summarize(build_inv = n())

ct_cash_buyers <- warren_select %>% 
  filter(cash_sale == 1) %>% 
  group_by(ct20_id) %>% 
  summarize(cash_trans = n())

#number of flips 
flips <- warren_select %>% 
  filter(flip_ind == 1) %>% 
  group_by(ct20_id) %>% 
  summarize(flip_count = n())

#count and % of R1F investor purchases
single_fam <- warren_select %>% 
  filter(restype == 'R1F') %>% 
  group_by(ct20_id) %>%
  mutate(sf_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(sf_inv = n(),
         sf_inv_p = sf_inv/sf_trans) %>% 
  select(ct20_id, sf_inv, sf_inv_p) %>% 
  distinct()

#count and % of R2F investor purchases
r2f <- warren_select %>% 
  filter(restype == 'R2F') %>% 
  group_by(ct20_id) %>%
  mutate(r2f_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(r2f_inv = n(),
         r2f_inv_p = r2f_inv/r2f_trans) %>% 
  select(ct20_id, r2f_inv, r2f_inv_p) %>% 
  distinct()

#count and % of R3F investor purchases
r3f <- warren_select %>% 
  filter(restype == 'R3F') %>% 
  group_by(ct20_id) %>%
  mutate(r3f_trans = n()) %>% 
  filter(investor_type_purchase != 'Non-investor') %>% 
  mutate(r3f_inv = n(),
         r3f_inv_p = r3f_inv/r3f_trans) %>% 
  select(ct20_id, r3f_inv, r3f_inv_p) %>% 
  distinct()

#joining summary tables
warren_ct20 <- full_join(warren_all, ct_transactions, by = 'ct20_id') %>% 
  full_join(ct_investor_all, by = 'ct20_id') %>% 
  full_join(ct_investor_small, by = 'ct20_id') %>%
  full_join(ct_investor_medium, by = 'ct20_id') %>%
  full_join(ct_investor_large, by = 'ct20_id') %>%
  full_join(ct_investor_inst,by = 'ct20_id') %>%
  full_join(ct_investor_llc, by = 'ct20_id') %>%
  full_join(ct_investor_value, by = 'ct20_id') %>%
  full_join(ct_investor_count, by = 'ct20_id') %>% 
  full_join(ct_investor_building, by = 'ct20_id') %>% 
  full_join(ct_cash_buyers, by = 'ct20_id') %>% 
  full_join(flips, by = 'ct20_id') %>% 
  full_join(single_fam, by = 'ct20_id') %>% 
  full_join(r2f, by = 'ct20_id') %>% 
  full_join(r3f, by = 'ct20_id') %>% 
  #calculing %s
  mutate(inv_p = inv_trans/tot_trans,
         sm_inv_p = sm_inv/inv_trans,
         med_inv_p = med_inv/inv_trans,
         lrg_inv_p = lrg_inv/inv_trans,
         inst_inv_p = inst_inv/inv_trans,
         li_inv = lrg_inv + inst_inv,
         li_inv_p = li_inv/inv_trans,
         llc_inv_p = llc_inv/inv_trans,
         val_inv_p = value_inv/inv_trans,
         c_inv_p = count_inv/inv_trans,
         bld_inv_p = build_inv/inv_trans,
         cash_p = cash_trans/tot_trans,
         flip_p = flip_count/tot_trans
         ) %>% 
  relocate("li_inv", .after = inst_inv) %>% 
  relocate(c("flip_count", "sf_inv", "r2f_inv", "r3f_inv"), .after = cash_trans) %>% 
  relocate(c("flip_p", "sf_inv_p", "r2f_inv_p", "r3f_inv_p"), .after = cash_p)

#export ct table
write.csv(warren_ct20, 'warren_ct20_agg.csv')
 
# view(warren_ct20)
# warren_ct20 %>% colnames()
