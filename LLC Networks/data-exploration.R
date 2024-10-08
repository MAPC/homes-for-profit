
library(tidyverse)

# Data
who_owns_path <- "C:/Users/aderosa/OneDrive - Metropolitan Area Planning Council/Shared Documents - Data Services/_Current Projects/_Housing/LLC Owner Networks - NNIP/Data/who-owns-csvs/"
hfp_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"

## Tables from MIT who-owns analysis
setwd(who_owns_path)
addresses <- read_csv("addresses.csv")
companies <- read_csv("companies.csv")
company_types <- read_csv("company_types.csv")
metacorps <- read_csv("metacorps.csv")
#munis <- read_csv("munis.csv")
owners <- read_csv("owners.csv")
sites <- read_csv("sites.csv")
#zips <- read_csv("zips.csv")

##  HFP Data
setwd(hfp_path)
hfp_wf <- read_csv("20240328_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window.csv")

#join metacorp info to companies?

# getting distinct owner list with network id to join
owners_inst <- owners |> 
  filter(inst == TRUE) |>
  select(name, cosine_group, network_group) |> 
  distinct()

owners_duplicates <- owners |> 
  filter(inst == TRUE) |> 
  distinct(name, cosine_group, network_group) |> 
  group_by(name) |> 
  mutate(count = n()) |> 
  ungroup() |> 
  filter(count > 1)

owners_dup_joined <- owners_duplicates |> 
  left_join(owners, by = c("name", "cosine_group", "network_group")) |> 
  select(-c('count', '...1')) |> 
  left_join(addresses, by = c("addr_id" = "id")) |> 
  select(name, cosine_group, network_group, addr_id, addr, muni, loc_id, inst, trust, trustees)

test <- left_join(hfp_wf, owners_inst, by = c("buyer1_adj" = "name"))

test_view <- test |> 
  select(address, municipal, buyer1_adj, buyer2_adj, investor_type_purchase, inst, trust, trustees, current_owner, R1F_total)







