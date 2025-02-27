# Create outputs needed for speculative investment PPT and presentation (Excel charts, data points maps)
library(tidyverse)
library(sf)

#Data Paths

data_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/Data/"
export_path <- "K:/DataServices/Projects/Current_Projects/Regional_Plan_Update_Research/Speculative Investment/visuals/inputs/_2000-2023/MIT tables with name cleaning without property managers"

setwd(data_path)

#Data - 5yr window
warren <- read_csv("20250109_warren_speculative-investment-analysis-dataset_withforeclosure_5yr-window-networks.csv") |>
   mutate(investor = ifelse(investor_type_purchase != "Non-investor", "Investor", "Non-investor"),
          total_transactions = n())
warren_noforeclosures <- read_csv("20250109_warren_speculative-investment-analysis-dataset_withoutforeclosure_5yr-window-networks.csv") |>
   mutate(investor = ifelse(investor_type_purchase != "Non-investor", "Investor", "Non-investor"),
          total_transactions = n())
warren_mapc <- read_csv("20250109_warren_speculative-investment-analysis-dataset_mapc_withforeclosure_5yr-window-networks.csv") |>
  mutate(investor = ifelse(investor_type_purchase != "Non-investor", "Investor", "Non-investor"),
         total_transactions = n())
warren_mapc_noforeclosures <- read_csv("20250109_warren_speculative-investment-analysis-dataset_mapc_withoutforeclosure_5yr-window-networks.csv") |>
  mutate(investor = ifelse(investor_type_purchase != "Non-investor", "Investor", "Non-investor"),
         total_transactions = n())

#Variables to help
res_list <- c("R1F", "R2F", "R3F", "CON")
#can't include first or last 3 years of data due to investor definitions
investor_year_min <- min(warren_mapc$year) + 4
investor_year_max <- max(warren_mapc$year) - 4
#can't include first 2 years or last 2 years due to flip definition
flip_year_min <- min(warren_mapc$year) + 2
flip_year_max <- max(warren_mapc$year) - 2

setwd(export_path)

#Data point - # of transactions in MAPC region
count(warren_mapc)

#Helpful to know - count and % of investors by size and property type
table <- warren_mapc |>
  group_by(investor_type_purchase) |>
  mutate(trans_by_investor_type = n()) |>
  filter(restype %in% res_list | restype == 'APT') |>
  group_by(restype, investor_type_purchase) |>
  mutate(count = n(),
         percent = count/trans_by_investor_type) |>
  select(investor_type_purchase, restype, count, percent) |>
  arrange(investor_type_purchase, restype) |>
  distinct()

# table |>
#   select(investor_type_purchase, restype, count) |>
#   pivot_wider(names_from = 'restype', values_from = 'count')
# 
# table |>
#   select(investor_type_purchase, restype, percent) |>
#   pivot_wider(names_from = 'restype', values_from = 'percent')

#Table 1: Warren Group Real Estate Transactions by Residential Type, MAPC Region
table_1 <- warren_mapc |>
  select(restype, total_transactions) |>
  mutate(restype_group = ifelse(restype == "REO" | restype == "MOB" | restype == "MUR" | restype == "NEW" | restype == "OMR",
                                "Other Residential Buildings", restype)) |> 
  group_by(restype_group) |>
  #count transactions by restype then divide by total transactions in full dataset
  reframe(
    transactions = n(),
    transactions_p = transactions/total_transactions
  ) |>
  distinct()

#table_1
write.csv(table_1, "table_1.csv")
rm(table_1)

#Table 2: Warren Group Real Estate Investor Transactions by Investor Size, MAPC Region
table_2 <- warren_mapc |>
  #filtering to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  #filtering to only investor transactions
  filter(investor == "Investor") |>
  #count of all investor transatcionts
  mutate(total_investor = n()) |>
  group_by(investor_type_purchase) |>
  #count investor transactions by investor size and then calculate %
  reframe(transactions = n(),
          transactions_p = transactions/total_investor) |>
  distinct()

#table_2
write.csv(table_2, "table_2.csv")
rm(table_2)

#Figure 1: Real Estate Transactions by Residential Building Type and Year, MAPC Region
figure_1 <- warren_mapc |>
  #creating grouping used in charts - is Other here all other restypes or is Other REO?
  mutate(res_group = ifelse(!(restype %in% res_list), 'Other', restype)) |>
  group_by(res_group, year) |>
  #count total transactions by above res_groups and year
  summarize(
    transactions = n()
  ) |>
  distinct() |>
  pivot_wider(names_from = year, values_from = transactions) |> 
  arrange(factor(res_group, levels = c("R1F", "CON", "R2F", "R3F", "Other")))

#figure_1
write.csv(figure_1, "figure_1.csv")
rm(figure_1)

#Figure 2: Percent of Real Estate Transactions Purchased with Cash by Year, MAPC Region
figure_2 <- warren_mapc |>
  group_by(year) |>
  #count of annual transactions
  mutate(annual_transactions = n()) |>
  #filter to only cash sales
  filter(cash_sale == 1) |>
  group_by(year, cash_sale) |>
  #get count of cash sales by year and then calculate %
  mutate(count = n(),
         cash_p = count/annual_transactions) |>
  ungroup() |>
  select(year, cash_p) |>
  arrange(year) |>
  distinct()

#figure_2
write.csv(figure_2, "figure_2.csv")
rm(figure_2)

#Figure 3: Total Value of Purchases made by Investors by Year, Millions
figure_3 <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  #filter to investor purchases
  filter(investor == "Investor") |>
  group_by(year) |>
  #sum price of all investor purchases by year
  mutate(total_spent = sum(price_adj),
         total_spent_millions = total_spent/1000000) |>
  select(year, total_spent, total_spent_millions) |>
  arrange(year) |>
  distinct()

#figure_3 |> print(n=25)
write.csv(figure_3, "figure_3.csv")
rm(figure_3)

#Percent of transactions in Metro Boston made by an investor - time period?
investor_p <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  #calculate total transactions in investor years
  mutate(total_transactions_period = n()) |>
  #filter to only investor transactions
  filter(investor == "Investor") |>
  #count total investor transactions then calculate %
  mutate(investor_transactions = n(),
         investor_transactions_p = investor_transactions/total_transactions_period) |>
  select(investor_transactions_p) |>
  distinct()

#investor_p

investor_p_4yrs <- warren_mapc |>
  #filter to investor years
  filter(year >= (investor_year_max - 5) & year <= investor_year_max) |>
  #calculate total transactions in investor years
  mutate(total_transactions_period = n()) |>
  #filter to only investor transactions
  filter(investor == "Investor") |>
  #count total investor transactions then calculate %
  mutate(investor_transactions = n(),
         investor_transactions_p = investor_transactions/total_transactions_period) |>
  select(investor_transactions_p) |>
  distinct()

#investor_p_4yrs
#write.csv(investor_p, "")
rm(investor_p, investor_p_4yrs)

#Figure 4: Investor Purchases by Residential Building Type and Year
by_restype <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(restype, year) |>
  #count transactions by year and restype
  mutate(annual_transactions = n()) |>
  #filter to only investor purchases and selected restypes
  filter(investor == "Investor" & restype %in% res_list) |>
  #count investor purchases by restype and then calculate %
  mutate(investor_transactions = n(),
         investor_transactions_p = investor_transactions/annual_transactions) |>
  select(restype, year, investor_transactions, annual_transactions, investor_transactions_p) |>
  distinct()

all_restype <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(year) |>
  #count transactions by year
  mutate(annual_transactions = n()) |>
  #filter to only investor transactions
  filter(investor == "Investor") |>
  #count total investor transactions across all restypes and calculate %
  mutate(investor_transactions = n(),
         investor_transactions_p = investor_transactions/annual_transactions,
         restype = 'All Residential Types') |>
  select(restype, year, investor_transactions, annual_transactions, investor_transactions_p) |>
  distinct()

#join two dataframes and format for Excel
figure_4 <- rbind(by_restype, all_restype) |>
  select(restype, year, investor_transactions_p) |>
  arrange(year) |>
  pivot_wider(names_from = 'year', values_from = 'investor_transactions_p') |> 
  arrange(factor(restype, levels = c("CON", "R1F", "R2F", "R3F", "All Residential Types")))

#figure_4
write.csv(figure_4, "figure_4.csv")
rm(by_restype, all_restype, figure_4)

#share of transactions that were foreclosures
# warren_mapc |> 
#   group_by(year) |> 
#   mutate(annual_transactions = n()) |> 
#   group_by(year, deedtype) |> 
#   mutate(foreclosure_c = sum(ifelse(deedtype == 'FD', 1, 0)),
#          foreclosure_p = round(100*(foreclosure_c/annual_transactions), digits = 2)
#          ) |> 
#   select(year, deedtype, foreclosure_c, annual_transactions, foreclosure_p) |> 
#   filter(deedtype == 'FD') |> 
#   distinct() |> 
#   arrange(year) |> 
#   view()

#Figure 5: Investor Purchases by Residential Building Type and Year, Excluding Foreclosures
by_restype <- warren_mapc_noforeclosures |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(restype, year) |>
  #count transactions by year and restype
  mutate(annual_transactions = n()) |>
  #filter to only investor purchases and selected restypes
  filter(investor == "Investor" & restype %in% res_list) |>
  #count transactions by investors and restype and calculate %
  mutate(investor_transactions = n(),
         investor_transactions_p = investor_transactions/annual_transactions) |>
  select(restype, year, investor_transactions, annual_transactions, investor_transactions_p) |>
  arrange(year) |>
  distinct()

all_restype <- warren_mapc_noforeclosures |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(year) |>
  #count transactions by year
  mutate(annual_transactions = n()) |>
  #filter to only investor transactions
  filter(investor == "Investor") |>
  #count investor transactions by year and calculate %
  mutate(investor_transactions = n(),
         investor_transactions_p = investor_transactions/annual_transactions,
         restype = 'All Residential Types') |>
  select(restype, year, investor_transactions, annual_transactions, investor_transactions_p) |>
  arrange(year) |>
  distinct()

#join data frames and clean for input to Excel
figure_5 <- rbind(by_restype, all_restype) |>
  select(restype, year, investor_transactions_p) |>
  arrange(year) |>
  pivot_wider(names_from = 'year', values_from = 'investor_transactions_p') |> 
  arrange(factor(restype, levels = c("CON", "R1F", "R2F", "R3F", "All Residential Types")))

figure_5
write.csv(figure_5, "figure_5.csv")
rm(by_restype, all_restype, figure_5)

#Figure 6: Investor Purchases by Investor Size as a Share of All Transactions
figure_6 <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(year) |>
  #count transactions by year
  mutate(annual_transactions = n()) |>
  #filter to only investor transactions
  filter(investor == "Investor") |>
  group_by(year, investor_type_purchase) |>
  #count transactions by year and investor type and calculate %
  mutate(annual_investor_type_transactions = n(),
         transactions_p = annual_investor_type_transactions/annual_transactions) |>
  select(year, investor_type_purchase, transactions_p) |>
  arrange(year) |>
  distinct() |>
  pivot_wider(names_from = 'year', values_from = 'transactions_p') |> 
  arrange(factor(investor_type_purchase, levels = c("Institutional", "Large", "Medium", "Small")))

#figure_6
write.csv(figure_6, "figure_6.csv")
rm(figure_6)

#Figure 7: Percent of Cash Sales by Investor Status and Real Estate Type
figure_7 <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  #filter to selected restypes
  filter(restype %in% res_list) |>
  group_by(restype, investor) |>
  #count total cash sales
  mutate(investor_restype_total = n()) |>
  #filter to cash sales
  filter(cash_sale == 1) |>
  #count transactions by restype and investor status and calculate %
  mutate(count = n(),
         sales_p = count/investor_restype_total) |>
  select(restype, investor, sales_p) |>
  arrange(restype) |>
  distinct() |>
  pivot_wider(names_from = 'investor', values_from = 'sales_p') |> 
  arrange(factor(restype, levels = c("R1F", "R2F", "R3F", "CON")))

#figure_7
write.csv(figure_7, "figure_7.csv")
rm(figure_7)

#Figure 8: Annual Median Sales Price, Condominiums, 1, 2, and 3 Family Properties, Cash vs. Non-Cash Sales
figure_8 <- warren_mapc |>
  #filter to selected restypes
  filter(restype %in% res_list) |>
  select(year, cash_sale, price_adj) |>
  group_by(cash_sale, year) |>
  #calculate median sale value by year and cash sale status
  summarize(
    median_sales = median(price_adj)
  ) |>
  pivot_wider(names_from = 'year', values_from = 'median_sales') |> 
  arrange(cash_sale) #check these

#figure_8
write.csv(figure_8, "figure_8.csv")
rm(figure_8)

#Figure 9: Annual Median Sales Price, Residential 1 Family Properties, Cash vs. Non-Cash Sales
figure_9 <- warren_mapc |>
  #filter to selected restypes
  filter(restype == "R1F") |>
  select(year, cash_sale, price_adj) |>
  group_by(cash_sale, year) |>
  #calculate median sale value by year and cash sale status
  summarize(
    median_sales = median(price_adj)
  ) |>
  pivot_wider(names_from = 'year', values_from = 'median_sales') |> 
  arrange(cash_sale) #check these

#figure_9
write.csv(figure_9, "figure_9.csv")
rm(figure_9)

#Figure 9: Annual Median Sales Price, Condos, Cash vs. Non-Cash Sales
figure_9_1 <- warren_mapc |>
  #filter to selected restypes
  filter(restype == "CON") |>
  select(year, cash_sale, price_adj) |>
  group_by(cash_sale, year) |>
  #calculate median sale value by year and cash sale status
  summarize(
    median_sales = median(price_adj)
  ) |>
  pivot_wider(names_from = 'year', values_from = 'median_sales') |> 
  arrange(cash_sale) #check these

#figure_9_1
write.csv(figure_9_1, "figure_9.1.csv")
rm(figure_9_1)

#Figure 10: Annual Median Sales Price, Residential 3 Family Properties, Cash vs. Non-Cash Sales
figure_10 <- warren_mapc |>
  #filter to selected restypes
  filter(restype == "R3F") |>
  select(year, cash_sale, price_adj) |>
  group_by(cash_sale, year) |>
  #calculate median sale value by year and cash sale status
  summarize(
    median_sales = median(price_adj)
  ) |>
  pivot_wider(names_from = 'year', values_from = 'median_sales') |> 
  arrange(cash_sale) #check these

#figure_10
write.csv(figure_10, "figure_10.csv")
rm(figure_10)

#Figure 11: Share of Transactions that are Investor Purchases by MAPC Submarket
figure_11 <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(mapc_submarket) |>
  #count total transactions by submarket
  mutate(submarket_total = n()) |>
  #filter to investor transactions
  filter(investor == "Investor") |>
  #count investor transactions by submarket and calculate %
  mutate(
    investor_count = n(),
    submarket_p = investor_count/submarket_total
  ) |>
  select(mapc_submarket, submarket_p) |>
  distinct() |>
  arrange(mapc_submarket)

#figure_11
write.csv(figure_11, "figure_11.csv")
rm(figure_11)

#Figure 12: Share of Transactions that are Investor Purchases by MAPC Submarket by Year
figure_12 <- warren_mapc |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(year, mapc_submarket) |>
  #count total transactions by year and submarket
  mutate(annual_submarket_transactions = n()) |> 
  #filter to investor transactions
  filter(investor == "Investor") |>
  #count investor transactions by year and submarket and calculate %
  mutate(
    investor_count = n(),
    submarket_p = investor_count/annual_submarket_transactions
  )  |>
  select(mapc_submarket, year, submarket_p) |>
  distinct() |>
  arrange(mapc_submarket, year) |>
  pivot_wider(names_from = 'year', values_from = 'submarket_p') |> 
  arrange(mapc_submarket)

#figure_12
write.csv(figure_12, "figure_12.csv")
rm(figure_12)

#Table 3: Buy-Side Flip Purchases in the MAPC Region by Residential Type
table_3_restype <- warren_mapc |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  group_by(restype) |>
  #count total transactions by restype
  mutate(total_res_transactions = n()) |>
  #filter to only flipped properties
  filter(flip_ind == 1) |>
  #count flipped properties by restype and calculate %
  mutate(flip_count =  n(),
         flip_p = flip_count/total_res_transactions) |>
  select(restype, flip_count, total_res_transactions, flip_p) |>
  distinct()

table_3_total <-  warren_mapc |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  #filter to just restypes in table
  filter(restype %in% res_list | restype == "APT" | restype == "MUR") |> 
  #count total transactions by restype
  mutate(total_res_transactions = n()) |>
  #filter to only flipped properties
  filter(flip_ind == 1) |>
  #count flipped properties by restype and calculate %
  mutate(flip_count =  n(),
         flip_p = flip_count/total_res_transactions,
         restype = "Total") |>
  select(restype, flip_count, total_res_transactions, flip_p) |>
  distinct()

table_3 <- rbind(table_3_restype, table_3_total)

#table_3
write.csv(table_3, "table_3.csv")
rm(table_3)

# % of residential building types that are flips over study period
warren_mapc |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  #count total transactions by restype
  mutate(total_transactions = n()) |>
  #filter to only flipped properties
  filter(flip_ind == 1) |>
  #count flipped properties by restype and calculate %
  mutate(flip_count =  n(),
         flip_p = flip_count/total_transactions) |>
  select(flip_count, total_transactions, flip_p) |>
  distinct()

#Figure 13: Percent of Purchases That Became Flipped Properties by Year, Excludes foreclosures
figure_13 <- warren_mapc_noforeclosures  |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  group_by(year) |>
  #count total transactions by year
  mutate(annual_transactions = n()) |>
  #filter to only flipped properties
  filter(buy_side_flip == 1) |>
  #count flipped properties by year and calculate %
  mutate(flip_count =  n(),
         flip_p = flip_count/annual_transactions) |>
  select(year, flip_p) |>
  arrange(year) |>
  distinct()

#figure_13
write.csv(figure_13, "figure_13.csv")
rm(figure_13)

#Figure 14: Percent of Purchases That Became Flipped Properties by Real Estate Type and Year, Excludes Foreclosures
figure_14 <- warren_mapc_noforeclosures |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  #filter to selected restypes
  filter(restype %in% res_list | restype == "APT") |>
  group_by(year, restype) |>
  #count transactions by year and restype
  mutate(annual_res_transactions = n()) |>
  #filter to flipped properties
  filter(buy_side_flip == 1) |>
  #count flipped properties by year and restype and calculate %
  mutate(flip_count =  n(),
         flip_p = flip_count/annual_res_transactions) |>
  select(year, restype, flip_p) |>
  distinct() |>
  arrange(year) |>
  pivot_wider(names_from = 'year', values_from = 'flip_p') |> 
  arrange(factor(restype, levels = c("APT", "CON", "R1F", "R2F", "R3F")))

#figure_14
write.csv(figure_14, "figure_14.csv")
rm(figure_14)

#Figure 15: Percent of Purchases That Became Flipped Properties by Investor Size and Real Estate Type, Excludes Foreclosures
figure_15 <- warren_mapc_noforeclosures |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  #filter to selected restypes
  filter(restype %in% res_list | restype == "APT") |>
  group_by(restype, investor_type_sale) |>
  #count transactions by restype and investor size
  mutate(res_investor_transactions = n()) |>
  #filter to flipped properties
  filter(buy_side_flip == 1) |>
  #count flipped properties by restype and investor size and calculate %
  mutate(flip_count =  n(),
         flip_p = flip_count/res_investor_transactions) |>
  select(restype, investor_type_sale, flip_p) |>
  arrange(restype, investor_type_sale) |>
  distinct() |>
  pivot_wider(names_from = 'restype', values_from = 'flip_p') |> 
  arrange(factor(investor_type_sale, levels = c("Institutional", "Large", "Medium", "Small")))

#figure_15
write.csv(figure_15, "figure_15.csv")
rm(figure_15)

#Figure 16: Share of Purchases that Become Flipped Properties, By MAPC Submarket, Excludes Foreclosures
figure_16 <- warren_mapc_noforeclosures |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  group_by(mapc_submarket) |>
  #count transactions by submarket
  mutate(submarket_count = n()) |>
  #filter to flipped properties
  filter(buy_side_flip == 1) |>
  #count flipped properties by submarket and calculate %
  mutate(flip_count = n(),
         submarket_p = flip_count/submarket_count) |>
  select(mapc_submarket, submarket_p) |>
  arrange(mapc_submarket) |>
  distinct()

#figure_16
write.csv(figure_16, "figure_16.csv")
rm(figure_16)

#Figure 17: Annual Median Sales Price by Year, Flips vs. Non-Flips, Excludes Foreclosures
figure_17 <- warren_mapc_noforeclosures |>
  #filter to flip years
  filter(year >= flip_year_min & year <= flip_year_max) |>
  group_by(year, buy_side_flip) |>
  #calculate median sales price by year and flip status
  summarize(median_sales = median(price_adj)) |>
  pivot_wider(names_from = 'year', values_from = 'median_sales') |> 
  arrange(desc(buy_side_flip))

#figure_17
write.csv(figure_17, "figure_17.csv")
rm(figure_17)

#Figure 18: Median Percent Difference in Sales Price of Flipped Single Family Homes, by Investor Type
figure_18 <- warren_mapc_noforeclosures |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  #filter to single family and flipped properties
  filter(restype == "R1F" & flip_ind == 1) |>
  #create investor groups used in chart
  mutate(investor_group = ifelse(investor_type_sale == "Non-investor", "Non-investor",
                                 ifelse(investor_type_sale == "Institutional", "Institutional Investors", "All Other Investors"))
         ) |>
  group_by(year, investor_group) |>
  #calculate median percent price difference by year and investor group -- using flip price difference % variable created in script 3
  summarize(flip_median_dif = median(price_diff_pch)) |>
  pivot_wider(names_from = 'year', values_from = 'flip_median_dif') |> 
  arrange(factor(investor_group, levels = c("All Other Investors", "Institutional Investors", "Non-investor")))

#figure_18
write.csv(figure_18, "figure_18.csv")
rm(figure_18)

# NEW CHART: Figure 19: % investor sales 2000-2022 excluding count investors
llc_investors <- warren_mapc |>
  group_by(year) |>
  mutate(annual_total = n()) |>
  filter(investor_type_purchase_llc != 'Non-Small LLC') |>
  mutate(llc_total = n(),
         llc_p = llc_total/annual_total) |>
  select(year, llc_p) |>
  distinct() |>
  arrange(year)

building_investors <- warren_mapc |>
  group_by(year) |>
  mutate(annual_total = n()) |>
  filter(investor_type_purchase_building != 'Non-building investor') |>
  mutate(building_total = n(),
         building_p = building_total/annual_total) |>
  select(year, building_p) |>
  distinct() |>
  arrange(year)

value_investors <- warren_mapc |>
  group_by(year) |>
  mutate(annual_total = n()) |>
  filter(investor_type_purchase_value != 'Non-value investor') |>
  mutate(value_total = n(),
         value_p = value_total/annual_total) |>
  select(year, value_p) |>
  distinct() |>
  arrange(year)

count_investors <- warren_mapc |>
  group_by(year) |>
  mutate(annual_total = n()) |>
  filter(investor_type_purchase_count != 'Non-count investor') |>
  mutate(count_total = n(),
         count_p = ifelse(year >= investor_year_min & year <= investor_year_max, count_total/annual_total, NA)
                          ) |>
  select(year, count_p) |>
  distinct() |>
  arrange(year)

figure_19 <- inner_join(llc_investors, building_investors,  by = 'year') |>
  inner_join(value_investors, by = 'year') |> 
  inner_join(count_investors, by = 'year')

#figure_19
write.csv(figure_19, "figure_19.csv")
rm(figure_19)

################ From 2023-04-01 PPT
#spat_dat_loc <- "K:/DataServices/Datasets/Boundaries/Spatial/"
#spat_dat_loc <- "S:/Network Shares/K Drive/DataServices/Datasets/Boundaries/Spatial/"
#setwd(spat_dat_loc)

setwd(export_path)

#Slide 9: Investor Purchases as Share of Total Purchases by census tract
slide_9 <- warren |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(ct_id) |>
  #count total transactions by census tract
  mutate(ct_transactions = n()) |>
  #filter to investor purchases
  filter(investor == "Investor") |>
  #count investor purchases by census tract and calculate %
  mutate(investor_count = n(),
         investor_p = investor_count/ct_transactions) |>
  select(ct_id, investor_p) |>
  distinct()

#slide_9

write.csv(slide_9, "slide_9.csv")
rm(slide_9)

#Slide 10: Healthy Neighborhood Communities - not using this
slide_10 <- warren |>
  #filter to investor years
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(municipal) |>
  #count transactions by municipality
  mutate(muni_transactions = n()) |>
  #filter to investor transactions
  filter(investor == "Investor") |>
  #count investor purchases by municipality and calculate %
  mutate(investor_count = n(),
         investor_p = investor_count/muni_transactions) |>
  select(municipal, muni_id, investor_p) |>
  distinct() |>
  arrange(municipal)

#slide_10
write.csv(slide_10, "slide_10.csv")
rm(slide_10)

#Slide 14: Share of Transactions that are Investor Purchases by MAPC Submarket
slide_14 <- warren_mapc |>
  #filter to investor year
  filter(year >= investor_year_min & year <= investor_year_max) |>
  group_by(mapc_submarket) |>
  #count the total transactions by submarket
  mutate(submarket_transactions = n()) |>
  group_by(investor_type_purchase, mapc_submarket) |>
  #count transactions by investor size and submarket and calculate %
  mutate(investor_type_transactions = n(),
         transactions_p = investor_type_transactions/submarket_transactions) |>
  select(mapc_submarket, investor_type_purchase, transactions_p) |>
  arrange(mapc_submarket) |>
  distinct() |>
  pivot_wider(names_from = 'investor_type_purchase', values_from = 'transactions_p')

#slide_14
write.csv(slide_14, "slide_14.csv")
rm(slide_14)