README for Dataset Wrangling R Scripts
Author: Seleeke Flingai, Sarah Philbrick
Date: May 1, 2020, updated June 30, 2021

If you make any changes to the Warren Group residential subset dataset -- add years, adjust the geography, include additional important variables, etc. -- then you 
must run the new Warren Group residential subset dataset through scripts 1-7 in order to get the investor dataset for the MAPC region. Below is a description of what 
each script does. 

Purpose of scripts: 
1_warren-group-data-formatting_address-sales-date-sort_20200429.R: 
Sort the Warren Group residential transaction dataset by address and date to facilitate flip identification

Input: 
The most updated Warren Group residential subset dataset (as of this writing, this is inclusive of data from the years 2000 to 2020 and 
can be found in the file K:/DataServices/Datasets/Housing/Warren Group - Home Sales/Data/Tabular/Modified/20210607_warren_group_2020_2020_residential_final.csv)

Output: 
A dataset sorted by address and date in order to ready the dataset for flip identification
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset.csv
--- 

2_warren-group-data-formatting_seller_buyer_name_adjustments_20200429.R:
Cleans and formats the names of buyers and sellers, particularly that of banks

Input: 
The sorted dataset produced from script 1.
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset.csv

Output: 
A dataset with more consistent buyer/seller naming (e.g. buyer/seller names like "Wells Fargo, "Well Fargo", "Fargo Bank," 
and "Wells, Fargo Bank Na" are all changed to "Wells Fargo Bank")
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_adjusted.csv

--- 

3_warren-group-data-formatting-flip-identification_20200429.R:
Identifies flips, but also:
- adjusts sales price for inflation to 2020 dollars
- creates indicator variables to identify business buyers, trusts, banks, LLCs, government agencies, and government-sponsored entities (e.g. Fannie Mae/Freddie Mac)
- creates indicator variable for whether the previous transaction for a given property was a foreclosure
- develops the 'flip horizon', or the amount of time between transactions for a given property ('month horizon' converts the flip horizon, which is in days, to months)
- identifies same day sales
- identifies 'buy-side flips' vs. 'sell-side flips'
- identifies 'flip term' - short (<6 months), medium (6-12 months), and long-term (12-24 months) flips
- calculates the price difference from the previous transaction to the one in question for a given property

Input: 
The cleaned dataset produced from script 2. 
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_adjusted.csv

Output: 
An updated dataset with the aforementioned variables appended
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_adjusted_detailed.csv

--- 

4_warren-group-data-formatting_buyer-sort_20200429.R:
Sorts Warren Group data by buyer and date to facilitate analysis of "repeat" or "quick" buyers'

Input:
The detailed dataset produced from script 3
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_adjusted_detailed.csv

Output:
The same dataset, simply sorted by buyer and date
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-buyer-sort-analysis-dataset.csv
--- 

5_warren-group-data-formatting_repeat-buyer-identification_20200429.R:
Identifies repeat buyers to help determine which buyers are simply normal home buyers and which are investors
- creates a function that calculates a 'buy horizon' -- that is, the number of days between transactions for a given buyer -- as well as a 
'quick buyer' indicator variable that identifies which buyers have purchased at least two properties within a specified number of years 

Input: 
Dataset produced from script 4:
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-buyer-sort-analysis-dataset.csv

Output:
This function was used to create four outputs, one for a specified time window (2, 3, 4, or 5 year time windows; only the 4yr is showed below): 
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_repeat-buyer_4yr-window.csv 

--- 

6_warren-group-mapc-submarket-join_20200429.R:
Identifies which MAPC Housing Submarket a given property resides
- creates a function that takes in a dataset and the MAPC submarket shapefile and performs a spatial join using the latitude/longitude of the transaction data and 
mapping each transaction to an MAPC submarket (if the transaction took place in the MAPC region; those outside of the MAPC region were not assigned a submarket). 

Input: 
The datasets produced in script 5 

Output:
Updated datasets with MAPC housing submarket information added for applicable transactions
~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset-w-submarket_4yr-window.csv

--- 

7_warren-group_final-df-w-investor-identification_foreclosure_20210510.R
Identifies investors and the type (small, medium, large, and institutional) of investor
- creates four investor functions: 
* FUNCTION 1: Defines investors based on the *number of transactions* for a given repeat buyer
* FUNCTION 2: Defines investors based off of their LLC status that are not captured in Function 1
* FUNCTION 3: Defines investors based off of the size of the building purchased (APT, MUR, and REO restypes)
* FUNCTION 4: Defines investors based on total sales value of transactions for a given repeat buyer 
- this script also creates MAPC-specific subsets of the final dataset after the investor identification steps have been completed. 

Input: 
Datasets produced in script 6
~Regional_Plan_Update_Research/Speculative Investment/Data/20200607_warren_speculative-investment-analysis-dataset-w-submarket_4yr-window.csv

Output:
State-wide and MAPC-specific datasets with investor identification information:
Statewide: ~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_withforeclosure_4yr-window.csv
MAPC subset: ~Regional_Plan_Update_Research/Speculative Investment/Data/20210607_warren_speculative-investment-analysis-dataset_mapc_withforeclosure_4yr-window.csv