library(tidyverse)

data_path <- "K:/DataServices/Datasets/OpenCorporates"
setwd(data_path)

additional_identifiers <- read_csv("additional_identifiers.csv")
alternative_names <- read_csv("alternative_names.csv")
companies <- read_csv("companies.csv")
non_reg_addresses <- read_csv("non_reg_addresses.csv")
officers <- read_csv("officers.csv")
