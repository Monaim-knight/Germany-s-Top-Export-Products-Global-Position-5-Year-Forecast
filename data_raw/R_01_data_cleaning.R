# Loading the raw dataset for cleaning 
library(dplyr)
library(readr)

trade_history <- read_csv(
  "data_raw/trade_history_6_countries_2010_2024_raw.csv"
)
# Checking the structure of dataset 
dim(trade_history)
######################################
# Keeping only the required columns as we have 50 columns 
#############################################
trade_master <- trade_history %>%
  select(
    ref_year,
    country_name,
    cmd_code,
    cmd_desc,
    primary_value
  )

# checking this new dataset 
glimpse(trade_master)

#########################################
# Renaming columns name accroding to the project and readability
###########################
trade_master <- trade_master %>%
  rename(
    year = ref_year,
    country = country_name,
    hs2_code = cmd_code,
    product_group = cmd_desc,
    export_value_usd = primary_value
  )
names(trade_master)
####################################
# Checking missing values
####################################
colSums(
  is.na(trade_master)
)
##############################
# Checking duplicates 
#######################
trade_master %>%
  count(
    country,
    year,
    hs2_code
  ) %>%
  filter(n > 1)
###########################
# Saving this file 
############################
write_csv(
  trade_master,
  "data_clean/trade_master.csv"
)
########################################
# Create Sector Reference table for a loolup table for Tableau 
#########################################
sector_lookup <- trade_master %>%
  distinct(
    hs2_code,
    product_group
  )
glimpse(sector_lookup)
#########################
# Creating "data_clean" folder in the project root 
##################################
dir.create(
  "data_clean",
  recursive = TRUE
)
####################
# Saving this for future work 
########################
write_csv(
  sector_lookup,
  "data_clean/sector_lookup.csv"
)
##########################################
# Creating market_share 
###########################################
market_share <- trade_master %>%
  group_by(
    year,
    hs2_code
  ) %>%
  mutate(
    total_exports =
      sum(export_value_usd),
    
    market_share =
      export_value_usd /
      total_exports
  ) %>%
  ungroup()
## Validating 
dim(market_share)
# Saving the file 
write_csv(
  market_share,
  "data_clean/market_share.csv"
)
# Verifying 
file.exists(
  "data_clean/market_share.csv"
)
########################################
# Create Country rankings 
##############################
country_rankings <- market_share %>%
  group_by(
    year,
    hs2_code
  ) %>%
  mutate(
    rank =
      rank(
        -export_value_usd,
        ties.method = "min"
      )
  ) %>%
  ungroup()
# Validating 
dim(country_rankings)
# Saving the file 
write_csv(
  country_rankings,
  "data_clean/country_rankings.csv"
)
# Verifying 
file.exists(
  "data_clean/country_rankings.csv"
)
