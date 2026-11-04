############################################
# Global Market Position
# Germany's four key export sectors
############################################

library(dplyr)
library(readr)

############################################
# Load global yearly export files
############################################

global_files <- list.files(
  "data_raw/global_yearly",
  pattern = "^global_exports_[0-9]{4}_raw\\.csv$",
  full.names = TRUE
)

length(global_files)
################################################
# Combine all of the 15 years 
################################################
global_trade_history <- global_files %>%
  lapply(
    readr::read_csv,
    show_col_types = FALSE
  ) %>%
  dplyr::bind_rows()
dim(global_trade_history)
sort(
  unique(global_trade_history$ref_year)
)
range(
  global_trade_history$ref_year,
  na.rm = TRUE
)
# Verifying the four products 
global_trade_history %>%
  count(
    cmd_code
  ) %>%
  arrange(cmd_code)
length(global_files)
############################################
# Step 7: Check duplicate global records
############################################

global_duplicate_records <- global_trade_history %>%
  count(
    ref_year,
    reporter_code,
    cmd_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)
global_duplicate_records
nrow(global_duplicate_records)
############################################
# Step 8: Check missing values
############################################

global_missing_values <- colSums(
  is.na(global_trade_history)
)

global_missing_values

############################################
# Missing values in critical project fields
############################################

global_critical_missing <- global_trade_history %>%
  summarise(
    missing_year =
      sum(is.na(ref_year)),
    
    missing_reporter =
      sum(is.na(reporter_code)),
    
    missing_country =
      sum(is.na(reporter_desc)),
    
    missing_hs2 =
      sum(is.na(cmd_code)),
    
    missing_product =
      sum(is.na(cmd_desc)),
    
    missing_export_value =
      sum(is.na(primary_value))
  )

global_critical_missing
# Display all six missing values 
print(
  global_critical_missing,
  width = Inf
)
print(global_critical_missing, width = Inf)
############################################
# Step 9: Check zero or negative export values
############################################

invalid_global_export_values <- global_trade_history %>%
  filter(
    primary_value <= 0
  )
nrow(invalid_global_export_values)

global_export_value_check <- global_trade_history %>%
  summarise(
    zero_export_values =
      sum(primary_value == 0, na.rm = TRUE),
    
    negative_export_values =
      sum(primary_value < 0, na.rm = TRUE),
    
    positive_export_values =
      sum(primary_value > 0, na.rm = TRUE)
  )

global_export_value_check
print(
  global_export_value_check,
  width = Inf
)

############################################
# Step 10: Create clean global trade table
############################################

global_trade_master <- global_trade_history %>%
  select(
    ref_year,
    reporter_code,
    reporter_iso,
    reporter_desc,
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  rename(
    year = ref_year,
    country_code = reporter_code,
    country_iso3 = reporter_iso,
    country = reporter_desc,
    hs2_code = cmd_code,
    product_group = cmd_desc,
    export_value_usd = primary_value
  ) %>%
  arrange(
    year,
    hs2_code,
    desc(export_value_usd)
  )
dim(global_trade_master)
names(global_trade_master)
dplyr::glimpse(global_trade_master)
nrow(global_trade_master) == nrow(global_trade_history)

############################################
# Step 11: Save clean global trade master
############################################

readr::write_csv(
  global_trade_master,
  "data_clean/global_trade_master.csv"
)
file.exists(
  "data_clean/global_trade_master.csv"
)

global_trade_master_check <- readr::read_csv(
  "data_clean/global_trade_master.csv",
  show_col_types = FALSE
)
dim(global_trade_master_check)
nrow(global_trade_master_check) ==
  nrow(global_trade_master)

############################################
# Step 12: Calculate global market position
############################################

global_market_position <- global_trade_master %>%
  group_by(
    year,
    hs2_code
  ) %>%
  mutate(
    global_total_exports_usd =
      sum(export_value_usd, na.rm = TRUE),
    
    global_market_share =
      export_value_usd / global_total_exports_usd,
    
    global_rank =
      min_rank(desc(export_value_usd)),
    
    reporting_country_count =
      n_distinct(country_code)
  ) %>%
  ungroup() %>%
  arrange(
    year,
    hs2_code,
    global_rank
  )
dim(global_market_position)
names(global_market_position)
nrow(global_market_position) ==
  nrow(global_trade_master)
dplyr::glimpse(global_market_position)
#####################################################
# Convert HS2 code to text 
##############################################
global_market_position <- global_market_position %>%
  mutate(
    hs2_code = sprintf(
      "%02d",
      as.integer(hs2_code)
    )
  )

class(global_market_position$hs2_code)

##############################################
# Add Tableau friendly measures 
############################################
global_market_position <- global_market_position %>%
  mutate(
    export_value_usd_billions =
      export_value_usd / 1e9,
    
    global_total_exports_usd_billions =
      global_total_exports_usd / 1e9,
    
    global_market_share_pct =
      global_market_share * 100
  )
##############################################
# Validate global market shares 
############################################

global_share_validation <- global_market_position %>%
  summarise(
    total_global_share =
      sum(global_market_share, na.rm = TRUE),
    
    reporting_country_count_calculated =
      n_distinct(country_iso3),
    
    .by = c(
      year,
      hs2_code
    )
  ) %>%
  mutate(
    share_difference =
      abs(total_global_share - 1),
    
    share_is_valid =
      share_difference < 1e-10
  )

# Inspect invalid groups 
global_share_validation %>%
  filter(!share_is_valid) %>%
  print(n = Inf)
dim(global_share_validation)
##############################
# Validate reporting country counts 
#################################
reporting_count_validation <-
  global_market_position %>%
  summarise(
    stored_reporting_country_count =
      first(reporting_country_count),
    
    calculated_reporting_country_count =
      n_distinct(country_iso3),
    
    .by = c(
      year,
      hs2_code
    )
  ) %>%
  mutate(
    count_matches =
      stored_reporting_country_count ==
      calculated_reporting_country_count
  )
# Inspect mismatches 
reporting_count_validation %>%
  filter(!count_matches) %>%
  print(n = Inf)
# Validate global rankings 
global_rank_validation <- global_market_position %>%
  summarise(
    minimum_rank =
      min(global_rank, na.rm = TRUE),
    
    maximum_rank =
      max(global_rank, na.rm = TRUE),
    
    distinct_ranks =
      n_distinct(global_rank),
    
    reporting_countries =
      n_distinct(country_iso3),
    
    .by = c(
      year,
      hs2_code
    )
  )
# Inspect invalid groups 
global_rank_validation %>%
  filter(
    minimum_rank != 1 |
      maximum_rank > reporting_countries
  ) %>%
  print(n = Inf)
##########################################
# Verify rank and export value consistency 
############################################
rank_order_validation <-
  global_market_position %>%
  arrange(
    year,
    hs2_code,
    global_rank
  ) %>%
  group_by(
    year,
    hs2_code
  ) %>%
  mutate(
    previous_export_value =
      lag(export_value_usd),
    
    rank_order_valid =
      is.na(previous_export_value) |
      export_value_usd <= previous_export_value
  ) %>%
  ungroup()
#########################################
# Inspect problems 
##########################################
rank_order_validation %>%
  filter(!rank_order_valid) %>%
  select(
    year,
    hs2_code,
    country,
    export_value_usd,
    global_rank,
    previous_export_value
  ) %>%
  print(n = Inf)
###########################################
# Verify Germany's coverage 
##########################################
germany_market_coverage <-
  global_market_position %>%
  filter(country_iso3 == "DEU") %>%
  summarise(
    record_count = n(),
    year_count = n_distinct(year),
    product_count = n_distinct(hs2_code),
    minimum_year = min(year),
    maximum_year = max(year)
  )
germany_market_coverage
##############################################
# Create Germany's historical global position table 
##############################################
germany_global_position <-
  global_market_position %>%
  filter(country_iso3 == "DEU") %>%
  select(
    year,
    country_iso3,
    country,
    hs2_code,
    product_group,
    export_value_usd,
    export_value_usd_billions,
    global_total_exports_usd,
    global_total_exports_usd_billions,
    global_market_share,
    global_market_share_pct,
    global_rank,
    reporting_country_count
  ) %>%
  arrange(
    hs2_code,
    year
  )
dim(germany_global_position)
#############################################
# Germany's 2024 positon 
##################################################
germany_global_position %>%
  filter(year == 2024) %>%
  select(
    year,
    hs2_code,
    product_group,
    export_value_usd_billions,
    global_market_share_pct,
    global_rank,
    reporting_country_count
  ) %>%
  arrange(global_rank) %>%
  print(n = 4)
##############################################
# Save the validated tables 
########################################################

readr::write_csv(
  global_market_position,
  "data_clean/global_market_position_2010_2024.csv"
)

readr::write_csv(
  germany_global_position,
  "data_clean/germany_global_position_2010_2024.csv"
)

readr::write_csv(
  global_share_validation,
  "data_clean/global_share_validation.csv"
)

readr::write_csv(
  global_rank_validation,
  "data_clean/global_rank_validation.csv"
)

readr::write_csv(
  reporting_count_validation,
  "data_clean/reporting_count_validation.csv"
)

############################################
# Step 13: Verify saved global-position files
############################################

global_position_files <- c(
  "data_clean/global_market_position_2010_2024.csv",
  "data_clean/germany_global_position_2010_2024.csv",
  "data_clean/global_share_validation.csv",
  "data_clean/global_rank_validation.csv",
  "data_clean/reporting_count_validation.csv"
)
############################
# Check whether every file exists 
##################################
global_position_file_check <- data.frame(
  file = global_position_files,
  exists = file.exists(global_position_files)
)

global_position_file_check

# Confirm that no file is missing 
all(global_position_file_check$exists)
# CHecking file size 
global_position_file_check$file_size_bytes <- file.info(
  global_position_files
)$size

global_position_file_check

# Checking for empty files 
global_position_file_check %>%
  filter(
    is.na(file_size_bytes) |
      file_size_bytes <= 0
  )

all(global_position_file_check$exists)