# Load packages and raw data 
####################################################
# Global Data Cleaning
# Products: HS30, HS84, HS85, HS87
# Period: 2010–2024
####################################################

library(dplyr)
library(readr)

global_history_raw <- read_csv(
  "data_raw/global_exports_hs30_84_85_87_2010_2024_raw.csv",
  show_col_types = FALSE
)
dim(global_history_raw)
range(
  global_history_raw$ref_year,
  na.rm = TRUE
)
#################################################
# Creating the clean globla master table 
################################################
####################################################
# Create clean global trade master table
####################################################

global_trade_master <- global_history_raw %>%
  filter(
    flow_code == "X",
    partner_code == 0,
    aggr_level == 2,
    !is.na(ref_year),
    !is.na(reporter_code),
    !is.na(cmd_code),
    !is.na(primary_value)
  ) %>%
  transmute(
    year = as.integer(ref_year),
    
    country_code = as.integer(reporter_code),
    
    country_iso = as.character(reporter_iso),
    
    country = as.character(reporter_desc),
    
    hs2_code = sprintf(
      "%02d",
      as.integer(cmd_code)
    ),
    
    source_product_description =
      as.character(cmd_desc),
    
    product_group = case_when(
      as.character(cmd_code) == "30" ~
        "Pharmaceutical products",
      
      as.character(cmd_code) == "84" ~
        "Machinery and mechanical appliances",
      
      as.character(cmd_code) == "85" ~
        "Electrical machinery and equipment",
      
      as.character(cmd_code) == "87" ~
        "Vehicles and parts",
      
      TRUE ~ NA_character_
    ),
    
    export_value_usd =
      as.numeric(primary_value),
    
    source_classification =
      as.character(classification_code),
    
    original_classification =
      as.logical(is_original_classification)
  ) %>%
  filter(
    hs2_code %in% c(
      "30",
      "84",
      "85",
      "87"
    )
  ) %>%
  arrange(
    year,
    hs2_code,
    country
  )
# Inspecting the dataset 
dim(global_trade_master)
head(global_trade_master)
names(global_trade_master)
#######################################
# Validating this dataset 
#####################################
# Validation - Year 
sort(
  unique(global_trade_master$year)
)
n_distinct(global_trade_master$year)
# Validation - HS2 codes 
sort(
  unique(global_trade_master$hs2_code)
)
# Checking the canonical product names 
global_trade_master %>%
  distinct(
    hs2_code,
    product_group
  ) %>%
  arrange(hs2_code)
# Validation- Business keys 
global_master_duplicates <- global_trade_master %>%
  count(
    year,
    country_code,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)
nrow(global_master_duplicates)
# Checking missing critical values 
global_master_missing <- global_trade_master %>%
  summarise(
    total_rows = n(),
    
    missing_year =
      sum(is.na(year)),
    
    missing_country_code =
      sum(is.na(country_code)),
    
    missing_country_name =
      sum(is.na(country)),
    
    missing_hs2_code =
      sum(is.na(hs2_code)),
    
    missing_product_group =
      sum(is.na(product_group)),
    
    missing_export_value =
      sum(is.na(export_value_usd))
  )
# Check negative and zero export views 
global_value_validation <- global_trade_master %>%
  summarise(
    total_rows = n(),
    
    negative_export_values =
      sum(export_value_usd < 0, na.rm = TRUE),
    
    zero_export_values =
      sum(export_value_usd == 0, na.rm = TRUE),
    
    positive_export_values =
      sum(export_value_usd > 0, na.rm = TRUE)
  )
global_value_validation
# Validation - Germany's coverage 
germany_global_master_coverage <- global_trade_master %>%
  filter(country_iso == "DEU") %>%
  count(
    year,
    name = "number_of_products"
  ) %>%
  arrange(year)
germany_global_master_coverage
# Checking incomplete Germany years 
germany_global_master_coverage %>%
  filter(number_of_products != 4)
# Validation - Global coverage
global_group_coverage <- global_trade_master %>%
  summarise(
    reporting_countries =
      n_distinct(country_code),
    
    number_of_records =
      n(),
    
    total_export_value_usd =
      sum(export_value_usd, na.rm = TRUE),
    
    .by = c(
      year,
      hs2_code,
      product_group
    )
  ) %>%
  arrange(
    year,
    hs2_code
  )
nrow(global_group_coverage)
head(
  global_group_coverage,
  20
)
#################################################
# Save the clean global master dataset 
############################################
write_csv(
  global_trade_master,
  "data_clean/global_trade_master_2010_2024.csv"
)
# Saving it's validation outputs
write_csv(
  global_master_duplicates,
  "data_clean/global_master_duplicate_check.csv"
)

write_csv(
  global_master_missing,
  "data_clean/global_master_missing_check.csv"
)

write_csv(
  global_value_validation,
  "data_clean/global_master_value_validation.csv"
)

write_csv(
  germany_global_master_coverage,
  "data_clean/germany_global_master_coverage.csv"
)
write_csv(
  global_group_coverage,
  "data_clean/global_group_coverage.csv"
)
# Verifying all outputs 
file.exists(
  c(
    "data_clean/global_trade_master_2010_2024.csv",
    "data_clean/global_master_duplicate_check.csv",
    "data_clean/global_master_missing_check.csv",
    "data_clean/global_master_value_validation.csv",
    "data_clean/germany_global_master_coverage.csv",
    "data_clean/global_group_coverage.csv"
  )
)

nrow(global_master_duplicates)
##########################################
# Final validation of the global master dataset 
###############################################
dim(global_master_duplicates)
names(global_master_duplicates)
dplyr::glimpse(global_master_duplicates)
# Loading object 
global_master_missing_check <- readr::read_csv(
  "data_clean/global_master_missing_check.csv",
  show_col_types = FALSE
)

print(global_master_missing_check, n = Inf)
# Validation- Export values 
global_master_value_validation <- readr::read_csv(
  "data_clean/global_master_value_validation.csv",
  show_col_types = FALSE
)

print(global_master_value_validation, n = Inf)

# Check Germany's coverage 
germany_global_master_coverage <- readr::read_csv(
  "data_clean/germany_global_master_coverage.csv",
  show_col_types = FALSE
)

print(germany_global_master_coverage, n = Inf)

# Confirming directly 
global_trade_master %>%
  filter(country_iso == "DEU") %>%
  summarise(
    rows = n(),
    years = n_distinct(year),
    products = n_distinct(hs2_code),
    minimum_year = min(year),
    maximum_year = max(year)
  )
# Validation - Global coverages 
global_group_coverage <- readr::read_csv(
  "data_clean/global_group_coverage.csv",
  show_col_types = FALSE
)

############################################
# Summarize reporting-country coverage
############################################

global_group_coverage %>%
  summarise(
    minimum_reporting_countries =
      min(reporting_countries, na.rm = TRUE),
    
    maximum_reporting_countries =
      max(reporting_countries, na.rm = TRUE),
    
    average_reporting_countries =
      mean(reporting_countries, na.rm = TRUE),
    
    .by = hs2_code
  )
############################################
# Inspect reporting coverage by year/product
############################################

global_group_coverage %>%
  select(
    year,
    hs2_code,
    reporting_countries,
    number_of_records
  ) %>%
  arrange(
    year,
    hs2_code
  ) %>%
  View()
# Check invalid export totals 
############################################
# Validate global export totals
############################################

invalid_global_totals <- global_group_coverage %>%
  filter(
    is.na(total_export_value_usd) |
      total_export_value_usd <= 0
  )

invalid_global_totals
# Checking missing values 
############################################
# Check missing values
############################################

colSums(
  is.na(global_group_coverage)
)
############################################
# Save validated global group coverage
############################################

readr::write_csv(
  global_group_coverage,
  "data_clean/global_group_coverage.csv"
)
# Verifying 
file.exists(
  "data_clean/global_group_coverage.csv"
)
# Verifying the dimensions after reading it back 
global_group_coverage_check <- readr::read_csv(
  "data_clean/global_group_coverage.csv",
  show_col_types = FALSE
)

dim(global_group_coverage_check)
# Verifyinng existance 
list.files("data_clean")
# Check the corrected table's columns
names(global_group_coverage_check)
dplyr::glimpse(global_group_coverage_check)
# Inspect all 60 coverage records 
global_group_coverage_check %>%
  arrange(
    hs2_code,
    year
  ) %>%
  print(n = 60)
# confirm uniqueness 
global_group_coverage_check %>%
  count(
    year,
    hs2_code
  ) %>%
  filter(n != 1)
############################################
# Step 4: Summarize reporting-country coverage
############################################

reporting_country_summary <- global_group_coverage_check %>%
  summarise(
    minimum_reporting_countries =
      min(reporting_countries, na.rm = TRUE),
    
    maximum_reporting_countries =
      max(reporting_countries, na.rm = TRUE),
    
    average_reporting_countries =
      mean(reporting_countries, na.rm = TRUE),
    
    .by = hs2_code
  )
reporting_country_summary
# Display all coverage- summary
print(
  reporting_country_summary,
  width = Inf
)
# Validatation- Summary
dim(reporting_country_summary)

# Missing values 
colSums(
  is.na(reporting_country_summary)
)
############################################
# Step 5: Save reporting-country summary
############################################

readr::write_csv(
  reporting_country_summary,
  "data_clean/reporting_country_summary.csv"
)
file.exists(
  "data_clean/reporting_country_summary.csv"
)
############################################
# Step 6: Save validated global group coverage
############################################

readr::write_csv(
  global_group_coverage_check,
  "data_clean/global_group_coverage.csv"
)
file.exists(
  "data_clean/global_group_coverage.csv"
)
