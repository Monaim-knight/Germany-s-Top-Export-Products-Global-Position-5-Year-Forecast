############################################################
# Germany Export Forecast Project
# Phase 6: MySQL Database Preparation and Data Loading
############################################################

# Purpose:
# Connect the validated R analytical pipeline to MySQL
# and load the project datasets into the analytical database.

############################################
# Step 14: Check required MySQL packages
############################################

required_mysql_packages <- c(
  "DBI",
  "RMariaDB"
)

mysql_package_check <- data.frame(
  package = required_mysql_packages,
  installed = required_mysql_packages %in%
    rownames(installed.packages())
)

mysql_package_check

############################################
# Step 16: Load database packages
############################################

library(DBI)
library(RMariaDB)

############################################
# Connect R to MySQL
############################################

mysql_connection <- DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "germany_export_analytics",
  host = "localhost",
  port = 3306,
  username = "root",
  password = rstudioapi::askForPassword(
    "070707"
  )
)

DBI::dbIsValid(
  mysql_connection
)

active_database <- DBI::dbGetQuery(
  mysql_connection,
  "SELECT DATABASE() AS active_database;"
)

active_database

##### Load the validated CSV files into R 
############################################
# Step 17: Load validated CSV datasets
############################################

trade_master_db <- readr::read_csv(
  "data_clean/trade_master.csv",
  show_col_types = FALSE
)

sector_lookup_db <- readr::read_csv(
  "data_clean/sector_lookup.csv",
  show_col_types = FALSE
)

comparison_share_db <- readr::read_csv(
  "data_clean/comparison_share.csv",
  show_col_types = FALSE
)

competitor_rankings_db <- readr::read_csv(
  "data_clean/competitor_rankings.csv",
  show_col_types = FALSE
)

global_trade_master_db <- readr::read_csv(
  "data_clean/global_trade_master.csv",
  show_col_types = FALSE
)

global_market_position_db <- readr::read_csv(
  "data_clean/global_market_position_2010_2024.csv",
  show_col_types = FALSE
)

germany_global_position_db <- readr::read_csv(
  "data_clean/germany_global_position_2010_2024.csv",
  show_col_types = FALSE
)

# CHeck that all objects exist 
############################################
# Confirm datasets were loaded
############################################

mysql_dataset_check <- data.frame(
  dataset = c(
    "trade_master_db",
    "sector_lookup_db",
    "comparison_share_db",
    "competitor_rankings_db",
    "global_trade_master_db",
    "global_market_position_db",
    "germany_global_position_db"
  ),
  exists = c(
    exists("trade_master_db"),
    exists("sector_lookup_db"),
    exists("comparison_share_db"),
    exists("competitor_rankings_db"),
    exists("global_trade_master_db"),
    exists("global_market_position_db"),
    exists("germany_global_position_db")
  ),
  rows = c(
    nrow(trade_master_db),
    nrow(sector_lookup_db),
    nrow(comparison_share_db),
    nrow(competitor_rankings_db),
    nrow(global_trade_master_db),
    nrow(global_market_position_db),
    nrow(germany_global_position_db)
  )
)

mysql_dataset_check

mysql_dataset_check %>%
  filter(
    !exists |
      rows <= 0
  )
##############################
# Inspecting the lookup dataset 
################################
dim(sector_lookup_db)
sector_lookup_db
###################################
############################################
# Step 18.2: Load sector_lookup into MySQL
############################################

sector_lookup_write <- DBI::dbWriteTable(
  conn = mysql_connection,
  name = "sector_lookup",
  value = sector_lookup_db,
  overwrite = TRUE,
  row.names = FALSE
)

sector_lookup_write
#### Confirming the table exists in MySQL
DBI::dbExistsTable(
  mysql_connection,
  "sector_lookup"
)
#Verifying row count 
sector_lookup_mysql_count <- DBI::dbGetQuery(
  mysql_connection,
  "
  SELECT COUNT(*) AS row_count
  FROM sector_lookup;
  "
)

sector_lookup_mysql_count
# Matching check 
sector_lookup_mysql_count$row_count ==
  nrow(sector_lookup_db)
# Inspection all look up records 
############################################
# Step 18.6A: Inspect sector lookup records
############################################

sector_lookup_db %>%
  arrange(hs2_code)
###################################
# Creating the corrected sector lookup 
############################################
# Step 18.7: Create standardized sector lookup
############################################

sector_lookup_corrected <- tibble::tibble(
  hs2_code = c(30, 84, 85, 87),
  
  product_group = c(
    "Pharmaceutical products",
    "Machinery and mechanical appliances",
    "Electrical machinery and equipment",
    "Vehicles and parts"
  )
)
sector_lookup_corrected
dim(sector_lookup_corrected)
dplyr::n_distinct(
  sector_lookup_corrected$hs2_code
)
# Checking for duplicate HS codes 
sector_lookup_corrected %>%
  count(
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)
######################################
# Replacing the old CSV with the corrected lookup 
###############################
############################################
# Step 18.9: Save corrected sector lookup
############################################

readr::write_csv(
  sector_lookup_corrected,
  "data_clean/sector_lookup.csv"
)
# Verifying 
file.exists(
  "data_clean/sector_lookup.csv"
)
# Reading and validating 
sector_lookup_check <- readr::read_csv(
  "data_clean/sector_lookup.csv",
  show_col_types = FALSE
)
dim(sector_lookup_check)
dplyr::n_distinct(
  sector_lookup_check$hs2_code
)
#############################################
# Overwritiing the sector_lookup in MySQL
##############################################
############################################
# Step 18.11: Replace sector_lookup in MySQL
############################################

sector_lookup_write <- DBI::dbWriteTable(
  conn = mysql_connection,
  name = "sector_lookup",
  value = sector_lookup_check,
  overwrite = TRUE,
  row.names = FALSE
)

sector_lookup_write
# Verifying MySQL row count 
sector_lookup_mysql_count <- DBI::dbGetQuery(
  mysql_connection,
  "
  SELECT COUNT(*) AS row_count
  FROM sector_lookup;
  "
)

sector_lookup_mysql_count
# Verifying the actual records 
sector_lookup_mysql <- DBI::dbGetQuery(
  mysql_connection,
  "
  SELECT
      hs2_code,
      product_group
  FROM sector_lookup
  ORDER BY hs2_code;
  "
)

sector_lookup_mysql
nrow(sector_lookup_mysql) ==
  nrow(sector_lookup_check)
dplyr::n_distinct(
  sector_lookup_mysql$hs2_code
)
#########################################
# Reconnecting R to MySQL 
#########################################
library(DBI)
library(RMariaDB)

mysql_con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost",
  port = 3306,
  dbname = "germany_export_analytics",
  username = "root",
  password = rstudioapi::askForPassword(
    "Enter MySQL password"
  )
)
# Verifying 
dbIsValid(mysql_con)
######################################################
# Preparing the global table before loading 
################################################
global_market_position_mysql <-
  global_market_position %>%
  mutate(
    year = as.integer(year),
    
    country_code = as.integer(country_code),
    
    country_iso3 = as.character(country_iso3),
    
    country = as.character(country),
    
    hs2_code = as.character(hs2_code),
    
    product_group = as.character(product_group),
    
    export_value_usd =
      as.numeric(export_value_usd),
    
    global_total_exports_usd =
      as.numeric(global_total_exports_usd),
    
    global_market_share =
      as.numeric(global_market_share),
    
    global_rank =
      as.integer(global_rank),
    
    reporting_country_count =
      as.integer(reporting_country_count)
  )
# cHECK 
glimpse(global_market_position_mysql)
nrow(global_market_position_mysql)
######################################
# Create the MySQL table explicitly 
##########################################
dbExecute(
  mysql_con,
  "
  CREATE TABLE IF NOT EXISTS global_market_position (
      year SMALLINT NOT NULL,
      country_code INT,
      country_iso3 VARCHAR(3),
      country VARCHAR(150) NOT NULL,
      hs2_code VARCHAR(2) NOT NULL,
      product_group VARCHAR(500),
      export_value_usd DECIMAL(20,2),
      global_total_exports_usd DECIMAL(20,2),
      global_market_share DECIMAL(18,10),
      global_rank INT,
      reporting_country_count INT,

      PRIMARY KEY (year, country_code, hs2_code),

      INDEX idx_country_iso3 (country_iso3),
      INDEX idx_hs2_code (hs2_code),
      INDEX idx_global_rank (global_rank),
      INDEX idx_year_hs2 (year, hs2_code)
  );
  "
)
# Verifying table exists 

dbListTables(mysql_con)
dbListFields(
  mysql_con,
  "global_market_position"
)
# Loading 9935 reords 
dbWriteTable(
  mysql_con,
  "global_market_position",
  global_market_position_mysql,
  append = TRUE,
  row.names = FALSE
)
# Checking the extra R columns 
setdiff(
  names(global_market_position_mysql),
  names(global_market_position_db)
)
# Checking whether R has columns missing from MySQL 
setdiff(
  names(global_market_position_db),
  names(global_market_position_mysql)
)
# Checking columns order 
identical(
  names(global_market_position_db),
  names(global_market_position_mysql)
)
# Compare ROW counts 
############################################
# Check R vs MySQL row counts
############################################

nrow(global_market_position_db)
nrow(global_market_position_mysql)
nrow(global_market_position_db) ==
  nrow(global_market_position_mysql)
####################################
# Check key uniqueness 
############################################
# Check duplicate business keys in R
############################################

global_market_position_r_duplicates <-
  global_market_position_db %>%
  count(
    year,
    country_code,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)

nrow(global_market_position_r_duplicates)
# MySQL read dataset 
############################################
# Check duplicate business keys in MySQL data
############################################

global_market_position_mysql_duplicates <-
  global_market_position_mysql %>%
  count(
    year,
    country_code,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)

nrow(global_market_position_mysql_duplicates)

nrow(global_market_position_r_duplicates) ==
  nrow(global_market_position_mysql_duplicates)
## Validation - Critical fields in the MySQL-loaded tables 
############################################
# Validate critical fields after MySQL load
############################################

global_market_position_mysql_missing <-
  global_market_position_mysql %>%
  summarise(
    missing_year =
      sum(is.na(year)),
    
    missing_country_code =
      sum(is.na(country_code)),
    
    missing_country =
      sum(is.na(country)),
    
    missing_hs2_code =
      sum(is.na(hs2_code)),
    
    missing_export_value =
      sum(is.na(export_value_usd)),
    
    missing_global_share =
      sum(is.na(global_market_share)),
    
    missing_global_rank =
      sum(is.na(global_rank))
  )
print(
  global_market_position_mysql_missing,
  width = Inf
)
##########################################
# Validation- Market_share 
#########################################
############################################
# Validate global market-share totals
# after MySQL loading
############################################

mysql_global_share_validation <-
  global_market_position_mysql %>%
  summarise(
    total_market_share =
      sum(global_market_share, na.rm = TRUE),
    
    .by = c(
      year,
      hs2_code
    )
  )
mysql_invalid_share_groups <-
  mysql_global_share_validation %>%
  filter(
    abs(total_market_share - 1) > 0.000001
  )
nrow(mysql_invalid_share_groups)
####################################
# Validation - ranks and row counts 
#####################################
mysql_row_count <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT COUNT(*) AS row_count
  FROM global_market_position;
  "
)

mysql_row_count
####################################
# This is the problem ; so now we will solve this problem.
####################################
# Rebuilding the MySQL ready dataframe 
library(dplyr)
library(DBI)
library(RMariaDB)

global_market_position_mysql <- global_market_position %>%
  transmute(
    year = as.integer(year),
    country_code = as.integer(country_code),
    country_iso3 = as.character(country_iso3),
    country = as.character(country),
    hs2_code = sprintf("%02d", as.integer(hs2_code)),
    product_group = as.character(product_group),
    export_value_usd = as.numeric(export_value_usd),
    global_total_exports_usd =
      as.numeric(global_total_exports_usd),
    global_market_share =
      as.numeric(global_market_share),
    global_rank = as.integer(global_rank),
    reporting_country_count =
      as.integer(reporting_country_count)
  )
dim(global_market_position_mysql)
# Confirming exact column alignment 
mysql_fields <- DBI::dbListFields(
  mysql_con,
  "global_market_position"
)

mysql_fields
# Checking the similiarities 
identical(
  names(global_market_position_mysql),
  mysql_fields
)
##########################################
# Validation- required values before upload 
###############################################
global_market_position_mysql %>%
  summarise(
    missing_year =
      sum(is.na(year)),
    
    missing_country_code =
      sum(is.na(country_code)),
    
    missing_country =
      sum(is.na(country)),
    
    missing_hs2_code =
      sum(is.na(hs2_code))
  )
# Ensuring a clean load by checking the empty table 
DBI::dbExecute(
  mysql_con,
  "TRUNCATE TABLE global_market_position;"
)
######################################################
# Uploading the dataframe 
##################################################
DBI::dbWithTransaction(
  mysql_con,
  {
    DBI::dbWriteTable(
      mysql_con,
      "global_market_position",
      global_market_position_mysql,
      append = TRUE,
      row.names = FALSE
    )
  }
)

# CHecking the MySQL row count 
mysql_row_count <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT COUNT(*) AS row_count
  FROM global_market_position;
  "
)

mysql_row_count
## Compare automatically 
mysql_row_count$row_count ==
  nrow(global_market_position_mysql)
# Inspect a few loaded records 
DBI::dbGetQuery(
  mysql_con,
  "
  SELECT *
  FROM global_market_position
  ORDER BY year, hs2_code, global_rank
  LIMIT 10;
  "
)
# Inspect Germany's record 
DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
      year,
      country,
      hs2_code,
      export_value_usd,
      global_market_share,
      global_rank
  FROM global_market_position
  WHERE country_iso3 = 'DEU'
    AND year = 2024
  ORDER BY global_rank;
  "
)
#############################################
# Validation - MySQL
##################################################
# Duplicate check 
mysql_duplicate_records <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
      year,
      country_code,
      hs2_code,
      COUNT(*) AS record_count
  FROM global_market_position
  GROUP BY
      year,
      country_code,
      hs2_code
  HAVING COUNT(*) > 1;
  "
)

nrow(mysql_duplicate_records)
# Validation - Rank 
mysql_invalid_rank_groups <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
      year,
      hs2_code,
      MIN(global_rank) AS minimum_rank,
      MAX(global_rank) AS maximum_rank,
      MAX(reporting_country_count) AS reporting_country_count
  FROM global_market_position
  GROUP BY
      year,
      hs2_code
  HAVING
      MIN(global_rank) <> 1
      OR MAX(global_rank) > MAX(reporting_country_count);
  "
)
# Germany's coverage 
mysql_germany_coverage <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
      COUNT(*) AS record_count,
      COUNT(DISTINCT year) AS year_count,
      COUNT(DISTINCT hs2_code) AS product_count,
      MIN(year) AS minimum_year,
      MAX(year) AS maximum_year
  FROM global_market_position
  WHERE country_iso3 = 'DEU';
  "
)

mysql_germany_coverage

nrow(mysql_invalid_rank_groups)


############################################
# Validation - Global market share totals
# directly from MySQL
############################################

mysql_invalid_share_groups <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
      year,
      hs2_code,
      SUM(global_market_share) AS total_market_share
  FROM global_market_position
  GROUP BY
      year,
      hs2_code
  HAVING
      ABS(SUM(global_market_share) - 1.0) > 0.000001;
  "
)
nrow(mysql_invalid_share_groups)
############################################
# Validation - Germany 2024 market position
# directly from MySQL
############################################

mysql_germany_2024 <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
      year,
      hs2_code,
      product_group,
      export_value_usd,
      global_market_share,
      global_rank,
      reporting_country_count
  FROM global_market_position
  WHERE country_iso3 = 'DEU'
    AND year = 2024
  ORDER BY global_rank;
  "
)
mysql_germany_2024
nrow(mysql_germany_2024)
sort(
  mysql_germany_2024$hs2_code
)
############################################################
# Phase 14 - Step 1
# Identify existing database connection objects
############################################################

phase14_db_connection_objects <- ls(
  envir = .GlobalEnv
)[
  grepl(
    "con|connection|mysql|db",
    ls(envir = .GlobalEnv),
    ignore.case = TRUE
  )
]

phase14_db_connection_objects

############################################################
# Phase 14 - Step 2
# Identify active MySQL connection object
############################################################

phase14_connection_check <- tibble::tibble(
  connection_object = c(
    "mysql_con",
    "mysql_connection"
  ),
  
  object_class = c(
    paste(class(mysql_con), collapse = ", "),
    paste(class(mysql_connection), collapse = ", ")
  ),
  
  is_dbi_connection = c(
    DBI::dbIsValid(mysql_con),
    DBI::dbIsValid(mysql_connection)
  )
)

phase14_connection_check

############################################################
# Phase 14 - Step 3
# Confirm target database and inspect existing tables
############################################################

phase14_database_name <- DBI::dbGetQuery(
  mysql_con,
  "SELECT DATABASE() AS database_name"
)

phase14_existing_tables <- DBI::dbListTables(
  mysql_con
)

phase14_database_name

phase14_existing_tables


############################################################
# Phase 14 - Step 3A
# Inspect the original MySQL connection call
############################################################

phase14_mysql_connection_call <- mysql_connection@call

phase14_mysql_connection_call

############################################################
# Phase 14 - Step 3B
# Retrieve safe metadata from existing MySQL connections
############################################################

phase14_mysql_con_info <- tryCatch(
  DBI::dbGetInfo(mysql_con),
  error = function(e) {
    list(error = conditionMessage(e))
  }
)

phase14_mysql_connection_info <- tryCatch(
  DBI::dbGetInfo(mysql_connection),
  error = function(e) {
    list(error = conditionMessage(e))
  }
)

phase14_mysql_con_info

phase14_mysql_connection_info


############################################################
# Phase 14 - Step 3C
# Reconnect securely to the established MySQL database
############################################################

phase14_mysql_password <- rstudioapi::askForPassword(
  "070707"
)

mysql_con <- DBI::dbConnect(
  RMySQL::MySQL(),
  host = phase14_mysql_con_info$host,
  port = phase14_mysql_con_info$port,
  user = phase14_mysql_con_info$username,
  password = phase14_mysql_password,
  dbname = phase14_mysql_con_info$dbname
)

# Keep the existing second connection name synchronized
mysql_connection <- mysql_con

# Remove password from the R environment immediately
rm(phase14_mysql_password)

############################################################
# Test the new connection with an actual query
############################################################

phase14_reconnection_test <- DBI::dbGetQuery(
  mysql_con,
  "SELECT DATABASE() AS database_name, 1 AS connection_test"
)

phase14_reconnection_test

############################################################
# Phase 14 - Step 3D
# Reconnect using the modern RMariaDB driver
############################################################

# Disconnect old connection objects if they are still valid
if (
  exists("mysql_con") &&
  inherits(
    try(DBI::dbIsValid(mysql_con), silent = TRUE),
    "logical"
  ) &&
  isTRUE(DBI::dbIsValid(mysql_con))
) {
  DBI::dbDisconnect(mysql_con)
}

# Install RMariaDB only if it is not already installed
if (!requireNamespace("RMariaDB", quietly = TRUE)) {
  install.packages("RMariaDB")
}

phase14_mysql_password <- rstudioapi::askForPassword(
  "070707"
)

mysql_con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  host = phase14_mysql_con_info$host,
  port = as.integer(phase14_mysql_con_info$port),
  user = phase14_mysql_con_info$username,
  password = phase14_mysql_password,
  dbname = phase14_mysql_con_info$dbname
)

mysql_connection <- mysql_con

rm(phase14_mysql_password)

phase14_reconnection_test <- DBI::dbGetQuery(
  mysql_con,
  "SELECT DATABASE() AS database_name, 1 AS connection_test"
)

phase14_reconnection_test

DBI::dbIsValid(mysql_con)

############################################################
# Phase 14 - Step 3E
# Confirm reconnected database and inspect existing tables
############################################################

phase14_connection_status <- tryCatch(
  {
    test_result <- DBI::dbGetQuery(
      mysql_con,
      "SELECT DATABASE() AS database_name, 1 AS connection_test"
    )
    
    tibble::tibble(
      connected = TRUE,
      database_name = test_result$database_name,
      connection_test = test_result$connection_test,
      error_message = NA_character_
    )
  },
  error = function(e) {
    tibble::tibble(
      connected = FALSE,
      database_name = NA_character_,
      connection_test = NA_integer_,
      error_message = conditionMessage(e)
    )
  }
)

phase14_existing_tables <- DBI::dbListTables(
  mysql_con
)

phase14_connection_status

phase14_existing_tables

############################################################
# Phase 14 - Step 4
# Validate active connection and required existing tables
############################################################

phase14_required_existing_tables <- c(
  "global_market_position",
  "sector_lookup"
)

phase14_database_valid <- all(
  isTRUE(phase14_connection_status$connected),
  phase14_connection_status$connection_test == 1,
  is.na(phase14_connection_status$error_message),
  all(
    phase14_required_existing_tables %in%
      phase14_existing_tables
  )
)

phase14_database_valid

############################################################
# Phase 14 - Step 4
# Check existing MySQL tables and target-name conflicts
############################################################

phase14_existing_tables <- DBI::dbListTables(
  mysql_con
)

phase14_target_tables <- c(
  "germany_export_forecasts_2025_2029",
  "competitor_position_projection_2025_2029",
  "germany_competitive_position_2025_2029",
  "germany_position_summary_2025_2029"
)

phase14_target_table_check <- tibble::tibble(
  target_table = phase14_target_tables,
  
  already_exists = phase14_target_tables %in%
    phase14_existing_tables
)

phase14_target_table_check

############################################################
# Phase 14 - Step 5
# Write Germany export forecasts to MySQL
############################################################

DBI::dbWriteTable(
  mysql_con,
  "germany_export_forecasts_2025_2029",
  phase11_final_forecasts,
  row.names = FALSE,
  overwrite = FALSE
)

############################################################
# Validate table creation
############################################################

phase14_germany_forecast_table_exists <- DBI::dbExistsTable(
  mysql_con,
  "germany_export_forecasts_2025_2029"
)

phase14_germany_forecast_row_count <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT COUNT(*) AS row_count
  FROM germany_export_forecasts_2025_2029
  "
)

phase14_germany_forecast_table_exists

phase14_germany_forecast_row_count

############################################################
# Phase 14 - Step 5A
# Reconnect to MySQL using RMariaDB
############################################################

library(DBI)
library(RMariaDB)

mysql_con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost",
  port = 3306,
  user = "root",
  password = "070707",
  dbname = "germany_export_analytics"
)

############################################################
# Phase 14 - Step 5B
# Validate repaired MySQL connection
############################################################

phase14_connection_check <- tibble::tibble(
  validation = c(
    "Connection is valid",
    "Connection uses RMariaDB"
  ),
  passed = c(
    DBI::dbIsValid(mysql_con),
    inherits(mysql_con, "MariaDBConnection")
  )
)

phase14_connection_check


############################################################
# Phase 14 - Step 5B
# Test clean RMariaDB connection
############################################################

mysql_con <- DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = 3306L,
  user = "root",
  password = "070707",
  dbname = "germany_export_analytics"
)

class(mysql_con)

DBI::dbIsValid(mysql_con)

############################################################
# Phase 14 - Step 5B
# Connect explicitly to Oracle MySQL
############################################################

mysql_con <- DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = 3306L,
  user = "root",
  password = "070707",
  dbname = "germany_export_analytics",
  mysql = TRUE
)



############################################################
# Phase 14 - Step 5B
# Create clean RMariaDB connection
############################################################

mysql_con <- DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = 3306L,
  user = "root",
  password = "070707",
  dbname = "germany_export_analytics",
  mysql = TRUE
)
class(mysql_con)

DBI::dbIsValid(mysql_con)

DBI::dbGetQuery(
  mysql_con,
  "SELECT VERSION() AS server_version"
)

############################################################
# Phase 14 - Step 5C
# Write Germany export forecasts to MySQL
############################################################

DBI::dbWriteTable(
  conn = mysql_con,
  name = "germany_export_forecasts_2025_2029",
  value = as.data.frame(phase11_final_forecasts),
  row.names = FALSE,
  overwrite = FALSE
)
############################################################
# Phase 14 - Restore project objects
############################################################

load("germany_export_project_backup.RData")

exists("phase11_final_forecasts")

ls(pattern = "phase11|forecast")

############################################################
# Phase 14 - Locate saved forecast files
############################################################

forecast_files <- list.files(
  path = ".",
  pattern = "forecast|2025_2029",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

forecast_files

############################################################
# Phase 14 - Restore Phase 11 final forecast object
############################################################

phase11_final_forecasts <- readr::read_csv(
  "data_clean/germany_export_forecasts_2025_2029.csv",
  show_col_types = FALSE
)

dim(phase11_final_forecasts)

names(phase11_final_forecasts)

head(phase11_final_forecasts)

############################################################
# Phase 14 - Step 5C
# Write Germany export forecasts to MySQL
############################################################

DBI::dbWriteTable(
  conn = mysql_con,
  name = "germany_export_forecasts_2025_2029",
  value = as.data.frame(phase11_final_forecasts),
  row.names = FALSE,
  overwrite = FALSE
)

############################################################
# Phase 14 - Step 5D
# Verify Germany forecast table in MySQL
############################################################

DBI::dbExistsTable(
  mysql_con,
  "germany_export_forecasts_2025_2029"
)

DBI::dbGetQuery(
  mysql_con,
  "SELECT COUNT(*) AS row_count
   FROM germany_export_forecasts_2025_2029"
)

############################################################
# Phase 14 - Step 6A
# Restore competitor position projection dataset
############################################################

phase12_competitor_projection <- readr::read_csv(
  "data_clean/competitor_position_projection_2025_2029.csv",
  show_col_types = FALSE
)

dim(phase12_competitor_projection)

names(phase12_competitor_projection)

head(phase12_competitor_projection)

############################################################
# Phase 14 - Step 6B
# Write competitor position projection to MySQL
############################################################

DBI::dbWriteTable(
  conn = mysql_con,
  name = "competitor_position_projection_2025_2029",
  value = as.data.frame(phase12_competitor_projection),
  row.names = FALSE,
  overwrite = FALSE
)
############################################################
# Phase 14 - Step 6C
# Verify competitor projection table in MySQL
############################################################

DBI::dbExistsTable(
  mysql_con,
  "competitor_position_projection_2025_2029"
)

DBI::dbGetQuery(
  mysql_con,
  "SELECT COUNT(*) AS row_count
   FROM competitor_position_projection_2025_2029"
)

############################################################
# Phase 14 - Step 7A
# Restore Germany competitive position dataset
############################################################

phase13_germany_competitive_position <- readr::read_csv(
  "data_clean/germany_competitive_position_2025_2029.csv",
  show_col_types = FALSE
)

dim(phase13_germany_competitive_position)

names(phase13_germany_competitive_position)

head(phase13_germany_competitive_position)

############################################################
# Phase 14 - Step 7B
# Write Germany competitive position to MySQL
############################################################

DBI::dbWriteTable(
  conn = mysql_con,
  name = "germany_competitive_position_2025_2029",
  value = as.data.frame(phase13_germany_competitive_position),
  row.names = FALSE,
  overwrite = FALSE
)

############################################################
# Phase 14 - Step 7C
# Verify Germany competitive position table
############################################################

DBI::dbExistsTable(
  mysql_con,
  "germany_competitive_position_2025_2029"
)

DBI::dbGetQuery(
  mysql_con,
  "SELECT COUNT(*) AS row_count
   FROM germany_competitive_position_2025_2029"
)

############################################################
# Phase 14 - Step 8A
# Restore Germany position summary dataset
############################################################

phase13_germany_position_summary <- readr::read_csv(
  "data_clean/germany_position_summary_2025_2029.csv",
  show_col_types = FALSE
)

dim(phase13_germany_position_summary)

names(phase13_germany_position_summary)

head(phase13_germany_position_summary)

############################################################
# Phase 14 - Step 8B
# Write Germany position summary to MySQL
############################################################

DBI::dbWriteTable(
  conn = mysql_con,
  name = "germany_position_summary_2025_2029",
  value = as.data.frame(phase13_germany_position_summary),
  row.names = FALSE,
  overwrite = FALSE
)
############################################################
# Phase 14 - Step 8C
# Verify Germany position summary table
############################################################

DBI::dbExistsTable(
  mysql_con,
  "germany_position_summary_2025_2029"
)

DBI::dbGetQuery(
  mysql_con,
  "SELECT COUNT(*) AS row_count
   FROM germany_position_summary_2025_2029"
)

############################################################
# Phase 14 - Step 9
# Final validation of all forecast tables in MySQL
############################################################

phase14_final_validation <- tibble::tibble(
  table_name = c(
    "germany_export_forecasts_2025_2029",
    "competitor_position_projection_2025_2029",
    "germany_competitive_position_2025_2029",
    "germany_position_summary_2025_2029"
  ),
  expected_rows = c(
    20,
    120,
    20,
    4
  )
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    exists = DBI::dbExistsTable(
      mysql_con,
      table_name
    ),
    mysql_rows = if (exists) {
      DBI::dbGetQuery(
        mysql_con,
        paste0(
          "SELECT COUNT(*) AS row_count FROM `",
          table_name,
          "`"
        )
      )$row_count
    } else {
      NA_integer_
    },
    row_count_matches = (
      exists &&
        mysql_rows == expected_rows
    )
  ) |>
  dplyr::ungroup()

phase14_final_validation

############################################################
# Phase 14 - Step 10
# Final Phase 14 completion check
############################################################

phase14_complete <- all(
  phase14_final_validation$exists &
    phase14_final_validation$row_count_matches
)

phase14_complete

############################################################
# Phase 15 - Step 1
# Inspect final MySQL analytical tables
############################################################

phase15_mysql_tables <- DBI::dbListTables(mysql_con)

phase15_mysql_tables

############################################################
# Phase 15 - Step 2
# Inspect structure of all MySQL analytical tables
############################################################

phase15_table_structure <- lapply(
  phase15_mysql_tables,
  function(tbl) {
    fields <- DBI::dbListFields(mysql_con, tbl)
    
    tibble::tibble(
      table_name = tbl,
      column_name = fields
    )
  }
) |>
  dplyr::bind_rows()

phase15_table_structure

############################################################
# Phase 15 - Step 2B
# Display all MySQL table columns
############################################################

print(
  phase15_table_structure,
  n = Inf,
  width = Inf
)

############################################################
# Phase 15 - Step 3
# Validate analytical table grain and uniqueness
############################################################

phase15_grain_check <- dplyr::bind_rows(
  
  # Competitor forecast:
  # expected one row per country + HS2 + year
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'competitor_position_projection_2025_2029' AS table_name,
      COUNT(*) AS total_rows,
      COUNT(DISTINCT country, hs2_code, year) AS unique_key_rows
    FROM competitor_position_projection_2025_2029
    "
  ),
  
  # Germany competitive position:
  # expected one row per HS2 + year
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'germany_competitive_position_2025_2029' AS table_name,
      COUNT(*) AS total_rows,
      COUNT(DISTINCT hs2_code, year) AS unique_key_rows
    FROM germany_competitive_position_2025_2029
    "
  ),
  
  # Germany forecasts:
  # expected one row per HS2 + year
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'germany_export_forecasts_2025_2029' AS table_name,
      COUNT(*) AS total_rows,
      COUNT(DISTINCT hs2_code, year) AS unique_key_rows
    FROM germany_export_forecasts_2025_2029
    "
  ),
  
  # Germany summary:
  # expected one row per HS2
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'germany_position_summary_2025_2029' AS table_name,
      COUNT(*) AS total_rows,
      COUNT(DISTINCT hs2_code) AS unique_key_rows
    FROM germany_position_summary_2025_2029
    "
  ),
  
  # Historical global market:
  # expected one row per country + HS2 + year
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'global_market_position' AS table_name,
      COUNT(*) AS total_rows,
      COUNT(DISTINCT country, hs2_code, year) AS unique_key_rows
    FROM global_market_position
    "
  ),
  
  # Sector lookup:
  # expected one row per HS2
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'sector_lookup' AS table_name,
      COUNT(*) AS total_rows,
      COUNT(DISTINCT hs2_code) AS unique_key_rows
    FROM sector_lookup
    "
  )
) |>
  dplyr::mutate(
    grain_is_unique = total_rows == unique_key_rows
  )

############################################################
# Phase 15 - Step 3B
# Display grain validation safely
############################################################

phase15_grain_check

############################################################
# Phase 15 - Step 4
# Validate HS2 relationship keys across analytical tables
############################################################

phase15_hs2_key_check <- dplyr::bind_rows(
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT hs2_code
    FROM competitor_position_projection_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "competitor_position_projection_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT hs2_code
    FROM germany_competitive_position_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "germany_competitive_position_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT hs2_code
    FROM germany_export_forecasts_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "germany_export_forecasts_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT hs2_code
    FROM germany_position_summary_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "germany_position_summary_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT hs2_code
    FROM global_market_position
    "
  ) |>
    dplyr::mutate(
      table_name = "global_market_position"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT hs2_code
    FROM sector_lookup
    "
  ) |>
    dplyr::mutate(
      table_name = "sector_lookup"
    )
) |>
  dplyr::arrange(
    table_name,
    hs2_code
  ) |>
  dplyr::select(
    table_name,
    hs2_code
  )

phase15_hs2_key_check

############################################################
# Phase 15 - Step 4B
# Validate standardized HS2 keys across all tables
############################################################

phase15_hs2_key_check <- dplyr::bind_rows(
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT CAST(hs2_code AS CHAR) AS hs2_code
    FROM competitor_position_projection_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "competitor_position_projection_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT CAST(hs2_code AS CHAR) AS hs2_code
    FROM germany_competitive_position_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "germany_competitive_position_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT CAST(hs2_code AS CHAR) AS hs2_code
    FROM germany_export_forecasts_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "germany_export_forecasts_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT CAST(hs2_code AS CHAR) AS hs2_code
    FROM germany_position_summary_2025_2029
    "
  ) |>
    dplyr::mutate(
      table_name = "germany_position_summary_2025_2029"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT CAST(hs2_code AS CHAR) AS hs2_code
    FROM global_market_position
    "
  ) |>
    dplyr::mutate(
      table_name = "global_market_position"
    ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT DISTINCT CAST(hs2_code AS CHAR) AS hs2_code
    FROM sector_lookup
    "
  ) |>
    dplyr::mutate(
      table_name = "sector_lookup"
    )
  
) |>
  dplyr::select(
    table_name,
    hs2_code
  ) |>
  dplyr::arrange(
    table_name,
    hs2_code
  )

phase15_hs2_key_check

############################################################
# Phase 15 - Step 5
# Inspect MySQL data types for relationship keys
############################################################

phase15_key_types <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
    TABLE_NAME AS table_name,
    COLUMN_NAME AS column_name,
    DATA_TYPE AS data_type,
    COLUMN_TYPE AS column_type
  FROM INFORMATION_SCHEMA.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND COLUMN_NAME IN ('hs2_code', 'year')
    AND TABLE_NAME IN (
      'competitor_position_projection_2025_2029',
      'germany_competitive_position_2025_2029',
      'germany_export_forecasts_2025_2029',
      'germany_position_summary_2025_2029',
      'global_market_position',
      'sector_lookup'
    )
  ORDER BY TABLE_NAME, COLUMN_NAME
  "
)

phase15_key_types

############################################################
# Phase 15 - Step 6
# Validate relationship keys before datatype standardization
############################################################

phase15_key_conversion_check <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
    'global_market_position' AS table_name,
    SUM(
      CASE
        WHEN hs2_code NOT REGEXP '^[0-9]+$'
        THEN 1 ELSE 0
      END
    ) AS invalid_hs2_values,
    SUM(
      CASE
        WHEN year IS NULL
        THEN 1 ELSE 0
      END
    ) AS invalid_year_values
  FROM global_market_position
  "
)

phase15_forecast_integer_check <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
    SUM(
      CASE
        WHEN hs2_code <> FLOOR(hs2_code)
        THEN 1 ELSE 0
      END
    ) AS non_integer_hs2_values,
    SUM(
      CASE
        WHEN year <> FLOOR(year)
        THEN 1 ELSE 0
      END
    ) AS non_integer_year_values
  FROM competitor_position_projection_2025_2029
  "
)

phase15_key_conversion_check

phase15_forecast_integer_check

############################################################
# Phase 15 - Step 7
# Standardize relationship key data types
############################################################

DBI::dbExecute(
  mysql_con,
  "
  ALTER TABLE competitor_position_projection_2025_2029
  MODIFY hs2_code INT NOT NULL,
  MODIFY year INT NOT NULL
  "
)

DBI::dbExecute(
  mysql_con,
  "
  ALTER TABLE germany_competitive_position_2025_2029
  MODIFY hs2_code INT NOT NULL,
  MODIFY year INT NOT NULL
  "
)

DBI::dbExecute(
  mysql_con,
  "
  ALTER TABLE germany_export_forecasts_2025_2029
  MODIFY hs2_code INT NOT NULL,
  MODIFY year INT NOT NULL
  "
)

DBI::dbExecute(
  mysql_con,
  "
  ALTER TABLE germany_position_summary_2025_2029
  MODIFY hs2_code INT NOT NULL
  "
)

DBI::dbExecute(
  mysql_con,
  "
  ALTER TABLE global_market_position
  MODIFY hs2_code INT NOT NULL,
  MODIFY year INT NOT NULL
  "
)

DBI::dbExecute(
  mysql_con,
  "
  ALTER TABLE sector_lookup
  MODIFY hs2_code INT NOT NULL
  "
)

############################################################
# Phase 15 - Step 8
# Validate standardized keys and preserved row counts
############################################################

phase15_standardized_key_types <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
    TABLE_NAME AS table_name,
    COLUMN_NAME AS column_name,
    DATA_TYPE AS data_type,
    IS_NULLABLE AS is_nullable
  FROM INFORMATION_SCHEMA.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND COLUMN_NAME IN ('hs2_code', 'year')
    AND TABLE_NAME IN (
      'competitor_position_projection_2025_2029',
      'germany_competitive_position_2025_2029',
      'germany_export_forecasts_2025_2029',
      'germany_position_summary_2025_2029',
      'global_market_position',
      'sector_lookup'
    )
  ORDER BY TABLE_NAME, COLUMN_NAME
  "
)

phase15_standardized_key_types

############################################################
# Phase 15 - Step 8B
# Confirm row counts after datatype standardization
############################################################

phase15_post_conversion_rows <- dplyr::bind_rows(
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'competitor_position_projection_2025_2029' AS table_name,
      COUNT(*) AS mysql_rows
    FROM competitor_position_projection_2025_2029
    "
  ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'germany_competitive_position_2025_2029' AS table_name,
      COUNT(*) AS mysql_rows
    FROM germany_competitive_position_2025_2029
    "
  ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'germany_export_forecasts_2025_2029' AS table_name,
      COUNT(*) AS mysql_rows
    FROM germany_export_forecasts_2025_2029
    "
  ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'germany_position_summary_2025_2029' AS table_name,
      COUNT(*) AS mysql_rows
    FROM germany_position_summary_2025_2029
    "
  ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'global_market_position' AS table_name,
      COUNT(*) AS mysql_rows
    FROM global_market_position
    "
  ),
  
  DBI::dbGetQuery(
    mysql_con,
    "
    SELECT
      'sector_lookup' AS table_name,
      COUNT(*) AS mysql_rows
    FROM sector_lookup
    "
  )
  
) |>
  dplyr::mutate(
    expected_rows = c(
      120,
      20,
      20,
      4,
      9935,
      4
    ),
    row_count_matches = mysql_rows == expected_rows
  )

phase15_post_conversion_rows
########################################################
############################################################
# Phase 15 - Step 10E.2
# Verify competitor projection contents directly in MySQL
############################################################

phase15_competitor_content_check <- DBI::dbGetQuery(
  mysql_con,
  "
  SELECT
    COUNT(*) AS total_rows,
    COUNT(DISTINCT country) AS distinct_countries,
    COUNT(DISTINCT hs2_code) AS distinct_hs2,
    COUNT(DISTINCT year) AS distinct_years,
    COUNT(DISTINCT country, hs2_code, year) AS unique_grain_rows
  FROM competitor_position_projection_2025_2029
  "
)

phase15_competitor_content_check

