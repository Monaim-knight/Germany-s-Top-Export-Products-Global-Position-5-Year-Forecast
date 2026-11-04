library(comtradr)
library(dplyr)
library(readr)

dir.create("data_raw", showWarnings = FALSE)

germany_exports_2023 <- ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = "87",
  flow_direction = "Export",
  reporter = "DEU",
  partner = "World",
  start_date = 2023,
  end_date = 2023,
  verbose = TRUE
)
#############################################
# Checking test data
#######################################
dim(germany_exports_2023)
names(germany_exports_2023)
head(germany_exports_2023)
########################
# Germany's major HS chapters 
###################################
hs2_codes <- sprintf("%02d", 1:99)

germany_2023_all <- ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = hs2_codes,
  flow_direction = "Export",
  reporter = "DEU",
  partner = "World",
  start_date = 2023,
  end_date = 2023,
  verbose = TRUE
)

dim(germany_exports_2023)
names(germany_exports_2023)
head(germany_exports_2023)
#########################
# Fixing valid HS2 codes from comtradr 
# Generate valid HS2 codes 
####################
library(dplyr)

hs2_reference <- hs_reference %>%
  filter(
    aggrLevel == 2,
    id != "TOTAL"
  ) %>%
  select(
    hs2_code = id,
    product_description = text
  )
# Inspecting the result 
dim(hs2_reference)
View(hs2_reference)
head(hs2_reference)
########################
#Create the HS2 code list
##########################
hs2_codes <- sprintf("%02d", 1:97)

head(hs2_codes)
#############################
# all Germany HS2 exports(2023)
#################################
germany_hs2_2023 <- ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = hs2_codes,
  flow_direction = "Export",
  reporter = "DEU",
  partner = "World",
  start_date = 2023,
  end_date = 2023,
  verbose = TRUE
)
##############################
# checking the downloaded data
#######################
dim(germany_hs2_2023)
germany_hs2_2023 %>%
  select(
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  arrange(desc(primary_value)) %>%
  head(15)

###########################
# Updating Project scope 
##########################
target_hs <- c(
  "87",
  "84",
  "85",
  "30"
)
##############################
# Top HS codes 
################################
top_hs_codes <- c(
  "30",  # Pharmaceutical products
  "84",  # Machinery and mechanical appliances
  "85",  # Electrical machinery and equipment
  "87"   # Vehicles and parts
)
germany_test_history <- download_country_history("DEU")

########################################################
# Download Function 
####################################################
download_country_history <- function(country_iso) {
  
  message("  Downloading 2010–2021...")
  
  period_1 <- comtradr::ct_get_data(
    type = "goods",
    frequency = "A",
    commodity_classification = "HS",
    commodity_code = top_hs_codes,
    flow_direction = "Export",
    reporter = country_iso,
    partner = "World",
    start_date = 2010,
    end_date = 2021,
    verbose = FALSE
  )
  
  message("  Downloading 2022–2024...")
  
  period_2 <- comtradr::ct_get_data(
    type = "goods",
    frequency = "A",
    commodity_classification = "HS",
    commodity_code = top_hs_codes,
    flow_direction = "Export",
    reporter = country_iso,
    partner = "World",
    start_date = 2022,
    end_date = 2024,
    verbose = FALSE
  )
  
  country_history <- dplyr::bind_rows(
    period_1,
    period_2
  )
  
  return(country_history)
}
##################################
# Download all countries dataset required for the project
####################################################
countries <- data.frame(
  country = c(
    "Germany",
    "United States",
    "China",
    "Japan",
    "South Korea",
    "France"
  ),
  iso3 = c(
    "DEU",
    "USA",
    "CHN",
    "JPN",
    "KOR",
    "FRA"
  ),
  stringsAsFactors = FALSE
)

###############################################
all_history <- list()

for (i in seq_len(nrow(countries))) {
  
  current_country <- countries$country[i]
  current_iso3 <- countries$iso3[i]
  
  message("Downloading: ", current_country)
  
  tmp <- download_country_history(current_iso3)
  
  tmp <- tmp %>%
    mutate(
      country_name = current_country,
      project_iso3 = current_iso3,
      extraction_date = as.character(Sys.Date())
    )
  
  all_history[[i]] <- tmp
  
  Sys.sleep(2)
}
######################################
# Combining the results 
###########################################
trade_history <- bind_rows(all_history)
# Checking dimensions of it 
dim(trade_history)
###################################
# Validating completeness 
###################################
trade_history %>%
  count(
    country_name,
    ref_year
  ) %>%
  arrange(
    country_name,
    ref_year
  )
## Checking HS codes 
sort(unique(trade_history$cmd_code))
# Checking years 
range(trade_history$ref_year,na.rm = TRUE)
#######################################
# Checking duplicate business keys 
########################################
trade_history %>%
  count(
    country_name,
    ref_year,
    cmd_code
  ) %>%
  filter(n > 1)
#####################################
# Saving the raw dataset 
###############################
write_csv(
  trade_history,
  "data_raw/trade_history_6_countries_2010_2024_raw.csv"
)
#########
# Confirming file is existed 
#############
file.exists(
  "data_raw/trade_history_6_countries_2010_2024_raw.csv"
)

###########################################
# Global Trade Position
##########################################
#####################################################
# GLOBAL EXPORT DATA
# HS30, HS84, HS85, HS87
#####################################################

global_hs_codes <- c(
  "30",
  "84",
  "85",
  "87"
)
##########################
# For Test First
# We need to test whether contradr can return all reporting 
# countries for one product/year without exceding the API'S
# record limits.
###################################

global_test_2024 <- comtradr::ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = "87",
  flow_direction = "Export",
  reporter = "all_countries",
  partner = "World",
  start_date = 2024,
  end_date = 2024,
  verbose = TRUE
)
dim(global_test_2024)

global_test_2024 %>%
  select(
    ref_year,
    reporter_iso,
    reporter_desc,
    cmd_code,
    primary_value
  ) %>%
  arrange(desc(primary_value)) %>%
  head(20)
###############################################
# Download all four products for 2024
############################################
library(comtradr)
library(dplyr)
library(readr)

# Germany's four target HS2 export categories
target_hs <- c(
  "30",  # Pharmaceutical products
  "84",  # Machinery and mechanical appliances
  "85",  # Electrical machinery and equipment
  "87"   # Vehicles and parts
)

# Download exports from all reporting countries for 2024
global_exports_2024 <- comtradr::ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = target_hs,
  flow_direction = "Export",
  reporter = "all_countries",
  partner = "World",
  start_date = 2024,
  end_date = 2024,
  verbose = TRUE
)
dim(global_exports_2024)
names(global_exports_2024)
# Checking how many reporting countries were returned 
global_exports_2024 %>%
  summarise(
    number_of_reporters = n_distinct(reporter_code)
  )
#############################################################
# Inspecting the data (Issues)
global_exports_2024 %>%
  select(
    ref_year,
    reporter_iso,
    reporter_desc,
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  arrange(
    cmd_code,
    desc(primary_value)
  ) %>%
  print(n = 30)
########################################################
global_preview_2024 <- global_exports_2024 %>%
  select(
    ref_year,
    reporter_iso,
    reporter_desc,
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  arrange(
    cmd_code,
    desc(primary_value)
  ) %>%
  slice_head(n = 30)

View(global_preview_2024)
print(global_preview_2024)
######################################
# Confirming all four products exist
##########################################
global_exports_2024 %>%
  count(
    cmd_code,
    cmd_desc,
    name = "number_of_reporters"
  ) %>%
  arrange(cmd_code)
# Checking duplicate country-product records 
global_duplicates_2024 <- global_exports_2024 %>%
  count(
    reporter_code,
    ref_year,
    cmd_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)

global_duplicates_2024
############################################
# Checking German Four Records 
#############################################
germany_global_2024 <- global_exports_2024 %>%
  filter(reporter_iso == "DEU") %>%
  select(
    ref_year,
    reporter_iso,
    reporter_desc,
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  arrange(cmd_code)

germany_global_2024

nrow(germany_global_2024)
##################################
# Saving the dataset for 2024 global 
################################
write_csv(
  global_exports_2024,
  "data_raw/global_exports_hs30_84_85_87_2024_raw.csv"
)
###############################################
# Downloading global historical data, 2010-2023 
################################################
# Createing a folder for yearly global files 
#############################################
####################################################
# Create folder for yearly global raw files
####################################################

dir.create(
  "data_raw/global_yearly",
  showWarnings = FALSE,
  recursive = TRUE
)
####################################
# creating the yearly download functions 
#########################################
####################################################
# Function: Download one year of global exports
####################################################

download_global_year <- function(
    year,
    hs_codes = target_hs,
    max_attempts = 3
) {
  
  for (attempt in seq_len(max_attempts)) {
    
    message(
      "Downloading global data for ",
      year,
      " | attempt ",
      attempt,
      " of ",
      max_attempts
    )
    
    result <- tryCatch(
      {
        comtradr::ct_get_data(
          type = "goods",
          frequency = "A",
          commodity_classification = "HS",
          commodity_code = hs_codes,
          flow_direction = "Export",
          reporter = "all_countries",
          partner = "World",
          start_date = year,
          end_date = year,
          verbose = FALSE
        )
      },
      error = function(e) {
        message(
          "Download failed for ",
          year,
          ": ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
    
    if (!is.null(result) && nrow(result) > 0) {
      
      result <- result %>%
        mutate(
          extraction_date = as.character(Sys.Date())
        )
      
      message(
        "Successfully downloaded ",
        nrow(result),
        " rows for ",
        year
      )
      
      return(result)
    }
    
    if (attempt < max_attempts) {
      message("Waiting 30 seconds before retrying...")
      Sys.sleep(30)
    }
  }
  
  warning(
    "No data downloaded for ",
    year,
    " after ",
    max_attempts,
    " attempts."
  )
  
  return(NULL)
}
##########################################
# Testing the function with 2023 
##############################################
global_test_2023 <- download_global_year(2023)
dim(global_test_2023)
# Checking all four products 
global_test_2023 %>%
  count(
    cmd_code,
    name = "number_of_records"
  ) %>%
  arrange(cmd_code)
# Checking Germany 
global_test_2023 %>%
  filter(reporter_iso == "DEU") %>%
  select(
    ref_year,
    reporter_iso,
    reporter_desc,
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  arrange(cmd_code)
################
# Saving the test result of 2023 
##################################
write_csv(
  global_test_2023,
  "data_raw/global_yearly/global_exports_2023_raw.csv"
)
####################################################
# Creating the historical year sequence 
########################################
####################################################
# Global historical extraction years
# 2023 and 2024 are already downloaded
####################################################

historical_years <- 2010:2022
######################################
# Run the controlled download loop 
#########################################
####################################################
# Download and save global data for 2010–2022
####################################################

download_log <- data.frame(
  year = integer(),
  status = character(),
  rows = integer(),
  file_path = character(),
  stringsAsFactors = FALSE
)

for (current_year in historical_years) {
  
  yearly_file <- paste0(
    "data_raw/global_yearly/global_exports_",
    current_year,
    "_raw.csv"
  )
###################################
  # Confirming no failed years 
#####################################
  failed_years <- download_log %>%
    filter(status == "failed")
  
  failed_years
# Save the download log 
  write_csv(
    download_log,
    "data_raw/global_yearly/global_download_log_2010_2022.csv"
  )
###################################
# Copy the 2024 files into the yearly folder 
########################################
  file.copy(
    from = "data_raw/global_exports_hs30_84_85_87_2024_raw.csv",
    to = "data_raw/global_yearly/global_exports_2024_raw.csv",
    overwrite = TRUE
  )
  
  # Skip years that were already downloaded successfully
  if (file.exists(yearly_file)) {
    
    message(
      "Skipping ",
      current_year,
      ": file already exists."
    )
    
    existing_data <- readr::read_csv(
      yearly_file,
      show_col_types = FALSE
    )
    
    download_log <- dplyr::bind_rows(
      download_log,
      data.frame(
        year = current_year,
        status = "already existed",
        rows = nrow(existing_data),
        file_path = yearly_file
      )
    )
    
    next
  }
  
  # Download the current year
  yearly_data <- download_global_year(current_year)
  
  # Save only when valid data were returned
  if (!is.null(yearly_data) && nrow(yearly_data) > 0) {
    
    readr::write_csv(
      yearly_data,
      yearly_file
    )
    
    download_log <- dplyr::bind_rows(
      download_log,
      data.frame(
        year = current_year,
        status = "downloaded",
        rows = nrow(yearly_data),
        file_path = yearly_file
      )
    )
    
    message(
      "Saved ",
      current_year,
      " with ",
      nrow(yearly_data),
      " rows."
    )
    
  } else {
    
    download_log <- dplyr::bind_rows(
      download_log,
      data.frame(
        year = current_year,
        status = "failed",
        rows = 0L,
        file_path = yearly_file
      )
    )
    
    warning(
      "No file was created for ",
      current_year
    )
  }
  
  # Reduce pressure on the API
  Sys.sleep(5)
}

##################################################
# List all yearly data files 
##########################################
global_yearly_files <- list.files(
  "data_raw/global_yearly",
  pattern = "^global_exports_[0-9]{4}_raw\\.csv$",
  full.names = TRUE
)

global_yearly_files <- sort(global_yearly_files)

length(global_yearly_files)
basename(global_yearly_files)
###########################
# Validating all 15 files 
###################
yearly_file_validation <- lapply(
  global_yearly_files,
  function(file_path) {
    
    file_data <- read_csv(
      file_path,
      show_col_types = FALSE
    )
    
    data.frame(
      file = basename(file_path),
      rows = nrow(file_data),
      reporters = n_distinct(file_data$reporter_code),
      hs_codes = n_distinct(file_data$cmd_code),
      minimum_year = min(file_data$ref_year, na.rm = TRUE),
      maximum_year = max(file_data$ref_year, na.rm = TRUE)
    )
  }
) %>%
  bind_rows()
###########################################
# Checking invalid files 
#########################################
invalid_yearly_files <- yearly_file_validation %>%
  filter(
    rows == 0 |
      reporters == 0 |
      hs_codes != 4 |
      minimum_year != maximum_year
  )

invalid_yearly_files
###########################################
# Saving the validation summary
###########################################
write_csv(
  yearly_file_validation,
  "data_raw/global_yearly/global_yearly_validation_2010_2024.csv"
)
#######################################
# Read and combine the 15 yearly files 
############################################
####################################################
# Combine validated global yearly datasets
# Years: 2010–2024
####################################################

global_history_raw <- lapply(
  global_yearly_files,
  function(file_path) {
    
    readr::read_csv(
      file_path,
      show_col_types = FALSE
    )
  }
) %>%
  dplyr::bind_rows()
# Checking the combined files 
dim(global_history_raw)
names(global_history_raw)
range(
  global_history_raw$ref_year,
  na.rm = TRUE
)
# Checking that all 15 years exist
sort(
  unique(global_history_raw$ref_year)
)
# Counting the years 
dplyr::n_distinct(
  global_history_raw$ref_year
)
# Validating products 
sort(
  unique(global_history_raw$cmd_code)
)
# Count the products 
dplyr::n_distinct(
  global_history_raw$cmd_code
)
# Checking records by year and product 
global_record_coverage <- global_history_raw %>%
  count(
    ref_year,
    cmd_code,
    name = "number_of_records"
  ) %>%
  arrange(
    ref_year,
    cmd_code
  )
View(global_record_coverage)
head(global_record_coverage,
     20)
nrow(global_record_coverage)
# Checkihg duplicate business keys 
global_duplicate_check <- global_history_raw %>%
  count(
    ref_year,
    reporter_code,
    cmd_code,
    name = "record_count"
  ) %>%
  filter(record_count > 1)
nrow(global_duplicate_check)

# Confirming Germany has four records per year 
germany_global_coverage <- global_history_raw %>%
  filter(reporter_iso == "DEU") %>%
  count(
    ref_year,
    name = "number_of_products"
  ) %>%
  arrange(ref_year)
germany_global_coverage
# Checking for missing Germany product-year 
germany_global_coverage %>%
  filter(number_of_products != 4)
#####################################################
# Check missing critical values 
global_missing_check <- global_history_raw %>%
  summarise(
    total_rows = n(),
    
    missing_year =
      sum(is.na(ref_year)),
    
    missing_reporter =
      sum(is.na(reporter_code)),
    
    missing_hs2_code =
      sum(is.na(cmd_code)),
    
    missing_export_value =
      sum(is.na(primary_value))
  )
############################################
#Saving the combined raw global dataset 
readr::write_csv(
  global_history_raw,
  "data_raw/global_exports_hs30_84_85_87_2010_2024_raw.csv"
)
###############################################
# Save the combination validation results 
###############################################
readr::write_csv(
  global_record_coverage,
  "data_raw/global_yearly/global_combined_coverage_validation.csv"
)

readr::write_csv(
  global_duplicate_check,
  "data_raw/global_yearly/global_combined_duplicate_check.csv"
)

readr::write_csv(
  germany_global_coverage,
  "data_raw/global_yearly/germany_global_coverage_validation.csv"
)

readr::write_csv(
  global_missing_check,
  "data_raw/global_yearly/global_combined_missing_check.csv"
)