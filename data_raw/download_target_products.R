library(comtradr)
library(dplyr)
library(readr)

target_hs <- c(
  "87",  # Vehicles
  "84",  # Machinery
  "85",  # Electrical Equipment
  "30"   # Pharmaceuticals
)

download_country_history <- function(
    reporter_iso,
    reporter_name
) {
  
  results <- list()
  
  for (yr in 2010:2024) {
    
    cat(
      "Downloading:",
      reporter_name,
      "-",
      yr,
      "\n"
    )
    
    df <- ct_get_data(
      type = "goods",
      frequency = "A",
      commodity_classification = "HS",
      commodity_code = target_hs,
      flow_direction = "Export",
      reporter = reporter_iso,
      partner = "World",
      start_date = yr,
      end_date = yr,
      verbose = FALSE
    )
    
    df$project_country <- reporter_name
    
    results[[as.character(yr)]] <- df
    
    Sys.sleep(1)
  }
  
  bind_rows(results)
}
#################################
# Checking Germany First 
################################
germany_history <- download_country_history(
  reporter_iso = "DEU",
  reporter_name = "Germany"
)

######################################
# Examining data 
#####################################
dim(germany_history)
germany_history %>%
  select(
    ref_year,
    cmd_code,
    cmd_desc,
    primary_value
  ) %>%
  head(20)
#############################
# as everythins is in ordered , we will save the dataset for Germany
##############################################
write_csv(
  germany_history,
  "data_raw/germany_history_2010_2024.csv"
)
##########################
# Verifying the save 
###########################
file.exists(
  "data_raw/germany_history_2010_2024.csv"
)
##################################
# Now we will download competitor countries 
############################################
#USA
#China
#Japan
#South Korea
#France
######################################
# Create Country Table
#####################################
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
  )
)
######################################
# Selected Export Categories 
#######################################
top_hs_codes <- c(
  "30",  # Pharma
  "84",  # Machinery
  "85",  # Electrical Equipment
  "87"   # Vehicles
)