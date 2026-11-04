############################################################
# Germany Export Forecast Project
#
# Reusable Data Validation Functions
#
# Purpose:
# Centralize repeated data-quality checks used throughout
# the project.
#
# Used by:
# - Data cleaning
# - Feature engineering
# - Forecast preparation
# - Forecast validation
# - MySQL loading
# - Tableau dataset preparation
############################################################

library(dplyr)

############################################################
# Function: validate_dataset()
#
# Purpose:
# Perform standard structural and data-quality checks
# consistently across project datasets.
############################################################

validate_dataset <- function(
    data,
    dataset_name,
    key_columns = NULL,
    critical_columns = NULL,
    expected_rows = NULL,
    expected_years = NULL,
    expected_products = NULL,
    expected_countries = NULL,
    year_column = "year",
    product_column = "hs2_code",
    country_column = "country"
) {
  
  # --------------------------------------------------------
  # Basic structure
  # --------------------------------------------------------
  
  actual_rows <- nrow(data)
  actual_columns <- ncol(data)
  
  # --------------------------------------------------------
  # Duplicate business-key check
  # --------------------------------------------------------
  
  duplicate_rows <- NA_integer_
  
  if (!is.null(key_columns)) {
    
    missing_key_columns <- setdiff(
      key_columns,
      names(data)
    )
    
    if (length(missing_key_columns) == 0) {
      
      duplicate_rows <- data %>%
        count(
          across(all_of(key_columns)),
          name = "record_count"
        ) %>%
        filter(record_count > 1) %>%
        nrow()
    }
  }
  
  # --------------------------------------------------------
  # Critical missing-value check
  # --------------------------------------------------------
  
  critical_missing_values <- NA_integer_
  
  if (!is.null(critical_columns)) {
    
    available_critical_columns <- intersect(
      critical_columns,
      names(data)
    )
    
    if (length(available_critical_columns) > 0) {
      
      critical_missing_values <- data %>%
        select(
          all_of(available_critical_columns)
        ) %>%
        is.na() %>%
        sum()
    }
  }
  
  # --------------------------------------------------------
  # Year coverage
  # --------------------------------------------------------
  
  actual_years <- NA_integer_
  
  if (year_column %in% names(data)) {
    
    actual_years <- data %>%
      pull(
        all_of(year_column)
      ) %>%
      n_distinct()
  }
  
  # --------------------------------------------------------
  # Product coverage
  # --------------------------------------------------------
  
  actual_products <- NA_integer_
  
  if (product_column %in% names(data)) {
    
    actual_products <- data %>%
      pull(
        all_of(product_column)
      ) %>%
      n_distinct()
  }
  
  # --------------------------------------------------------
  # Country coverage
  # --------------------------------------------------------
  
  actual_countries <- NA_integer_
  
  if (country_column %in% names(data)) {
    
    actual_countries <- data %>%
      pull(
        all_of(country_column)
      ) %>%
      n_distinct()
  }
  
  # --------------------------------------------------------
  # Create validation report
  # --------------------------------------------------------
  
  validation_report <- tibble::tibble(
    dataset = dataset_name,
    
    rows = actual_rows,
    columns = actual_columns,
    
    duplicate_key_groups = duplicate_rows,
    
    critical_missing_values =
      critical_missing_values,
    
    years = actual_years,
    products = actual_products,
    countries = actual_countries,
    
    row_count_valid =
      if (is.null(expected_rows)) {
        NA
      } else {
        actual_rows == expected_rows
      },
    
    year_count_valid =
      if (is.null(expected_years)) {
        NA
      } else {
        actual_years == expected_years
      },
    
    product_count_valid =
      if (is.null(expected_products)) {
        NA
      } else {
        actual_products == expected_products
      },
    
    country_count_valid =
      if (is.null(expected_countries)) {
        NA
      } else {
        actual_countries == expected_countries
      },
    
    duplicate_check_valid =
      if (is.na(duplicate_rows)) {
        NA
      } else {
        duplicate_rows == 0
      },
    
    missing_check_valid =
      if (is.na(critical_missing_values)) {
        NA
      } else {
        critical_missing_values == 0
      }
  )
  
  return(validation_report)
}
############################################################
# Step C: Test reusable validation function
############################################################

competitor_validation_report <- validate_dataset(
  data = competitor_features,
  dataset_name = "Competitor Historical Features",
  
  key_columns = c(
    "country",
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "country",
    "year",
    "hs2_code",
    "product_group",
    "export_value_usd",
    "comparison_export_share",
    "competitor_rank"
  ),
  
  expected_rows = 360,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 6
)
print(
  competitor_validation_report,
  width = Inf
)