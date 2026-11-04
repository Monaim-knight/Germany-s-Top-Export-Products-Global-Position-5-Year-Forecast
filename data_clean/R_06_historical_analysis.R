############################################################
# Germany Export Forecast Project
#
# Phase 8: Historical Exploratory & Comparative Analysis
#
# Period: 2010-2024
# Products: HS30, HS84, HS85, HS87
#
# Purpose:
# Analyze Germany's historical export performance,
# global market position, growth trends, and competitive
# performance before developing the forecasting models.
############################################################

library(dplyr)
library(readr)

source(
  "R/R_validation_functions.R"
)
############################################
# Verify validation functions were loaded
############################################

ls()
exists("function_name")

############################################
# List functions currently available
############################################

loaded_functions <- ls()[
  sapply(
    ls(),
    function(x) {
      is.function(
    get(x, envir = .GlobalEnv)
      )
    }
  )
]

loaded_functions

library(dplyr)
library(readr)

source(
  "R/R_validation_functions.R"
)
############################################################
# Phase 8 - Step 1
# Load validated analytical datasets
############################################################

germany_history_analysis <- readr::read_csv(
  "data_clean/germany_historical_features_2010_2024.csv",
  show_col_types = FALSE
)

competitor_history_analysis <- readr::read_csv(
  "data_clean/competitor_historical_features_2010_2024.csv",
  show_col_types = FALSE
)

germany_competitor_analysis <- readr::read_csv(
  "data_clean/germany_competitor_comparison_2010_2024.csv",
  show_col_types = FALSE
)

global_market_analysis <- readr::read_csv(
  "data_clean/global_market_position_2010_2024.csv",
  show_col_types = FALSE
)
############################################################
# Check datasets loaded successfully
############################################################

phase8_loading_check <- tibble::tibble(
  dataset = c(
    "Germany Historical Features",
    "Competitor Historical Features",
    "Germany Competitor Comparison",
    "Global Market Position"
  ),
  
  rows = c(
    nrow(germany_history_analysis),
    nrow(competitor_history_analysis),
    nrow(germany_competitor_analysis),
    nrow(global_market_analysis)
  ),
  
  columns = c(
    ncol(germany_history_analysis),
    ncol(competitor_history_analysis),
    ncol(germany_competitor_analysis),
    ncol(global_market_analysis)
  )
)

phase8_loading_check

phase8_loading_check %>%
  filter(
    rows <= 0 |
      columns <= 0
  )

############################################################
# Phase 8 - Step 2.1
# Validate Germany historical features
############################################################

validation_germany_history <- validate_dataset(
  data = germany_history_analysis,
  dataset_name = "Germany Historical Analysis",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "country",
    "hs2_code",
    "product_group",
    "export_value_usd",
    "global_market_share",
    "global_rank"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)

############################################################
# Phase 8 - Step 2.2
# Validate competitor historical features
############################################################

validation_competitor_history <- validate_dataset(
  data = competitor_history_analysis,
  dataset_name = "Competitor Historical Analysis",
  
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

############################################################
# Phase 8 - Step 2.3
# Validate Germany competitor comparison
############################################################

validation_germany_competitor <- validate_dataset(
  data = germany_competitor_analysis,
  dataset_name = "Germany Competitor Analysis",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "country",
    "hs2_code",
    "product_group",
    "export_value_usd",
    "global_market_share",
    "global_rank",
    "competitor_average_export_value_usd",
    "leading_competitor",
    "competitor_leadership_status"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)
############################################################
# Phase 8 - Step 2.4
# Validate global market-position dataset
############################################################

validation_global_market <- validate_dataset(
  data = global_market_analysis,
  dataset_name = "Global Market Analysis",
  
  key_columns = c(
    "year",
    "country_code",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "country_code",
    "country",
    "hs2_code",
    "product_group",
    "export_value_usd",
    "global_market_share",
    "global_rank"
  ),
  
  expected_rows = 9935,
  expected_years = 15,
  expected_products = 4
)

############################################################
# Combine Phase 8 validation reports
############################################################

phase8_validation_report <- dplyr::bind_rows(
  validation_germany_history,
  validation_competitor_history,
  validation_germany_competitor,
  validation_global_market
)
print(
  phase8_validation_report,
  width = Inf
)

phase8_validation_failures <- phase8_validation_report %>%
  filter(
    row_count_valid == FALSE |
      year_count_valid == FALSE |
      product_count_valid == FALSE |
      country_count_valid == FALSE |
      duplicate_check_valid == FALSE |
      missing_check_valid == FALSE
  )

phase8_validation_failures
nrow(
  phase8_validation_failures
)

############################################################
# Phase 8 - Step 3.1
# Germany export-value trend summary
# Period: 2010-2024
############################################################

germany_export_trend_summary <- germany_history_analysis %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    start_year =
      min(year),
    
    end_year =
      max(year),
    
    start_export_value_usd =
      export_value_usd[which.min(year)],
    
    end_export_value_usd =
      export_value_usd[which.max(year)],
    
    minimum_export_value_usd =
      min(export_value_usd, na.rm = TRUE),
    
    minimum_export_year =
      year[which.min(export_value_usd)],
    
    maximum_export_value_usd =
      max(export_value_usd, na.rm = TRUE),
    
    maximum_export_year =
      year[which.max(export_value_usd)],
    
    average_export_value_usd =
      mean(export_value_usd, na.rm = TRUE),
    
    median_export_value_usd =
      median(export_value_usd, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    absolute_export_change_usd =
      end_export_value_usd -
      start_export_value_usd,
    
    total_export_growth_pct =
      (
        end_export_value_usd /
          start_export_value_usd - 1
      ) * 100
  ) %>%
  arrange(
    desc(end_export_value_usd)
  )

print(
  germany_export_trend_summary,
  n = 4,
  width = Inf
)
############################################################
# Validate Germany export trend summary
############################################################

germany_export_trend_validation <- validate_dataset(
  data = germany_export_trend_summary,
  dataset_name = "Germany Export Trend Summary",
  
  key_columns = c(
    "hs2_code"
  ),
  
  critical_columns = c(
    "hs2_code",
    "product_group",
    "start_year",
    "end_year",
    "start_export_value_usd",
    "end_export_value_usd",
    "minimum_export_value_usd",
    "maximum_export_value_usd",
    "average_export_value_usd",
    "absolute_export_change_usd",
    "total_export_growth_pct"
  ),
  
  expected_rows = 4,
  expected_products = 4
)

print(
  germany_export_trend_validation,
  width = Inf
)
germany_export_trend_summary %>%
  select(
    hs2_code,
    start_year,
    end_year
  ) %>%
  arrange(hs2_code)
## Create an easier to read analytical view 
germany_export_trend_view <- germany_export_trend_summary %>%
  transmute(
    hs2_code,
    product_group,
    
    start_export_usd_bn =
      start_export_value_usd / 1e9,
    
    end_export_usd_bn =
      end_export_value_usd / 1e9,
    
    absolute_change_usd_bn =
      absolute_export_change_usd / 1e9,
    
    total_growth_pct =
      total_export_growth_pct,
    
    minimum_export_usd_bn =
      minimum_export_value_usd / 1e9,
    
    minimum_export_year,
    
    maximum_export_usd_bn =
      maximum_export_value_usd / 1e9,
    
    maximum_export_year
  ) %>%
  arrange(
    desc(end_export_usd_bn)
  )
print(
  germany_export_trend_view,
  n = 4,
  width = Inf
)
############################################################
# Step 21.1: Save Germany export trend summary
############################################################

readr::write_csv(
  germany_export_trend_summary,
  "data_clean/germany_export_trend_summary_2010_2024.csv"
)
############################################################
# Step 21.2: Save readable export trend view
############################################################

readr::write_csv(
  germany_export_trend_view,
  "data_clean/germany_export_trend_view_2010_2024.csv"
)
trend_summary_files <- c(
  "data_clean/germany_export_trend_summary_2010_2024.csv",
  "data_clean/germany_export_trend_view_2010_2024.csv"
)

file.exists(
  trend_summary_files
)
germany_export_trend_summary_check <- readr::read_csv(
  "data_clean/germany_export_trend_summary_2010_2024.csv",
  show_col_types = FALSE
)

germany_export_trend_view_check <- readr::read_csv(
  "data_clean/germany_export_trend_view_2010_2024.csv",
  show_col_types = FALSE
)

dim(
  germany_export_trend_summary_check
)
dim(
  germany_export_trend_view_check
)
############################################################
# Phase 8 - Step 3.6
# Display Germany export trend results
############################################################

print(
  germany_export_trend_view_check,
  n = 4,
  width = Inf
)

############################################################
# Phase 8 - Step 4.1
# Identify strongest and weakest YoY export-growth years
############################################################

germany_turning_points <- germany_history_analysis %>%
  filter(
    !is.na(export_yoy_growth_pct)
  ) %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    strongest_growth_year =
      year[which.max(export_yoy_growth_pct)],
    
    strongest_growth_pct =
      max(
        export_yoy_growth_pct,
        na.rm = TRUE
      ),
    
    weakest_growth_year =
      year[which.min(export_yoy_growth_pct)],
    
    weakest_growth_pct =
      min(
        export_yoy_growth_pct,
        na.rm = TRUE
      ),
    
    positive_growth_years =
      sum(
        export_yoy_growth_pct > 0,
        na.rm = TRUE
      ),
    
    negative_growth_years =
      sum(
        export_yoy_growth_pct < 0,
        na.rm = TRUE
      ),
    
    unchanged_growth_years =
      sum(
        export_yoy_growth_pct == 0,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  arrange(
    hs2_code
  )
print(
  germany_turning_points,
  n = 4,
  width = Inf
)
############################################################
# Validate Germany historical turning-point summary
############################################################

germany_turning_points_validation <- validate_dataset(
  data = germany_turning_points,
  dataset_name = "Germany Historical Turning Points",
  
  key_columns = c(
    "hs2_code"
  ),
  
  critical_columns = c(
    "hs2_code",
    "product_group",
    "strongest_growth_year",
    "strongest_growth_pct",
    "weakest_growth_year",
    "weakest_growth_pct",
    "positive_growth_years",
    "negative_growth_years"
  ),
  
  expected_rows = 4,
  expected_products = 4
)

print(
  germany_turning_points_validation,
  width = Inf
)
# Validation - The number of growth intervals 
turning_point_interval_check <- germany_turning_points %>%
  mutate(
    total_growth_intervals =
      positive_growth_years +
      negative_growth_years +
      unchanged_growth_years
  )
# Inspecting 
turning_point_interval_check %>%
  select(
    hs2_code,
    positive_growth_years,
    negative_growth_years,
    unchanged_growth_years,
    total_growth_intervals
  )
all(
  turning_point_interval_check$
    total_growth_intervals == 14
)
############################################################
# Phase 8 - Step 5.1
# Germany global market-share trend analysis
############################################################

germany_market_share_trend <- germany_history_analysis %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    start_year =
      min(year),
    
    end_year =
      max(year),
    
    start_market_share =
      global_market_share[which.min(year)],
    
    end_market_share =
      global_market_share[which.max(year)],
    
    minimum_market_share =
      min(
        global_market_share,
        na.rm = TRUE
      ),
    
    minimum_market_share_year =
      year[which.min(global_market_share)],
    
    maximum_market_share =
      max(
        global_market_share,
        na.rm = TRUE
      ),
    
    maximum_market_share_year =
      year[which.max(global_market_share)],
    
    average_market_share =
      mean(
        global_market_share,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    market_share_change =
      end_market_share -
      start_market_share,
    
    market_share_change_pct_points =
      market_share_change * 100,
    
    start_market_share_pct =
      start_market_share * 100,
    
    end_market_share_pct =
      end_market_share * 100,
    
    minimum_market_share_pct =
      minimum_market_share * 100,
    
    maximum_market_share_pct =
      maximum_market_share * 100,
    
    average_market_share_pct =
      average_market_share * 100
  ) %>%
  arrange(
    hs2_code
  )

# Inspect the readable result 
germany_market_share_view <- germany_market_share_trend %>%
  select(
    hs2_code,
    product_group,
    start_market_share_pct,
    end_market_share_pct,
    market_share_change_pct_points,
    minimum_market_share_pct,
    minimum_market_share_year,
    maximum_market_share_pct,
    maximum_market_share_year,
    average_market_share_pct
  )
print(
  germany_market_share_view,
  n = 4,
  width = Inf
)
############################################################
# Validate Germany market-share trend summary
############################################################

germany_market_share_validation <- validate_dataset(
  data = germany_market_share_trend,
  dataset_name = "Germany Global Market Share Trend",
  
  key_columns = c(
    "hs2_code"
  ),
  
  critical_columns = c(
    "hs2_code",
    "product_group",
    "start_market_share",
    "end_market_share",
    "market_share_change_pct_points",
    "minimum_market_share",
    "maximum_market_share",
    "average_market_share"
  ),
  
  expected_rows = 4,
  expected_products = 4
)

print(
  germany_market_share_validation,
  width = Inf
)
# Confirm market shares are valid percentages 
invalid_market_share_summary <- germany_market_share_trend %>%
  filter(
    start_market_share < 0 |
      start_market_share > 1 |
      end_market_share < 0 |
      end_market_share > 1 |
      minimum_market_share < 0 |
      maximum_market_share > 1
  )

nrow(
  invalid_market_share_summary
)
############################################################
# Phase 8 - Step 6.1
# Germany global-rank trend analysis
############################################################

germany_rank_trend <- germany_history_analysis %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    start_year =
      min(year),
    
    end_year =
      max(year),
    
    start_global_rank =
      global_rank[which.min(year)],
    
    end_global_rank =
      global_rank[which.max(year)],
    
    best_global_rank =
      min(
        global_rank,
        na.rm = TRUE
      ),
    
    best_rank_year =
      year[which.min(global_rank)],
    
    worst_global_rank =
      max(
        global_rank,
        na.rm = TRUE
      ),
    
    worst_rank_year =
      year[which.max(global_rank)],
    
    average_global_rank =
      mean(
        global_rank,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    global_rank_change =
      end_global_rank -
      start_global_rank,
    
    rank_trend = case_when(
      global_rank_change < 0 ~
        "Improved",
      
      global_rank_change > 0 ~
        "Declined",
      
      global_rank_change == 0 ~
        "Unchanged"
    )
  ) %>%
  arrange(
    hs2_code
  )
############################################################
# Phase 8 - Step 6.1
# Germany global-rank trend analysis
############################################################

germany_rank_trend <- germany_history_analysis %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    start_year =
      min(year),
    
    end_year =
      max(year),
    
    start_global_rank =
      global_rank[which.min(year)],
    
    end_global_rank =
      global_rank[which.max(year)],
    
    best_global_rank =
      min(
        global_rank,
        na.rm = TRUE
      ),
    
    best_rank_year =
      year[which.min(global_rank)],
    
    worst_global_rank =
      max(
        global_rank,
        na.rm = TRUE
      ),
    
    worst_rank_year =
      year[which.max(global_rank)],
    
    average_global_rank =
      mean(
        global_rank,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    global_rank_change =
      end_global_rank -
      start_global_rank,
    
    rank_trend = case_when(
      global_rank_change < 0 ~
        "Improved",
      
      global_rank_change > 0 ~
        "Declined",
      
      global_rank_change == 0 ~
        "Unchanged"
    )
  ) %>%
  arrange(
    hs2_code
  )
print(
  germany_rank_trend,
  n = 4,
  width = Inf
)
############################################################
# Validate Germany global-rank trend analysis
############################################################

germany_rank_trend_validation <- validate_dataset(
  data = germany_rank_trend,
  dataset_name = "Germany Global Rank Trend",
  
  key_columns = c(
    "hs2_code"
  ),
  
  critical_columns = c(
    "hs2_code",
    "product_group",
    "start_global_rank",
    "end_global_rank",
    "best_global_rank",
    "best_rank_year",
    "worst_global_rank",
    "worst_rank_year",
    "average_global_rank",
    "global_rank_change",
    "rank_trend"
  ),
  
  expected_rows = 4,
  expected_products = 4
)
print(
  germany_rank_trend_validation,
  width = Inf
)
# Validate rank logically 
invalid_rank_trends <- germany_rank_trend %>%
  filter(
    best_global_rank >
      worst_global_rank
  )

nrow(
  invalid_rank_trends
)
germany_rank_trend %>%
  select(
    hs2_code,
    start_year,
    end_year
  ) %>%
  arrange(hs2_code)

############################################################
# Phase 8 - Step 7.1
# Germany vs competitor long-term growth analysis
############################################################

germany_competitor_growth <- readr::read_csv(
  "data_clean/germany_vs_competitors_cagr_2010_2024.csv",
  show_col_types = FALSE
) %>%
  select(
    hs2_code,
    product_group,
    germany_cagr_pct,
    competitor_average_cagr_pct,
    competitor_median_cagr_pct,
    fastest_competitor,
    fastest_competitor_cagr_pct,
    cagr_gap_vs_competitor_average_pct_points,
    cagr_gap_vs_fastest_competitor_pct_points,
    germany_growth_position
  ) %>%
  arrange(
    hs2_code
  )
print(
  germany_competitor_growth,
  n = 4,
  width = Inf
)
############################################################
# Validate competitive-growth summary
############################################################

germany_competitor_growth_validation <- validate_dataset(
  data = germany_competitor_growth,
  dataset_name = "Germany Competitor Growth Analysis",
  
  key_columns = c(
    "hs2_code"
  ),
  
  critical_columns = c(
    "hs2_code",
    "product_group",
    "germany_cagr_pct",
    "competitor_average_cagr_pct",
    "competitor_median_cagr_pct",
    "fastest_competitor",
    "fastest_competitor_cagr_pct",
    "cagr_gap_vs_competitor_average_pct_points",
    "cagr_gap_vs_fastest_competitor_pct_points",
    "germany_growth_position"
  ),
  
  expected_rows = 4,
  expected_products = 4
)
print(
  germany_competitor_growth_validation,
  width = Inf
)
table(
  germany_competitor_growth$
    germany_growth_position,
  useNA = "ifany"
)

############################################################
# Phase 8 - Step 8.1
# Analyze Germany's annual competitive leadership
############################################################

germany_leadership_summary <- germany_competitor_analysis %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    years_germany_leads =
      sum(
        competitor_leadership_status ==
          "Germany leads",
        na.rm = TRUE
      ),
    
    years_competitor_leads =
      sum(
        competitor_leadership_status ==
          "Competitor leads",
        na.rm = TRUE
      ),
    
    total_years =
      n(),
    
    leadership_rate =
      years_germany_leads /
      total_years,
    
    leadership_rate_pct =
      leadership_rate * 100,
    
    latest_leadership_status =
      competitor_leadership_status[
        which.max(year)
      ],
    
    latest_leading_competitor =
      leading_competitor[
        which.max(year)
      ],
    
    .groups = "drop"
  ) %>%
  arrange(
    hs2_code
  )
print(
  germany_leadership_summary,
  n = 4,
  width = Inf
)
############################################################
# Validate Germany competitive-leadership summary
############################################################

germany_leadership_validation <- validate_dataset(
  data = germany_leadership_summary,
  dataset_name = "Germany Competitive Leadership Summary",
  
  key_columns = c(
    "hs2_code"
  ),
  
  critical_columns = c(
    "hs2_code",
    "product_group",
    "years_germany_leads",
    "years_competitor_leads",
    "total_years",
    "leadership_rate",
    "leadership_rate_pct",
    "latest_leadership_status",
    "latest_leading_competitor"
  ),
  
  expected_rows = 4,
  expected_products = 4
)
print(
  germany_leadership_validation,
  width = Inf
)
# Validating the annual counts 
leadership_interval_check <- germany_leadership_summary %>%
  mutate(
    classified_years =
      years_germany_leads +
      years_competitor_leads
  )
leadership_interval_check %>%
  select(
    hs2_code,
    years_germany_leads,
    years_competitor_leads,
    classified_years,
    total_years
  )
all(
  leadership_interval_check$classified_years ==
    leadership_interval_check$total_years
)
### Validate leadership percentages 
invalid_leadership_rates <- germany_leadership_summary %>%
  filter(
    leadership_rate < 0 |
      leadership_rate > 1 |
      leadership_rate_pct < 0 |
      leadership_rate_pct > 100
  )

nrow(
  invalid_leadership_rates
)

############################################################
# Phase 8 - Step 9
# Save validated historical-analysis summaries
############################################################

readr::write_csv(
  germany_export_trend_summary,
  "data_clean/germany_export_trend_summary_2010_2024.csv"
)

readr::write_csv(
  germany_export_trend_view,
  "data_clean/germany_export_trend_view_2010_2024.csv"
)

readr::write_csv(
  germany_turning_points,
  "data_clean/germany_historical_turning_points_2010_2024.csv"
)

readr::write_csv(
  germany_market_share_trend,
  "data_clean/germany_market_share_trend_2010_2024.csv"
)

readr::write_csv(
  germany_rank_trend,
  "data_clean/germany_global_rank_trend_2010_2024.csv"
)

readr::write_csv(
  germany_competitor_growth,
  "data_clean/germany_competitor_growth_analysis_2010_2024.csv"
)

readr::write_csv(
  germany_leadership_summary,
  "data_clean/germany_competitive_leadership_2010_2024.csv"
)

############################################################
# Validate Phase 8 summary files
############################################################

phase8_summary_files <- c(
  "data_clean/germany_export_trend_summary_2010_2024.csv",
  "data_clean/germany_export_trend_view_2010_2024.csv",
  "data_clean/germany_historical_turning_points_2010_2024.csv",
  "data_clean/germany_market_share_trend_2010_2024.csv",
  "data_clean/germany_global_rank_trend_2010_2024.csv",
  "data_clean/germany_competitor_growth_analysis_2010_2024.csv",
  "data_clean/germany_competitive_leadership_2010_2024.csv"
)

phase8_summary_file_check <- tibble::tibble(
  file = basename(phase8_summary_files),
  
  exists =
    file.exists(phase8_summary_files),
  
  size_bytes =
    file.info(phase8_summary_files)$size
)

phase8_summary_file_check
#############################
# Create one PASS/FAIL result
phase8_summary_files_valid <- all(
  phase8_summary_file_check$exists &
    !is.na(phase8_summary_file_check$size_bytes) &
    phase8_summary_file_check$size_bytes > 0
)

phase8_summary_files_valid

############################################################
# Phase 8 - Step 10
# Validate row counts of saved historical-analysis summaries
############################################################

phase8_saved_row_counts <- tibble::tibble(
  file = basename(phase8_summary_files),
  
  rows = purrr::map_int(
    phase8_summary_files,
    ~ nrow(
      readr::read_csv(
        .x,
        show_col_types = FALSE
      )
    )
  )
)

phase8_saved_row_counts

############################################################
# Phase 8 - Step 11
# Validate saved summary files contain data rows
############################################################

phase8_saved_rows_valid <- all(
  phase8_saved_row_counts$rows > 0
)

phase8_saved_rows_valid
############################################################
# Phase 8 - Step 12
# Validate expected number of product-group rows
############################################################

phase8_expected_rows_valid <- all(
  phase8_saved_row_counts$rows == 4
)

phase8_expected_rows_valid

############################################################
# Phase 8 - Step 13
# Final validation of Phase 8 saved summary outputs
############################################################

phase8_final_validation <- tibble::tibble(
  validation = c(
    "Files exist and are non-empty",
    "Saved files contain data rows",
    "Expected product-group row count"
  ),
  
  passed = c(
    phase8_summary_files_valid,
    phase8_saved_rows_valid,
    phase8_expected_rows_valid
  )
)

phase8_final_validation

############################################################
# Phase 9 - Step 1
# Inspect historical data for forecasting preparation
############################################################

names(global_market_position_mysql)

dim(global_market_position_mysql)

range(
  global_market_position_mysql$year,
  na.rm = TRUE
)

sort(
  unique(global_market_position_mysql$hs2_code)
)
############################################################
# Phase 9 - Step 2
# Check historical year coverage by HS2 sector
############################################################

phase9_sector_coverage <- global_market_position_mysql %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    minimum_year = min(year, na.rm = TRUE),
    maximum_year = max(year, na.rm = TRUE),
    number_of_years = dplyr::n_distinct(year),
    .groups = "drop"
  ) %>%
  dplyr::arrange(hs2_code)

phase9_sector_coverage
# Inconsistancy for products' descriptions
##########################################
############################################################
# Phase 9 - Step 2A
# Investigate duplicate product descriptions for HS2 84
############################################################

phase9_hs84_description_check <- global_market_position_mysql %>%
  dplyr::filter(
    hs2_code == "84"
  ) %>%
  dplyr::group_by(
    product_group
  ) %>%
  dplyr::summarise(
    minimum_year = min(year, na.rm = TRUE),
    maximum_year = max(year, na.rm = TRUE),
    number_of_years = dplyr::n_distinct(year),
    number_of_records = dplyr::n(),
    .groups = "drop"
  )

phase9_hs84_description_check

############################################################
# Phase 9 - Step 2B
# Check duplicate country-year records for HS2 84
############################################################

phase9_hs84_duplicates <- global_market_position_mysql %>%
  dplyr::filter(
    hs2_code == "84"
  ) %>%
  dplyr::count(
    year,
    country_code,
    hs2_code,
    name = "record_count"
  ) %>%
  dplyr::filter(
    record_count > 1
  )

nrow(phase9_hs84_duplicates)

############################################################
# Phase 9 - Step 2C
# Inspect HS2 84 product descriptions by year
############################################################

phase9_hs84_year_labels <- global_market_position_mysql %>%
  dplyr::filter(
    hs2_code == "84"
  ) %>%
  dplyr::distinct(
    year,
    product_group
  ) %>%
  dplyr::arrange(
    year,
    product_group
  )

print(
  phase9_hs84_year_labels,
  n = Inf
)

############################################################
# Phase 9 - Step 2D.1
# Standardize HS2 code type in sector lookup
############################################################

sector_lookup_mysql <- sector_lookup_mysql %>%
  dplyr::mutate(
    hs2_code = as.character(hs2_code)
  )

class(global_market_position_mysql$hs2_code)

class(sector_lookup_mysql$hs2_code)

############################################################
# Phase 9 - Step 2D
# Create standardized forecasting base using sector lookup
############################################################

phase9_forecasting_base <- global_market_position_mysql %>%
  dplyr::select(
    -product_group
  ) %>%
  dplyr::left_join(
    sector_lookup_mysql,
    by = "hs2_code"
  ) %>%
  dplyr::relocate(
    product_group,
    .after = hs2_code
  )

dim(phase9_forecasting_base)

names(phase9_forecasting_base)

sort(
  unique(phase9_forecasting_base$product_group)
)

############################################################
# Phase 9 - Step 2E
# Revalidate historical coverage after label standardization
############################################################

phase9_sector_coverage_corrected <- phase9_forecasting_base %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    minimum_year = min(year, na.rm = TRUE),
    maximum_year = max(year, na.rm = TRUE),
    number_of_years = dplyr::n_distinct(year),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

phase9_sector_coverage_corrected
############################################################
# Phase 9 - Step 2F
# Final validation of historical sector coverage
############################################################

phase9_sector_coverage_valid <- all(
  phase9_sector_coverage_corrected$minimum_year == 2010 &
    phase9_sector_coverage_corrected$maximum_year == 2024 &
    phase9_sector_coverage_corrected$number_of_years == 15
)

phase9_sector_coverage_valid

############################################################
# Phase 9 - Step 3
# Check critical missing values in forecasting base
############################################################

phase9_missing_check <- phase9_forecasting_base %>%
  dplyr::summarise(
    missing_year =
      sum(is.na(year)),
    
    missing_hs2 =
      sum(is.na(hs2_code)),
    
    missing_product_group =
      sum(is.na(product_group)),
    
    missing_export_value =
      sum(is.na(export_value_usd)),
    
    missing_global_total =
      sum(is.na(global_total_exports_usd)),
    
    missing_market_share =
      sum(is.na(global_market_share)),
    
    missing_global_rank =
      sum(is.na(global_rank))
  )

print(
  phase9_missing_check,
  width = Inf
)

############################################################
# Phase 9 - Step 4
# Validate export values in forecasting base
############################################################

phase9_export_value_check <- phase9_forecasting_base %>%
  dplyr::summarise(
    zero_export_values =
      sum(export_value_usd == 0, na.rm = TRUE),
    
    negative_export_values =
      sum(export_value_usd < 0, na.rm = TRUE),
    
    positive_export_values =
      sum(export_value_usd > 0, na.rm = TRUE)
  )

print(
  phase9_export_value_check,
  width = Inf
)

############################################################
# Phase 9 - Step 5
# Validate global market share
############################################################

phase9_market_share_check <- phase9_forecasting_base %>%
  dplyr::summarise(
    minimum_market_share =
      min(global_market_share, na.rm = TRUE),
    
    maximum_market_share =
      max(global_market_share, na.rm = TRUE),
    
    negative_market_share =
      sum(global_market_share < 0, na.rm = TRUE),
    
    above_one_market_share =
      sum(global_market_share > 1, na.rm = TRUE)
  )

print(
  phase9_market_share_check,
  width = Inf
)

############################################################
# Phase 9 - Step 6
# Validate global rank values
############################################################

phase9_rank_check <- phase9_forecasting_base %>%
  dplyr::summarise(
    minimum_global_rank =
      min(global_rank, na.rm = TRUE),
    
    maximum_global_rank =
      max(global_rank, na.rm = TRUE),
    
    zero_global_rank =
      sum(global_rank == 0, na.rm = TRUE),
    
    negative_global_rank =
      sum(global_rank < 0, na.rm = TRUE)
  )

print(
  phase9_rank_check,
  width = Inf
)

############################################################
# Phase 9 - Step 7
# Validate global total export values
############################################################

phase9_global_total_check <- phase9_forecasting_base %>%
  dplyr::summarise(
    zero_global_totals =
      sum(global_total_exports_usd == 0, na.rm = TRUE),
    
    negative_global_totals =
      sum(global_total_exports_usd < 0, na.rm = TRUE),
    
    positive_global_totals =
      sum(global_total_exports_usd > 0, na.rm = TRUE)
  )

print(
  phase9_global_total_check,
  width = Inf
)

############################################################
# Phase 9 - Step 8
# Validate reporting-country counts
############################################################

phase9_reporting_country_check <- phase9_forecasting_base %>%
  dplyr::summarise(
    minimum_reporting_countries =
      min(reporting_country_count, na.rm = TRUE),
    
    maximum_reporting_countries =
      max(reporting_country_count, na.rm = TRUE),
    
    zero_reporting_countries =
      sum(reporting_country_count == 0, na.rm = TRUE),
    
    negative_reporting_countries =
      sum(reporting_country_count < 0, na.rm = TRUE)
  )

print(
  phase9_reporting_country_check,
  width = Inf
)
############################################################
# Phase 9 - Step 9
# Create Germany historical forecasting series
############################################################

germany_forecasting_series <- phase9_forecasting_base %>%
  dplyr::filter(
    country_iso3 == "DEU"
  ) %>%
  dplyr::select(
    year,
    hs2_code,
    product_group,
    export_value_usd,
    global_total_exports_usd,
    global_market_share,
    global_rank,
    reporting_country_count
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

dim(germany_forecasting_series)

dplyr::glimpse(germany_forecasting_series)

############################################################
# Phase 9 - Step 10
# Check duplicate Germany sector-year observations
############################################################

phase9_germany_duplicates <- germany_forecasting_series %>%
  dplyr::count(
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  dplyr::filter(
    record_count > 1
  )

nrow(phase9_germany_duplicates)

############################################################
# Phase 9 - Step 11
# Validate complete Germany time series by sector
############################################################

phase9_germany_series_coverage <- germany_forecasting_series %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    first_year = min(year),
    last_year = max(year),
    number_of_years = dplyr::n_distinct(year),
    number_of_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

print(
  phase9_germany_series_coverage,
  width = Inf
)
############################################################
# Phase 9 - Step 12
# Final validation of Germany historical forecasting series
############################################################

phase9_germany_series_valid <- all(
  phase9_germany_series_coverage$first_year == 2010 &
    phase9_germany_series_coverage$last_year == 2024 &
    phase9_germany_series_coverage$number_of_years == 15 &
    phase9_germany_series_coverage$number_of_records == 15
)

phase9_germany_series_valid

############################################################
# Phase 9 - Step 13
# Save validated Germany historical forecasting series
############################################################

readr::write_csv(
  germany_forecasting_series,
  "data_clean/germany_forecasting_series_2010_2024.csv"
)

phase9_forecasting_file_check <- tibble::tibble(
  file = "germany_forecasting_series_2010_2024.csv",
  
  exists = file.exists(
    "data_clean/germany_forecasting_series_2010_2024.csv"
  ),
  
  size_bytes = file.info(
    "data_clean/germany_forecasting_series_2010_2024.csv"
  )$size
)

phase9_forecasting_file_check
############################################################
# Phase 9 - Step 14
# Read back and validate saved Germany forecasting series
############################################################

germany_forecasting_series_check <- readr::read_csv(
  "data_clean/germany_forecasting_series_2010_2024.csv",
  show_col_types = FALSE
)

dim(germany_forecasting_series_check)

phase9_saved_forecasting_data_valid <- all(
  nrow(germany_forecasting_series_check) == 60,
  ncol(germany_forecasting_series_check) == 8,
  dplyr::n_distinct(
    germany_forecasting_series_check$hs2_code
  ) == 4,
  min(
    germany_forecasting_series_check$year,
    na.rm = TRUE
  ) == 2010,
  max(
    germany_forecasting_series_check$year,
    na.rm = TRUE
  ) == 2024
)

phase9_saved_forecasting_data_valid

############################################################
# Phase 9 - Step 15
# Compare original and saved forecasting datasets
############################################################

phase9_original_sorted <- germany_forecasting_series %>%
  dplyr::arrange(
    hs2_code,
    year
  )

phase9_saved_sorted <- germany_forecasting_series_check %>%
  dplyr::arrange(
    hs2_code,
    year
  )

phase9_saved_matches_original <- isTRUE(
  all.equal(
    phase9_original_sorted,
    phase9_saved_sorted,
    check.attributes = FALSE
  )
)
# Did not pass
#########################################
############################################################
# Phase 9 - Step 15A
# Diagnose differences between original and saved datasets
############################################################

phase9_comparison_details <- all.equal(
  phase9_original_sorted,
  phase9_saved_sorted,
  check.attributes = FALSE
)

phase9_comparison_details
############################################################
# Phase 9 - Step 15A
# Correct HS2 type and repeat saved-data comparison
############################################################

phase9_saved_sorted <- germany_forecasting_series_check %>%
  dplyr::mutate(
    hs2_code = as.character(hs2_code)
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

phase9_comparison_details <- all.equal(
  phase9_original_sorted,
  phase9_saved_sorted,
  check.attributes = FALSE
)

phase9_comparison_details
############################################################
# Phase 9 - Step 15B
# Store final saved-versus-original validation result
############################################################

phase9_saved_matches_original <- isTRUE(
  phase9_comparison_details
)

phase9_saved_matches_original

############################################################
# Phase 9 - Step 16
# Final validation summary for forecasting preparation
############################################################

phase9_preparation_validation <- tibble::tibble(
  validation = c(
    "Historical sector coverage complete",
    "Germany historical series complete",
    "Forecasting CSV saved correctly",
    "Saved dataset structure valid",
    "Saved dataset matches original"
  ),
  
  passed = c(
    phase9_sector_coverage_valid,
    phase9_germany_series_valid,
    phase9_forecasting_file_check$exists &
      phase9_forecasting_file_check$size_bytes > 0,
    phase9_saved_forecasting_data_valid,
    phase9_saved_matches_original
  )
)

phase9_preparation_validation
phase9_saved_matches_original

############################################################
# Phase 9 - Step 16
# Final PASS/FAIL for forecasting preparation
############################################################

phase9_preparation_complete <- all(
  phase9_preparation_validation$passed
)

phase9_preparation_complete

############################################################
# Phase 9 - Step 16
# Final validation summary for forecasting preparation
############################################################

phase9_preparation_validation <- tibble::tibble(
  validation = c(
    "Historical sector coverage complete",
    "Germany historical series complete",
    "Forecasting CSV saved correctly",
    "Saved dataset structure valid",
    "Saved dataset matches original"
  ),
  
  passed = c(
    phase9_sector_coverage_valid,
    phase9_germany_series_valid,
    phase9_forecasting_file_check$exists &
      phase9_forecasting_file_check$size_bytes > 0,
    phase9_saved_forecasting_data_valid,
    phase9_saved_matches_original
  )
)

phase9_preparation_validation

############################################################
# Phase 10 - Step 1
# Check required forecasting packages
############################################################

phase10_required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "readr",
  "forecast"
)

phase10_package_check <- tibble::tibble(
  package = phase10_required_packages,
  
  installed = purrr::map_lgl(
    phase10_required_packages,
    requireNamespace,
    quietly = TRUE
  )
)

phase10_package_check

############################################################
# Phase 10 - Step 2
# Load required forecasting packages
############################################################

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(forecast)

phase10_packages_loaded <- all(
  purrr::map_lgl(
    phase10_required_packages,
    ~ paste0("package:", .x) %in% search()
  )
)

phase10_packages_loaded

############################################################
# Phase 10 - Step 3
# Create training and test datasets
############################################################

germany_forecasting_train <- germany_forecasting_series %>%
  dplyr::filter(
    year <= 2021
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

germany_forecasting_test <- germany_forecasting_series %>%
  dplyr::filter(
    year >= 2022
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

dim(germany_forecasting_train)

dim(germany_forecasting_test)

############################################################
# Phase 10 - Step 4
# Validate training and test coverage by sector
############################################################

phase10_split_coverage <- germany_forecasting_series %>%
  dplyr::mutate(
    dataset = dplyr::if_else(
      year <= 2021,
      "Training",
      "Test"
    )
  ) %>%
  dplyr::group_by(
    hs2_code,
    product_group,
    dataset
  ) %>%
  dplyr::summarise(
    first_year = min(year),
    last_year = max(year),
    number_of_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    dataset
  )

print(
  phase10_split_coverage,
  width = Inf
)

############################################################
# Phase 10 - Step 5
# Final validation of training and test split
############################################################

phase10_split_valid <- all(
  phase10_split_coverage$number_of_records[
    phase10_split_coverage$dataset == "Training"
  ] == 12,
  
  phase10_split_coverage$first_year[
    phase10_split_coverage$dataset == "Training"
  ] == 2010,
  
  phase10_split_coverage$last_year[
    phase10_split_coverage$dataset == "Training"
  ] == 2021,
  
  phase10_split_coverage$number_of_records[
    phase10_split_coverage$dataset == "Test"
  ] == 3,
  
  phase10_split_coverage$first_year[
    phase10_split_coverage$dataset == "Test"
  ] == 2022,
  
  phase10_split_coverage$last_year[
    phase10_split_coverage$dataset == "Test"
  ] == 2024
)

phase10_split_valid

############################################################
# Phase 10 - Step 6
# Create export-value training time series by HS2 sector
############################################################

phase10_training_series <- germany_forecasting_train %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    training_ts = list(
      stats::ts(
        export_value_usd,
        start = min(year),
        frequency = 1
      )
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

phase10_training_series_check <- phase10_training_series %>%
  dplyr::mutate(
    series_length = purrr::map_int(
      training_ts,
      length
    ),
    
    start_year = purrr::map_int(
      training_ts,
      ~ as.integer(stats::start(.x)[1])
    ),
    
    end_year = purrr::map_int(
      training_ts,
      ~ as.integer(stats::end(.x)[1])
    ),
    
    frequency = purrr::map_int(
      training_ts,
      stats::frequency
    )
  ) %>%
  dplyr::select(
    hs2_code,
    product_group,
    series_length,
    start_year,
    end_year,
    frequency
  )

print(
  phase10_training_series_check,
  width = Inf
)

############################################################
# Phase 10 - Step 7
# Final validation of sector training time series
############################################################

phase10_training_series_valid <- all(
  phase10_training_series_check$series_length == 12 &
    phase10_training_series_check$start_year == 2010 &
    phase10_training_series_check$end_year == 2021 &
    phase10_training_series_check$frequency == 1
)

phase10_training_series_valid

############################################################
# Phase 10 - Step 8
# Create actual test values by HS2 sector
############################################################

phase10_test_values <- germany_forecasting_test %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    actual_test_values = list(
      export_value_usd[order(year)]
    ),
    
    test_years = list(
      year[order(year)]
    ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

############################################################
# Phase 10
# Validate test values by HS2 sector
############################################################

phase10_test_values_valid <- all(
  phase10_test_values_check$number_of_test_values == 3 &
    phase10_test_values_check$first_test_year == 2022 &
    phase10_test_values_check$last_test_year == 2024
)

phase10_test_values_valid

phase10_test_values_check <- phase10_test_values %>%
  dplyr::mutate(
    number_of_test_values = purrr::map_int(
      actual_test_values,
      length
    ),
    
    first_test_year = purrr::map_int(
      test_years,
      min
    ),
    
    last_test_year = purrr::map_int(
      test_years,
      max
    )
  ) %>%
  dplyr::select(
    hs2_code,
    product_group,
    number_of_test_values,
    first_test_year,
    last_test_year
  )

print(
  phase10_test_values_check,
  width = Inf
  
)

############################################################
# Phase 10
# Fit benchmark models and forecast the test period
############################################################

phase10_benchmark_models <- phase10_training_series %>%
  dplyr::mutate(
    naive_forecast = purrr::map(
      training_ts,
      ~ forecast::naive(
        .x,
        h = 3
      )
    ),
    
    drift_forecast = purrr::map(
      training_ts,
      ~ forecast::rwf(
        .x,
        h = 3,
        drift = TRUE
      )
    ),
    
    ets_forecast = purrr::map(
      training_ts,
      ~ forecast::forecast(
        forecast::ets(.x),
        h = 3
      )
    )
  )

phase10_benchmark_model_check <- phase10_benchmark_models %>%
  dplyr::transmute(
    hs2_code,
    product_group,
    
    naive_forecast_length = purrr::map_int(
      naive_forecast,
      ~ length(.x$mean)
    ),
    
    drift_forecast_length = purrr::map_int(
      drift_forecast,
      ~ length(.x$mean)
    ),
    
    ets_forecast_length = purrr::map_int(
      ets_forecast,
      ~ length(.x$mean)
    )
  )

print(
  phase10_benchmark_model_check,
  width = Inf
)

############################################################
# Phase 10
# Validate benchmark forecast lengths
############################################################

phase10_benchmark_models_valid <- all(
  phase10_benchmark_model_check$naive_forecast_length == 3 &
    phase10_benchmark_model_check$drift_forecast_length == 3 &
    phase10_benchmark_model_check$ets_forecast_length == 3
)

phase10_benchmark_models_valid

############################################################
# Phase 10
# Extract benchmark forecasts and actual test values
############################################################

phase10_benchmark_predictions <- phase10_benchmark_models %>%
  dplyr::transmute(
    hs2_code,
    product_group,
    
    year = purrr::map(
      naive_forecast,
      ~ as.integer(stats::time(.x$mean))
    ),
    
    naive_prediction = purrr::map(
      naive_forecast,
      ~ as.numeric(.x$mean)
    ),
    
    drift_prediction = purrr::map(
      drift_forecast,
      ~ as.numeric(.x$mean)
    ),
    
    ets_prediction = purrr::map(
      ets_forecast,
      ~ as.numeric(.x$mean)
    )
  ) %>%
  tidyr::unnest(
    cols = c(
      year,
      naive_prediction,
      drift_prediction,
      ets_prediction
    )
  ) %>%
  dplyr::left_join(
    germany_forecasting_test %>%
      dplyr::select(
        year,
        hs2_code,
        actual_export_value = export_value_usd
      ),
    by = c(
      "year",
      "hs2_code"
    )
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

dim(phase10_benchmark_predictions)

dplyr::glimpse(phase10_benchmark_predictions)
#################################################
# Before calculating accuracy metrics , confirm that 
# none of actual or predicted values is missing.
##################################################

############################################################
# Phase 10
# Validate benchmark prediction completeness
############################################################

phase10_prediction_missing_check <- phase10_benchmark_predictions %>%
  dplyr::summarise(
    missing_year =
      sum(is.na(year)),
    
    missing_actual =
      sum(is.na(actual_export_value)),
    
    missing_naive =
      sum(is.na(naive_prediction)),
    
    missing_drift =
      sum(is.na(drift_prediction)),
    
    missing_ets =
      sum(is.na(ets_prediction))
  )

print(
  phase10_prediction_missing_check,
  width = Inf
)

############################################################
# Phase 10
# Calculate benchmark-model accuracy by HS2 sector
############################################################

phase10_model_accuracy <- phase10_benchmark_predictions %>%
  tidyr::pivot_longer(
    cols = c(
      naive_prediction,
      drift_prediction,
      ets_prediction
    ),
    names_to = "model",
    values_to = "predicted_export_value"
  ) %>%
  dplyr::mutate(
    model = dplyr::recode(
      model,
      naive_prediction = "Naive",
      drift_prediction = "Drift",
      ets_prediction = "ETS"
    ),
    
    absolute_error = abs(
      actual_export_value - predicted_export_value
    ),
    
    squared_error = (
      actual_export_value - predicted_export_value
    )^2,
    
    absolute_percentage_error = abs(
      (
        actual_export_value - predicted_export_value
      ) / actual_export_value
    ) * 100
  ) %>%
  dplyr::group_by(
    hs2_code,
    product_group,
    model
  ) %>%
  dplyr::summarise(
    MAE = mean(absolute_error),
    RMSE = sqrt(mean(squared_error)),
    MAPE = mean(absolute_percentage_error),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    MAPE
  )

print(
  phase10_model_accuracy,
  width = Inf,
  n = Inf
)
############################################################
# Phase 10
# Select best benchmark model for each HS2 sector
############################################################

phase10_best_models <- phase10_model_accuracy %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::arrange(
    RMSE,
    MAE,
    MAPE,
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(
    n = 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    selected_model = model
  ) %>%
  dplyr::arrange(
    hs2_code
  )

print(
  phase10_best_models,
  width = Inf
)

############################################################
# Phase 10
# Validate best-model selection
############################################################

phase10_best_models_valid <- all(
  nrow(phase10_best_models) == 4,
  
  dplyr::n_distinct(
    phase10_best_models$hs2_code
  ) == 4,
  
  !is.na(
    phase10_best_models$selected_model
  ),
  
  phase10_best_models$selected_model %in%
    c(
      "Naive",
      "Drift",
      "ETS"
    ),
  
  is.finite(
    phase10_best_models$MAE
  ),
  
  is.finite(
    phase10_best_models$RMSE
  ),
  
  is.finite(
    phase10_best_models$MAPE
  )
)

phase10_best_models_valid

############################################################
# Phase 10
# Validate best-model selection
############################################################

phase10_best_models_valid <- all(
  nrow(phase10_best_models) == 4,
  
  dplyr::n_distinct(
    phase10_best_models$hs2_code
  ) == 4,
  
  !is.na(
    phase10_best_models$selected_model
  ),
  
  phase10_best_models$selected_model %in%
    c(
      "Naive",
      "Drift",
      "ETS"
    ),
  
  is.finite(
    phase10_best_models$MAE
  ),
  
  is.finite(
    phase10_best_models$RMSE
  ),
  
  is.finite(
    phase10_best_models$MAPE
  )
)

phase10_best_models_valid

############################################################
# Phase 10
# Create full 2010-2024 time series for final model refitting
############################################################

phase10_full_series <- germany_forecasting_series %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    full_ts = list(
      stats::ts(
        export_value_usd,
        start = min(year),
        frequency = 1
      )
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    phase10_best_models %>%
      dplyr::select(
        hs2_code,
        selected_model
      ),
    by = "hs2_code"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

phase10_full_series_check <- phase10_full_series %>%
  dplyr::transmute(
    hs2_code,
    product_group,
    selected_model,
    
    series_length = purrr::map_int(
      full_ts,
      length
    ),
    
    start_year = purrr::map_int(
      full_ts,
      ~ as.integer(stats::start(.x)[1])
    ),
    
    end_year = purrr::map_int(
      full_ts,
      ~ as.integer(stats::end(.x)[1])
    ),
    
    frequency = purrr::map_int(
      full_ts,
      stats::frequency
    )
  )

print(
  phase10_full_series_check,
  width = Inf
)

############################################################
# Phase 10
# Validate full time series for final model refitting
############################################################

phase10_full_series_valid <- all(
  nrow(phase10_full_series_check) == 4,
  
  phase10_full_series_check$series_length == 15,
  
  phase10_full_series_check$start_year == 2010,
  
  phase10_full_series_check$end_year == 2024,
  
  phase10_full_series_check$frequency == 1,
  
  !is.na(
    phase10_full_series_check$selected_model
  )
)

phase10_full_series_valid

############################################################
# Phase 11 - Step 1
# Verify objects required for final forecasting
############################################################

phase11_required_objects <- c(
  "germany_forecasting_series",
  "phase10_best_models"
)

phase11_object_check <- tibble::tibble(
  object = phase11_required_objects,
  exists = purrr::map_lgl(
    phase11_required_objects,
    exists
  )
)

phase11_object_check

phase11_objects_valid <- all(
  phase11_object_check$exists
)

phase11_objects_valid

############################################################
# Phase 11 - Step 2
# Refit selected models using full 2010-2024 series
############################################################

phase11_final_models <- germany_forecasting_series %>%
  dplyr::arrange(
    hs2_code,
    year
  ) %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  tidyr::nest() %>%
  dplyr::left_join(
    phase10_best_models %>%
      dplyr::select(
        hs2_code,
        selected_model
      ),
    by = "hs2_code"
  ) %>%
  dplyr::mutate(
    
    full_series = purrr::map(
      data,
      ~ stats::ts(
        .x$export_value_usd,
        start = min(.x$year),
        frequency = 1
      )
    ),
    
    final_model = purrr::map2(
      full_series,
      selected_model,
      ~ switch(
        .y,
        
        "Naive" = forecast::naive(
          .x,
          h = 5
        ),
        
        "Drift" = forecast::rwf(
          .x,
          h = 5,
          drift = TRUE
        ),
        
        "ETS" = forecast::forecast(
          forecast::ets(.x),
          h = 5
        )
      )
    )
  )

phase11_final_models %>%
  dplyr::select(
    hs2_code,
    product_group,
    selected_model
  )

############################################################
# Phase 11 - Step 3
# Validate final refitted models
############################################################

phase11_final_models_check <- phase11_final_models %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    hs2_code,
    product_group,
    selected_model,
    
    model_created = purrr::map_lgl(
      final_model,
      ~ !is.null(.x)
    ),
    
    forecast_length = purrr::map_int(
      final_model,
      ~ length(.x$mean)
    )
  )

phase11_final_models_check

phase11_final_models_valid <- all(
  nrow(phase11_final_models_check) == 4,
  phase11_final_models_check$model_created,
  phase11_final_models_check$forecast_length == 5
)

phase11_final_models_valid

############################################################
# Phase 11
# Extract final export forecasts for 2025-2029
############################################################

phase11_final_forecasts <- phase11_final_models %>%
  dplyr::transmute(
    hs2_code,
    product_group,
    selected_model,
    
    year = purrr::map(
      final_forecast,
      ~ as.integer(stats::time(.x$mean))
    ),
    
    forecast_export_value_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$mean)
    ),
    
    lower_80_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$lower[, "80%"])
    ),
    
    upper_80_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$upper[, "80%"])
    ),
    
    lower_95_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$lower[, "95%"])
    ),
    
    upper_95_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$upper[, "95%"])
    )
  ) %>%
  tidyr::unnest(
    cols = c(
      year,
      forecast_export_value_usd,
      lower_80_usd,
      upper_80_usd,
      lower_95_usd,
      upper_95_usd
    )
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

dim(phase11_final_forecasts)

dplyr::glimpse(phase11_final_forecasts)
# did not run
#########################################
############################################################
# Phase 11
# Inspect final model object column names
############################################################

names(phase11_final_models)

############################################################
# Phase 11
# Extract final export forecasts for 2025-2029
############################################################

phase11_final_forecasts <- phase11_final_models %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    hs2_code,
    product_group,
    selected_model,
    
    year = purrr::map(
      final_model,
      ~ as.integer(stats::time(.x$mean))
    ),
    
    forecast_export_value_usd = purrr::map(
      final_model,
      ~ as.numeric(.x$mean)
    ),
    
    lower_80_usd = purrr::map(
      final_model,
      ~ as.numeric(.x$lower[, "80%"])
    ),
    
    upper_80_usd = purrr::map(
      final_model,
      ~ as.numeric(.x$upper[, "80%"])
    ),
    
    lower_95_usd = purrr::map(
      final_model,
      ~ as.numeric(.x$lower[, "95%"])
    ),
    
    upper_95_usd = purrr::map(
      final_model,
      ~ as.numeric(.x$upper[, "95%"])
    )
  ) %>%
  tidyr::unnest(
    cols = c(
      year,
      forecast_export_value_usd,
      lower_80_usd,
      upper_80_usd,
      lower_95_usd,
      upper_95_usd
    )
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

dim(phase11_final_forecasts)

dplyr::glimpse(phase11_final_forecasts)

############################################################
# Phase 11
# Validate final 2025-2029 forecast structure
############################################################

phase11_final_forecast_check <- phase11_final_forecasts %>%
  dplyr::group_by(
    hs2_code,
    product_group,
    selected_model
  ) %>%
  dplyr::summarise(
    first_forecast_year = min(year),
    last_forecast_year = max(year),
    number_of_forecast_years = dplyr::n_distinct(year),
    number_of_forecast_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

print(
  phase11_final_forecast_check,
  width = Inf
)

############################################################
# Phase 11
# Final validation of forecast structure
############################################################

phase11_final_forecast_structure_valid <- all(
  nrow(phase11_final_forecast_check) == 4,
  
  phase11_final_forecast_check$first_forecast_year == 2025,
  
  phase11_final_forecast_check$last_forecast_year == 2029,
  
  phase11_final_forecast_check$number_of_forecast_years == 5,
  
  phase11_final_forecast_check$number_of_forecast_records == 5
)

phase11_final_forecast_structure_valid

############################################################
# Phase 11
# Validate forecast values and prediction intervals
############################################################

phase11_forecast_value_check <- phase11_final_forecasts %>%
  dplyr::summarise(
    missing_values = sum(
      is.na(forecast_export_value_usd) |
        is.na(lower_80_usd) |
        is.na(upper_80_usd) |
        is.na(lower_95_usd) |
        is.na(upper_95_usd)
    ),
    
    non_finite_values = sum(
      !is.finite(forecast_export_value_usd) |
        !is.finite(lower_80_usd) |
        !is.finite(upper_80_usd) |
        !is.finite(lower_95_usd) |
        !is.finite(upper_95_usd)
    ),
    
    non_positive_forecasts = sum(
      forecast_export_value_usd <= 0,
      na.rm = TRUE
    ),
    
    invalid_interval_order = sum(
      lower_95_usd > lower_80_usd |
        lower_80_usd > forecast_export_value_usd |
        forecast_export_value_usd > upper_80_usd |
        upper_80_usd > upper_95_usd,
      na.rm = TRUE
    )
  )

print(
  phase11_forecast_value_check,
  width = Inf
)

############################################################
# Phase 11
# Final validation of forecast values and intervals
############################################################

phase11_forecast_values_valid <- all(
  phase11_forecast_value_check$missing_values == 0,
  phase11_forecast_value_check$non_finite_values == 0,
  phase11_forecast_value_check$non_positive_forecasts == 0,
  phase11_forecast_value_check$invalid_interval_order == 0
)

phase11_forecast_values_valid
############################################################
# Phase 11
# Save validated final export forecasts for 2025-2029
############################################################

readr::write_csv(
  phase11_final_forecasts,
  "data_clean/germany_export_forecasts_2025_2029.csv"
)

phase11_forecast_file_check <- tibble::tibble(
  file = "germany_export_forecasts_2025_2029.csv",
  
  exists = file.exists(
    "data_clean/germany_export_forecasts_2025_2029.csv"
  ),
  
  size_bytes = file.info(
    "data_clean/germany_export_forecasts_2025_2029.csv"
  )$size
)

phase11_forecast_file_check


############################################################
# Phase 11
# Read back and validate saved final forecast file
############################################################

phase11_final_forecasts_check <- readr::read_csv(
  "data_clean/germany_export_forecasts_2025_2029.csv",
  col_types = readr::cols(
    hs2_code = readr::col_character(),
    .default = readr::col_guess()
  )
)

dim(phase11_final_forecasts_check)

phase11_saved_forecast_valid <- all(
  nrow(phase11_final_forecasts_check) == 20,
  
  ncol(phase11_final_forecasts_check) == 9,
  
  dplyr::n_distinct(
    phase11_final_forecasts_check$hs2_code
  ) == 4,
  
  min(
    phase11_final_forecasts_check$year,
    na.rm = TRUE
  ) == 2025,
  
  max(
    phase11_final_forecasts_check$year,
    na.rm = TRUE
  ) == 2029,
  
  !anyNA(
    phase11_final_forecasts_check
  )
)

phase11_saved_forecast_valid

############################################################
# Phase 12 - Step 1
# Confirm Phase 11 forecast output is fully validated
############################################################

phase11_final_output_valid <- all(
  phase11_final_models_valid,
  phase11_final_forecast_structure_valid,
  phase11_forecast_values_valid,
  phase11_forecast_file_check$exists,
  phase11_forecast_file_check$size_bytes > 0
)

phase11_final_output_valid

############################################################
# Phase 12 - Step 2
# Identify available competitor-analysis objects
############################################################

phase12_available_objects <- ls(
  envir = .GlobalEnv
)

phase12_competitor_objects <- phase12_available_objects[
  grepl(
    pattern = "competitor|benchmark|ranking|market_position",
    x = phase12_available_objects,
    ignore.case = TRUE
  )
]

phase12_competitor_objects

############################################################
# Phase 12 - Step 3
# Inspect existing competitor datasets
############################################################

phase12_object_structure <- tibble::tibble(
  object = c(
    "annual_competitor_benchmark",
    "competitor_cagr",
    "competitor_features",
    "competitor_rankings_db"
  ),
  
  rows = c(
    nrow(annual_competitor_benchmark),
    nrow(competitor_cagr),
    nrow(competitor_features),
    nrow(competitor_rankings_db)
  ),
  
  columns = c(
    ncol(annual_competitor_benchmark),
    ncol(competitor_cagr),
    ncol(competitor_features),
    ncol(competitor_rankings_db)
  )
)

phase12_object_structure

names(annual_competitor_benchmark)

names(competitor_cagr)

names(competitor_features)

names(competitor_rankings_db)

############################################################
# Phase 12 - Step 4
# Inspect competitor countries and historical coverage
############################################################

phase12_competitor_coverage <- competitor_features %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    number_of_years = dplyr::n_distinct(year),
    number_of_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    country
  )

print(
  phase12_competitor_coverage,
  width = Inf,
  n = Inf
)

############################################################
# Phase 12 - Step 5
# Validate competitor historical coverage
############################################################

phase12_competitor_coverage_valid <- all(
  nrow(phase12_competitor_coverage) == 24,
  
  dplyr::n_distinct(
    phase12_competitor_coverage$country
  ) == 6,
  
  dplyr::n_distinct(
    phase12_competitor_coverage$hs2_code
  ) == 4,
  
  phase12_competitor_coverage$first_year == 2010,
  
  phase12_competitor_coverage$last_year == 2024,
  
  phase12_competitor_coverage$number_of_years == 15,
  
  phase12_competitor_coverage$number_of_records == 15
)

phase12_competitor_coverage_valid

############################################################
# Phase 12 - Step 6
# Check duplicate country-sector-year records
############################################################

phase12_competitor_duplicates <- competitor_features %>%
  dplyr::count(
    country,
    hs2_code,
    year,
    name = "record_count"
  ) %>%
  dplyr::filter(
    record_count > 1
  )

nrow(phase12_competitor_duplicates)

############################################################
# Phase 12 - Step 7
# Check critical missing values in competitor data
############################################################

phase12_competitor_missing_check <- competitor_features %>%
  dplyr::summarise(
    missing_year =
      sum(is.na(year)),
    
    missing_country =
      sum(is.na(country)),
    
    missing_hs2 =
      sum(is.na(hs2_code)),
    
    missing_product_group =
      sum(is.na(product_group)),
    
    missing_export_value =
      sum(is.na(export_value_usd)),
    
    missing_comparison_share =
      sum(is.na(comparison_export_share)),
    
    missing_competitor_rank =
      sum(is.na(competitor_rank))
  )

print(
  phase12_competitor_missing_check,
  width = Inf
)

############################################################
# Phase 12 - Step 8
# Validate competitor export values, shares, and ranks
############################################################

phase12_competitor_value_check <- competitor_features %>%
  dplyr::summarise(
    zero_export_values =
      sum(export_value_usd == 0, na.rm = TRUE),
    
    negative_export_values =
      sum(export_value_usd < 0, na.rm = TRUE),
    
    minimum_comparison_share =
      min(comparison_export_share, na.rm = TRUE),
    
    maximum_comparison_share =
      max(comparison_export_share, na.rm = TRUE),
    
    invalid_comparison_share =
      sum(
        comparison_export_share < 0 |
          comparison_export_share > 1,
        na.rm = TRUE
      ),
    
    minimum_competitor_rank =
      min(competitor_rank, na.rm = TRUE),
    
    maximum_competitor_rank =
      max(competitor_rank, na.rm = TRUE),
    
    invalid_competitor_rank =
      sum(competitor_rank < 1, na.rm = TRUE)
  )

print(
  phase12_competitor_value_check,
  width = Inf
)

############################################################
# Phase 12 - Step 9
# Create standardized competitor forecasting base
############################################################

phase12_competitor_forecasting_base <- competitor_features %>%
  dplyr::transmute(
    year = as.integer(year),
    country,
    hs2_code = as.character(hs2_code),
    product_group,
    export_value_usd,
    comparison_total_exports_usd,
    comparison_export_share,
    competitor_rank
  ) %>%
  dplyr::arrange(
    hs2_code,
    country,
    year
  )

dim(phase12_competitor_forecasting_base)

dplyr::glimpse(
  phase12_competitor_forecasting_base
)

############################################################
# Phase 12 - Step 10
# Create competitor training and test datasets
############################################################

phase12_competitor_train <- phase12_competitor_forecasting_base %>%
  dplyr::filter(
    year <= 2021
  ) %>%
  dplyr::arrange(
    hs2_code,
    country,
    year
  )

phase12_competitor_test <- phase12_competitor_forecasting_base %>%
  dplyr::filter(
    year >= 2022
  ) %>%
  dplyr::arrange(
    hs2_code,
    country,
    year
  )

dim(phase12_competitor_train)

dim(phase12_competitor_test)

############################################################
# Phase 12 - Step 11
# Validate competitor train/test coverage
############################################################

phase12_split_coverage <- phase12_competitor_forecasting_base %>%
  dplyr::mutate(
    dataset = dplyr::if_else(
      year <= 2021,
      "Training",
      "Test"
    )
  ) %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group,
    dataset
  ) %>%
  dplyr::summarise(
    first_year = min(year),
    last_year = max(year),
    number_of_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    country,
    dataset
  )

phase12_split_valid <- all(
  phase12_split_coverage$number_of_records[
    phase12_split_coverage$dataset == "Training"
  ] == 12,
  
  phase12_split_coverage$first_year[
    phase12_split_coverage$dataset == "Training"
  ] == 2010,
  
  phase12_split_coverage$last_year[
    phase12_split_coverage$dataset == "Training"
  ] == 2021,
  
  phase12_split_coverage$number_of_records[
    phase12_split_coverage$dataset == "Test"
  ] == 3,
  
  phase12_split_coverage$first_year[
    phase12_split_coverage$dataset == "Test"
  ] == 2022,
  
  phase12_split_coverage$last_year[
    phase12_split_coverage$dataset == "Test"
  ] == 2024
)

phase12_split_valid

############################################################
# Phase 12 - Step 12
# Create competitor training time series
############################################################

phase12_training_series <- phase12_competitor_train %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    training_ts = list(
      stats::ts(
        export_value_usd,
        start = min(year),
        frequency = 1
      )
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    country
  )

phase12_training_series_check <- phase12_training_series %>%
  dplyr::mutate(
    series_length = purrr::map_int(
      training_ts,
      length
    ),
    
    start_year = purrr::map_int(
      training_ts,
      ~ as.integer(stats::start(.x)[1])
    ),
    
    end_year = purrr::map_int(
      training_ts,
      ~ as.integer(stats::end(.x)[1])
    ),
    
    frequency = purrr::map_int(
      training_ts,
      stats::frequency
    )
  ) %>%
  dplyr::select(
    country,
    hs2_code,
    product_group,
    series_length,
    start_year,
    end_year,
    frequency
  )

print(
  phase12_training_series_check,
  width = Inf,
  n = Inf
)

############################################################
# Phase 12 - Step 13
# Validate competitor training time series
############################################################

phase12_training_series_valid <- all(
  nrow(phase12_training_series_check) == 24,
  
  phase12_training_series_check$series_length == 12,
  
  phase12_training_series_check$start_year == 2010,
  
  phase12_training_series_check$end_year == 2021,
  
  phase12_training_series_check$frequency == 1
)

phase12_training_series_valid


############################################################
# Phase 12 - Step 14
# Fit benchmark models for all country-sector series
############################################################

phase12_benchmark_models <- phase12_training_series %>%
  dplyr::mutate(
    naive_forecast = purrr::map(
      training_ts,
      ~ forecast::naive(
        .x,
        h = 3
      )
    ),
    
    drift_forecast = purrr::map(
      training_ts,
      ~ forecast::rwf(
        .x,
        h = 3,
        drift = TRUE
      )
    ),
    
    ets_forecast = purrr::map(
      training_ts,
      ~ forecast::forecast(
        forecast::ets(.x),
        h = 3
      )
    )
  )

phase12_benchmark_model_check <- phase12_benchmark_models %>%
  dplyr::transmute(
    country,
    hs2_code,
    product_group,
    
    naive_forecast_length = purrr::map_int(
      naive_forecast,
      ~ length(.x$mean)
    ),
    
    drift_forecast_length = purrr::map_int(
      drift_forecast,
      ~ length(.x$mean)
    ),
    
    ets_forecast_length = purrr::map_int(
      ets_forecast,
      ~ length(.x$mean)
    )
  )

print(
  phase12_benchmark_model_check,
  width = Inf,
  n = Inf
)

############################################################
# Phase 12 - Step 15
# Validate competitor benchmark forecast lengths
############################################################

phase12_benchmark_models_valid <- all(
  nrow(phase12_benchmark_model_check) == 24,
  
  phase12_benchmark_model_check$naive_forecast_length == 3,
  
  phase12_benchmark_model_check$drift_forecast_length == 3,
  
  phase12_benchmark_model_check$ets_forecast_length == 3
)

phase12_benchmark_models_valid

############################################################
# Phase 12 - Step 16
# Extract competitor predictions and actual test values
############################################################

phase12_benchmark_predictions <- phase12_benchmark_models %>%
  dplyr::transmute(
    country,
    hs2_code,
    product_group,
    
    year = purrr::map(
      naive_forecast,
      ~ as.integer(stats::time(.x$mean))
    ),
    
    naive_prediction = purrr::map(
      naive_forecast,
      ~ as.numeric(.x$mean)
    ),
    
    drift_prediction = purrr::map(
      drift_forecast,
      ~ as.numeric(.x$mean)
    ),
    
    ets_prediction = purrr::map(
      ets_forecast,
      ~ as.numeric(.x$mean)
    )
  ) %>%
  tidyr::unnest(
    cols = c(
      year,
      naive_prediction,
      drift_prediction,
      ets_prediction
    )
  ) %>%
  dplyr::left_join(
    phase12_competitor_test %>%
      dplyr::select(
        country,
        hs2_code,
        year,
        actual_export_value = export_value_usd
      ),
    by = c(
      "country",
      "hs2_code",
      "year"
    )
  ) %>%
  dplyr::arrange(
    hs2_code,
    country,
    year
  )

dim(phase12_benchmark_predictions)

dplyr::glimpse(
  phase12_benchmark_predictions
)

############################################################
# Phase 12 - Step 17
# Validate competitor prediction completeness
############################################################

phase12_prediction_missing_check <- phase12_benchmark_predictions %>%
  dplyr::summarise(
    missing_country =
      sum(is.na(country)),
    
    missing_hs2 =
      sum(is.na(hs2_code)),
    
    missing_year =
      sum(is.na(year)),
    
    missing_actual =
      sum(is.na(actual_export_value)),
    
    missing_naive =
      sum(is.na(naive_prediction)),
    
    missing_drift =
      sum(is.na(drift_prediction)),
    
    missing_ets =
      sum(is.na(ets_prediction))
  )

print(
  phase12_prediction_missing_check,
  width = Inf
)

############################################################
# Phase 12 - Step 18
# Calculate model accuracy for each country-sector series
############################################################

phase12_model_accuracy <- phase12_benchmark_predictions %>%
  tidyr::pivot_longer(
    cols = c(
      naive_prediction,
      drift_prediction,
      ets_prediction
    ),
    names_to = "model",
    values_to = "predicted_export_value"
  ) %>%
  dplyr::mutate(
    model = dplyr::recode(
      model,
      naive_prediction = "Naive",
      drift_prediction = "Drift",
      ets_prediction = "ETS"
    ),
    
    absolute_error = abs(
      actual_export_value - predicted_export_value
    ),
    
    squared_error = (
      actual_export_value - predicted_export_value
    )^2,
    
    absolute_percentage_error = abs(
      (
        actual_export_value - predicted_export_value
      ) / actual_export_value
    ) * 100
  ) %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group,
    model
  ) %>%
  dplyr::summarise(
    MAE = mean(absolute_error),
    RMSE = sqrt(mean(squared_error)),
    MAPE = mean(absolute_percentage_error),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    country,
    RMSE
  )

dim(phase12_model_accuracy)

dplyr::glimpse(
  phase12_model_accuracy
)

############################################################
# Phase 12 - Step 19
# Select best model for each country-sector series
############################################################

phase12_best_models <- phase12_model_accuracy %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group
  ) %>%
  dplyr::arrange(
    RMSE,
    MAE,
    MAPE,
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(
    n = 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    selected_model = model
  ) %>%
  dplyr::arrange(
    hs2_code,
    country
  )

print(
  phase12_best_models,
  width = Inf,
  n = Inf
)

############################################################
# Phase 12 - Step 20
# Validate best-model selection for all country-sector series
############################################################

phase12_best_models_valid <- all(
  nrow(phase12_best_models) == 24,
  
  dplyr::n_distinct(
    paste(
      phase12_best_models$country,
      phase12_best_models$hs2_code,
      sep = "_"
    )
  ) == 24,
  
  !is.na(
    phase12_best_models$selected_model
  ),
  
  phase12_best_models$selected_model %in%
    c(
      "Naive",
      "Drift",
      "ETS"
    ),
  
  is.finite(
    phase12_best_models$MAE
  ),
  
  is.finite(
    phase12_best_models$RMSE
  ),
  
  is.finite(
    phase12_best_models$MAPE
  )
)

phase12_best_models_valid

############################################################
# Phase 12 - Step 21
# Create full competitor time series for final refitting
############################################################

phase12_full_series <- phase12_competitor_forecasting_base %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    full_ts = list(
      stats::ts(
        export_value_usd,
        start = min(year),
        frequency = 1
      )
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    phase12_best_models %>%
      dplyr::select(
        country,
        hs2_code,
        selected_model
      ),
    by = c(
      "country",
      "hs2_code"
    )
  ) %>%
  dplyr::arrange(
    hs2_code,
    country
  )

phase12_full_series_check <- phase12_full_series %>%
  dplyr::transmute(
    country,
    hs2_code,
    product_group,
    selected_model,
    
    series_length = purrr::map_int(
      full_ts,
      length
    ),
    
    start_year = purrr::map_int(
      full_ts,
      ~ as.integer(stats::start(.x)[1])
    ),
    
    end_year = purrr::map_int(
      full_ts,
      ~ as.integer(stats::end(.x)[1])
    ),
    
    frequency = purrr::map_int(
      full_ts,
      stats::frequency
    )
  )

print(
  phase12_full_series_check,
  width = Inf,
  n = Inf
)

############################################################
# Phase 12 - Step 22
# Validate full competitor time series
############################################################

phase12_full_series_valid <- all(
  nrow(phase12_full_series_check) == 24,
  
  phase12_full_series_check$series_length == 15,
  
  phase12_full_series_check$start_year == 2010,
  
  phase12_full_series_check$end_year == 2024,
  
  phase12_full_series_check$frequency == 1,
  
  !is.na(
    phase12_full_series_check$selected_model
  ),
  
  phase12_full_series_check$selected_model %in%
    c(
      "Naive",
      "Drift",
      "ETS"
    )
)

phase12_full_series_valid

############################################################
# Phase 12 - Step 23
# Fit selected models and forecast competitor exports
# for 2025-2029
############################################################

phase12_final_models <- phase12_full_series %>%
  dplyr::mutate(
    final_forecast = purrr::map2(
      full_ts,
      selected_model,
      ~ {
        if (.y == "Naive") {
          forecast::naive(
            .x,
            h = 5,
            level = c(80, 95)
          )
        } else if (.y == "Drift") {
          forecast::rwf(
            .x,
            h = 5,
            drift = TRUE,
            level = c(80, 95)
          )
        } else if (.y == "ETS") {
          forecast::forecast(
            forecast::ets(.x),
            h = 5,
            level = c(80, 95)
          )
        } else {
          stop(
            paste(
              "Unsupported model:",
              .y
            )
          )
        }
      }
    )
  )

phase12_final_models_check <- phase12_final_models %>%
  dplyr::transmute(
    country,
    hs2_code,
    product_group,
    selected_model,
    
    model_created = purrr::map_lgl(
      final_forecast,
      ~ inherits(.x, "forecast")
    ),
    
    forecast_length = purrr::map_int(
      final_forecast,
      ~ length(.x$mean)
    ),
    
    first_forecast_year = purrr::map_int(
      final_forecast,
      ~ as.integer(stats::start(.x$mean)[1])
    ),
    
    last_forecast_year = purrr::map_int(
      final_forecast,
      ~ as.integer(stats::end(.x$mean)[1])
    )
  )

print(
  phase12_final_models_check,
  width = Inf,
  n = Inf
)

############################################################
# Phase 12 - Step 24
# Validate final competitor models and forecast periods
############################################################

phase12_final_models_valid <- all(
  nrow(phase12_final_models_check) == 24,
  
  phase12_final_models_check$model_created,
  
  phase12_final_models_check$forecast_length == 5,
  
  phase12_final_models_check$first_forecast_year == 2025,
  
  phase12_final_models_check$last_forecast_year == 2029
)

phase12_final_models_valid

############################################################
# Phase 12 - Step 25
# Extract final competitor export forecasts for 2025-2029
############################################################

phase12_final_forecasts <- phase12_final_models %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    country,
    hs2_code,
    product_group,
    selected_model,
    
    year = purrr::map(
      final_forecast,
      ~ as.integer(stats::time(.x$mean))
    ),
    
    forecast_export_value_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$mean)
    ),
    
    lower_80_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$lower[, "80%"])
    ),
    
    upper_80_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$upper[, "80%"])
    ),
    
    lower_95_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$lower[, "95%"])
    ),
    
    upper_95_usd = purrr::map(
      final_forecast,
      ~ as.numeric(.x$upper[, "95%"])
    )
  ) %>%
  tidyr::unnest(
    cols = c(
      year,
      forecast_export_value_usd,
      lower_80_usd,
      upper_80_usd,
      lower_95_usd,
      upper_95_usd
    )
  ) %>%
  dplyr::arrange(
    hs2_code,
    year,
    dplyr::desc(forecast_export_value_usd)
  )

dim(phase12_final_forecasts)

dplyr::glimpse(
  phase12_final_forecasts
)

############################################################
# Phase 12 - Step 26
# Validate final competitor forecast structure
############################################################

phase12_final_forecast_check <- phase12_final_forecasts %>%
  dplyr::group_by(
    country,
    hs2_code,
    product_group,
    selected_model
  ) %>%
  dplyr::summarise(
    first_forecast_year = min(year),
    last_forecast_year = max(year),
    number_of_forecast_years = dplyr::n_distinct(year),
    number_of_forecast_records = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    country
  )

phase12_final_forecast_structure_valid <- all(
  nrow(phase12_final_forecast_check) == 24,
  
  phase12_final_forecast_check$first_forecast_year == 2025,
  
  phase12_final_forecast_check$last_forecast_year == 2029,
  
  phase12_final_forecast_check$number_of_forecast_years == 5,
  
  phase12_final_forecast_check$number_of_forecast_records == 5
)

phase12_final_forecast_structure_valid

############################################################
# Phase 12 - Step 27
# Validate competitor forecast values and intervals
############################################################

phase12_forecast_value_check <- phase12_final_forecasts %>%
  dplyr::summarise(
    missing_values = sum(
      is.na(forecast_export_value_usd) |
        is.na(lower_80_usd) |
        is.na(upper_80_usd) |
        is.na(lower_95_usd) |
        is.na(upper_95_usd)
    ),
    
    non_finite_values = sum(
      !is.finite(forecast_export_value_usd) |
        !is.finite(lower_80_usd) |
        !is.finite(upper_80_usd) |
        !is.finite(lower_95_usd) |
        !is.finite(upper_95_usd)
    ),
    
    non_positive_forecasts = sum(
      forecast_export_value_usd <= 0,
      na.rm = TRUE
    ),
    
    invalid_interval_order = sum(
      lower_95_usd > lower_80_usd |
        lower_80_usd > forecast_export_value_usd |
        forecast_export_value_usd > upper_80_usd |
        upper_80_usd > upper_95_usd,
      na.rm = TRUE
    )
  )

print(
  phase12_forecast_value_check,
  width = Inf
)
############################################################
# Phase 13 - Step 1
# Inspect validated competitor forecast dataset
############################################################

dim(
  phase12_final_forecasts
)

names(
  phase12_final_forecasts
)

dplyr::glimpse(
  phase12_final_forecasts
)
############################################################
# Phase 13 - Step 2
# Calculate projected comparison shares and rankings
############################################################

phase13_competitive_projection <- phase12_final_forecasts %>%
  dplyr::group_by(
    hs2_code,
    product_group,
    year
  ) %>%
  dplyr::mutate(
    projected_comparison_total_usd =
      sum(
        forecast_export_value_usd,
        na.rm = TRUE
      ),
    
    projected_comparison_share =
      forecast_export_value_usd /
      projected_comparison_total_usd,
    
    projected_competitor_rank =
      dplyr::min_rank(
        dplyr::desc(
          forecast_export_value_usd
        )
      )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    hs2_code,
    year,
    projected_competitor_rank
  )

dim(
  phase13_competitive_projection
)

dplyr::glimpse(
  phase13_competitive_projection
)
############################################################
# Phase 13 - Step 3
# Validate projected comparison shares and ranks
############################################################

phase13_projection_validation <- phase13_competitive_projection %>%
  dplyr::group_by(
    hs2_code,
    product_group,
    year
  ) %>%
  dplyr::summarise(
    country_count =
      dplyr::n_distinct(country),
    
    total_projected_share =
      sum(
        projected_comparison_share,
        na.rm = TRUE
      ),
    
    minimum_projected_rank =
      min(
        projected_competitor_rank,
        na.rm = TRUE
      ),
    
    maximum_projected_rank =
      max(
        projected_competitor_rank,
        na.rm = TRUE
      ),
    
    distinct_projected_ranks =
      dplyr::n_distinct(
        projected_competitor_rank
      ),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

phase13_projection_valid <- all(
  nrow(phase13_projection_validation) == 20,
  
  phase13_projection_validation$country_count == 6,
  
  abs(
    phase13_projection_validation$total_projected_share - 1
  ) < 1e-10,
  
  phase13_projection_validation$minimum_projected_rank == 1,
  
  phase13_projection_validation$maximum_projected_rank == 6,
  
  phase13_projection_validation$distinct_projected_ranks == 6
)

phase13_projection_valid

############################################################
# Phase 13 - Step 4
# Extract Germany projected competitive position
############################################################

phase13_germany_projection <- phase13_competitive_projection %>%
  dplyr::filter(
    country == "Germany"
  ) %>%
  dplyr::select(
    year,
    hs2_code,
    product_group,
    selected_model,
    forecast_export_value_usd,
    projected_comparison_total_usd,
    projected_comparison_share,
    projected_competitor_rank,
    lower_80_usd,
    upper_80_usd,
    lower_95_usd,
    upper_95_usd
  ) %>%
  dplyr::arrange(
    hs2_code,
    year
  )

dim(
  phase13_germany_projection
)

print(
  phase13_germany_projection,
  width = Inf,
  n = Inf
)

############################################################
# Phase 13 - Step 5
# Validate Germany projected competitive position
############################################################

phase13_germany_projection_check <- phase13_germany_projection %>%
  dplyr::group_by(
    hs2_code,
    product_group
  ) %>%
  dplyr::summarise(
    first_projection_year =
      min(year),
    
    last_projection_year =
      max(year),
    
    number_of_projection_years =
      dplyr::n_distinct(year),
    
    number_of_projection_records =
      dplyr::n(),
    
    minimum_projected_rank =
      min(projected_competitor_rank),
    
    maximum_projected_rank =
      max(projected_competitor_rank),
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    hs2_code
  )

print(
  phase13_germany_projection_check,
  width = Inf
)

############################################################
# Phase 13 - Step 6
# Final validation of Germany competitive projection
############################################################

phase13_germany_projection_valid <- all(
  nrow(phase13_germany_projection) == 20,
  
  nrow(phase13_germany_projection_check) == 4,
  
  phase13_germany_projection_check$first_projection_year == 2025,
  
  phase13_germany_projection_check$last_projection_year == 2029,
  
  phase13_germany_projection_check$number_of_projection_years == 5,
  
  phase13_germany_projection_check$number_of_projection_records == 5,
  
  phase13_germany_projection$projected_competitor_rank >= 1,
  
  phase13_germany_projection$projected_competitor_rank <= 6,
  
  phase13_germany_projection$projected_comparison_share >= 0,
  
  phase13_germany_projection$projected_comparison_share <= 1
)

phase13_germany_projection_valid


############################################################
# Phase 13 - Step 7
# Create Germany 2025 versus 2029 position summary
############################################################

phase13_germany_position_summary <- phase13_germany_projection %>%
  dplyr::filter(
    year %in% c(2025, 2029)
  ) %>%
  dplyr::select(
    hs2_code,
    product_group,
    year,
    forecast_export_value_usd,
    projected_comparison_share,
    projected_competitor_rank
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(
      forecast_export_value_usd,
      projected_comparison_share,
      projected_competitor_rank
    ),
    names_glue = "{.value}_{year}"
  ) %>%
  dplyr::mutate(
    forecast_value_change_usd =
      forecast_export_value_usd_2029 -
      forecast_export_value_usd_2025,
    
    forecast_growth_pct =
      (
        forecast_export_value_usd_2029 /
          forecast_export_value_usd_2025 - 1
      ) * 100,
    
    comparison_share_change_pct_points =
      (
        projected_comparison_share_2029 -
          projected_comparison_share_2025
      ) * 100,
    
    projected_rank_change =
      projected_competitor_rank_2029 -
      projected_competitor_rank_2025,
    
    projected_position_direction =
      dplyr::case_when(
        projected_rank_change < 0 ~ "Improved",
        projected_rank_change > 0 ~ "Declined",
        TRUE ~ "Unchanged"
      )
  ) %>%
  dplyr::arrange(
    hs2_code
  )

print(
  phase13_germany_position_summary,
  width = Inf
)

############################################################
# Phase 13 - Step 8
# Validate Germany 2025 versus 2029 position summary
############################################################

phase13_germany_position_summary_valid <- all(
  nrow(phase13_germany_position_summary) == 4,
  
  dplyr::n_distinct(
    phase13_germany_position_summary$hs2_code
  ) == 4,
  
  is.finite(
    phase13_germany_position_summary$
      forecast_export_value_usd_2025
  ),
  
  is.finite(
    phase13_germany_position_summary$
      forecast_export_value_usd_2029
  ),
  
  is.finite(
    phase13_germany_position_summary$
      forecast_growth_pct
  ),
  
  is.finite(
    phase13_germany_position_summary$
      comparison_share_change_pct_points
  ),
  
  phase13_germany_position_summary$
    projected_competitor_rank_2025 >= 1,
  
  phase13_germany_position_summary$
    projected_competitor_rank_2025 <= 6,
  
  phase13_germany_position_summary$
    projected_competitor_rank_2029 >= 1,
  
  phase13_germany_position_summary$
    projected_competitor_rank_2029 <= 6,
  
  phase13_germany_position_summary$
    projected_position_direction %in%
    c(
      "Improved",
      "Declined",
      "Unchanged"
    )
)

phase13_germany_position_summary_valid

############################################################
# Phase 13 - Step 9
# Save validated competitive-projection outputs
############################################################

readr::write_csv(
  phase13_competitive_projection,
  "data_clean/competitor_position_projection_2025_2029.csv"
)

readr::write_csv(
  phase13_germany_projection,
  "data_clean/germany_competitive_position_2025_2029.csv"
)

readr::write_csv(
  phase13_germany_position_summary,
  "data_clean/germany_position_summary_2025_2029.csv"
)

############################################################
# Check saved files
############################################################

phase13_output_files <- c(
  "data_clean/competitor_position_projection_2025_2029.csv",
  "data_clean/germany_competitive_position_2025_2029.csv",
  "data_clean/germany_position_summary_2025_2029.csv"
)

phase13_output_file_check <- tibble::tibble(
  file = basename(
    phase13_output_files
  ),
  
  exists = file.exists(
    phase13_output_files
  ),
  
  size_bytes = file.info(
    phase13_output_files
  )$size
)

phase13_output_file_check

############################################################
# Phase 13 - Step 10
# Read back and validate saved projection outputs
############################################################

phase13_competitive_projection_check <- readr::read_csv(
  "data_clean/competitor_position_projection_2025_2029.csv",
  col_types = readr::cols(
    hs2_code = readr::col_character(),
    .default = readr::col_guess()
  )
)

phase13_germany_projection_saved_check <- readr::read_csv(
  "data_clean/germany_competitive_position_2025_2029.csv",
  col_types = readr::cols(
    hs2_code = readr::col_character(),
    .default = readr::col_guess()
  )
)

phase13_germany_summary_saved_check <- readr::read_csv(
  "data_clean/germany_position_summary_2025_2029.csv",
  col_types = readr::cols(
    hs2_code = readr::col_character(),
    .default = readr::col_guess()
  )
)

phase13_saved_output_dimensions <- tibble::tibble(
  dataset = c(
    "Competitive projection",
    "Germany projection",
    "Germany position summary"
  ),
  
  rows = c(
    nrow(phase13_competitive_projection_check),
    nrow(phase13_germany_projection_saved_check),
    nrow(phase13_germany_summary_saved_check)
  ),
  
  columns = c(
    ncol(phase13_competitive_projection_check),
    ncol(phase13_germany_projection_saved_check),
    ncol(phase13_germany_summary_saved_check)
  )
)

phase13_saved_output_dimensions

############################################################
# Phase 13 - Step 11
# Final validation of saved Phase 13 outputs
############################################################

phase13_saved_outputs_valid <- all(
  phase13_saved_output_dimensions$rows ==
    c(120, 20, 4),
  
  phase13_saved_output_dimensions$columns ==
    c(13, 12, 13),
  
  phase13_output_file_check$exists,
  
  phase13_output_file_check$size_bytes > 0
)

phase13_saved_outputs_valid