############################################################
# Germany Export Forecast Project
#
# Phase 7: Historical Feature Engineering
#
# Purpose:
# Create historical analytical features for Germany's
# four key export sectors (HS30, HS84, HS85, HS87)
# using validated 2010-2024 trade data.
#
# Main features:
# - Year-over-Year export growth
# - Global market-share change
# - Global rank change
# - CAGR
# - Absolute export-value change
# - Germany vs competitor performance
#
# These features will support:
# - Historical trend analysis
# - Forecast model development
# - Tableau dashboards
############################################################
############################################
# Load required packages
############################################

library(dplyr)
library(readr)

############################################################
# Load reusable project validation functions
############################################################

source(
  "R/validation_functions.R"
)
file.exists(
  "R/validation_functions.R"
)

############################################################
# Step 1: Load validated Germany historical data
############################################################

germany_features <- readr::read_csv(
  "data_clean/germany_global_position_2010_2024.csv",
  show_col_types = FALSE
)
dim(germany_features)
names(germany_features)
range(
  germany_features$year,
  na.rm = TRUE
)
sort(
  unique(germany_features$hs2_code)
)
germany_feature_duplicates <- germany_features %>%
  count(
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)

nrow(germany_feature_duplicates)
#########################################
# Calcualtion Year-over-Year Export growth 
########################################
############################################################
# Step 2: Calculate Year-over-Year export growth
############################################################

germany_features <- germany_features %>%
  arrange(
    hs2_code,
    year
  ) %>%
  group_by(
    hs2_code
  ) %>%
  mutate(
    previous_year_export_value_usd =
      lag(export_value_usd),
    
    export_value_change_usd =
      export_value_usd -
      previous_year_export_value_usd,
    
    export_yoy_growth =
      if_else(
        !is.na(previous_year_export_value_usd) &
          previous_year_export_value_usd != 0,
        export_value_usd /
          previous_year_export_value_usd - 1,
        NA_real_
      ),
    
    export_yoy_growth_pct =
      export_yoy_growth * 100
  ) %>%
  ungroup()
 names(germany_features)
 dim(germany_features)
 # Confirming the first year has no YoY value 
 germany_features %>%
   filter(year == 2010) %>%
   select(
     year,
     hs2_code,
     export_value_usd,
     previous_year_export_value_usd,
     export_yoy_growth_pct
   ) %>%
   arrange(hs2_code)
 # Counting missin YoY values 
 sum(
   is.na(germany_features$export_yoy_growth_pct)
 )
 ###############################################
 # Calculatin global market-share change 
 ################################################
 ############################################################
 # Step 3: Calculate Year-over-Year market-share change
 ############################################################
 
 germany_features <- germany_features %>%
   arrange(
     hs2_code,
     year
   ) %>%
   group_by(
     hs2_code
   ) %>%
   mutate(
     previous_year_global_market_share =
       lag(global_market_share),
     
     global_market_share_change =
       global_market_share -
       previous_year_global_market_share,
     
     global_market_share_change_pct_points =
       global_market_share_change * 100
   ) %>%
   ungroup()
 # Checking the new columns 
 germany_features %>%
   select(
     year,
     hs2_code,
     global_market_share,
     previous_year_global_market_share,
     global_market_share_change,
     global_market_share_change_pct_points
   ) %>%
   head(12)
 # Validating 2010
 germany_features %>%
   filter(year == 2010) %>%
   select(
     year,
     hs2_code,
     global_market_share,
     previous_year_global_market_share,
     global_market_share_change_pct_points
   ) %>%
   arrange(hs2_code)
 # Count missing share change values 
 sum(
   is.na(
     germany_features$global_market_share_change_pct_points
   )
 )
 ##################################################
 # Global Rank Change
 ####################################################
 ############################################################
 # Step 4: Calculate Year-over-Year global rank change
 ############################################################
 
 germany_features <- germany_features %>%
   arrange(
     hs2_code,
     year
   ) %>%
   group_by(
     hs2_code
   ) %>%
   mutate(
     previous_year_global_rank =
       lag(global_rank),
     
     global_rank_change =
       global_rank -
       previous_year_global_rank
   ) %>%
   ungroup()
 # Inspecting new fields 
 germany_features %>%
   select(
     year,
     hs2_code,
     global_rank,
     previous_year_global_rank,
     global_rank_change
   ) %>%
   head(20)
 # Validation -2010
 germany_features %>%
   filter(year == 2010) %>%
   select(
     year,
     hs2_code,
     global_rank,
     previous_year_global_rank,
     global_rank_change
   ) %>%
   arrange(hs2_code)
 # Count missing Rank change 
 sum(
   is.na(
     germany_features$global_rank_change
   )
 )
 # Make the interpretation easier 
 germany_features <- germany_features %>%
   mutate(
     global_rank_direction = case_when(
       is.na(global_rank_change) ~ NA_character_,
       global_rank_change < 0 ~ "Improved",
       global_rank_change > 0 ~ "Declined",
       global_rank_change == 0 ~ "Unchanged"
     )
   )
 table(
   germany_features$global_rank_direction,
   useNA = "ifany"
 )
 ###############################################
 # Calculating CAGR for each German export sector 
 ##################################################
 ############################################################
 # Step 5: Calculate CAGR by product
 ############################################################
 
 germany_cagr <- germany_features %>%
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
     
     number_of_years =
       max(year) - min(year),
     
     .groups = "drop"
   ) %>%
   mutate(
     export_cagr =
       (
         end_export_value_usd /
           start_export_value_usd
       )^(1 / number_of_years) - 1,
     
     export_cagr_pct =
       export_cagr * 100
   )
 # Results 
 germany_cagr %>%
   select(
     hs2_code,
     product_group,
     start_year,
     end_year,
     start_export_value_usd,
     end_export_value_usd,
     number_of_years,
     export_cagr_pct
   ) %>%
   arrange(hs2_code)
 dim(germany_cagr)
 # Validation - the calculated period 
 germany_cagr %>%
   select(
     hs2_code,
     start_year,
     end_year,
     number_of_years
   )
 # Check for invalid CAGR values 
 germany_cagr %>%
   filter(
     is.na(export_cagr) |
       is.infinite(export_cagr)
   )
######################################
 # Fix HS product labels before CAGR
 #########################################
 # Standardise the four product labels 
 ###########################################
 ############################################################
 # Step 5A: Standardize HS2 product labels
 ############################################################
 
 germany_features <- germany_features %>%
   mutate(
     product_group = case_when(
       hs2_code == 30 ~
         "Pharmaceutical products",
       
       hs2_code == 84 ~
         "Machinery and mechanical appliances",
       
       hs2_code == 85 ~
         "Electrical machinery and equipment",
       
       hs2_code == 87 ~
         "Vehicles and parts",
       
       TRUE ~ product_group
     )
   )
 # Validating the standardized mapping 
 germany_features %>%
   distinct(
     hs2_code,
     product_group
   ) %>%
   arrange(hs2_code)
 # CHECK 
 germany_features %>%
   distinct(
     hs2_code,
     product_group
   ) %>%
   nrow()
 ##############################################
 ############################################################
 # Step 5B: Calculate export CAGR by product
 # Period: 2010-2024
 ############################################################
 
 germany_cagr <- germany_features %>%
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
     
     number_of_years =
       max(year) - min(year),
     
     .groups = "drop"
   ) %>%
   mutate(
     export_cagr =
       (
         end_export_value_usd /
           start_export_value_usd
       )^(1 / number_of_years) - 1,
     
     export_cagr_pct =
       export_cagr * 100
   )
 # Results 
 germany_cagr %>%
   select(
     hs2_code,
     product_group,
     start_year,
     end_year,
     start_export_value_usd,
     end_export_value_usd,
     number_of_years,
     export_cagr_pct
   ) %>%
   arrange(hs2_code)
  dim(germany_cagr)
  # Validation- the CAGR period 
  germany_cagr %>%
    select(
      hs2_code,
      start_year,
      end_year,
      number_of_years
    )
 # Check for invalid CAGR values 
  invalid_cagr <- germany_cagr %>%
    filter(
      is.na(export_cagr) |
        is.infinite(export_cagr)
    )
  
  nrow(invalid_cagr)
#################################################
#############################################################
  # Step 5C: Review final CAGR results
  ############################################################
  
  germany_cagr_summary <- germany_cagr %>%
    select(
      hs2_code,
      product_group,
      start_year,
      end_year,
      start_export_value_usd,
      end_export_value_usd,
      export_cagr_pct
    ) %>%
    arrange(
      desc(export_cagr_pct)
    )
# Complete results 
  print(
    germany_cagr_summary,
    width = Inf
  )
dim(germany_cagr_summary)
# Calculation Period 
germany_cagr_summary %>%
  select(
    hs2_code,
    start_year,
    end_year
  ) %>%
  arrange(hs2_code)
#Checking for missing or invalid CAGR values 
invalid_cagr <- germany_cagr_summary %>%
  filter(
    is.na(export_cagr_pct) |
      is.infinite(export_cagr_pct)
  )

nrow(invalid_cagr)
# HS2 products existance 
sort(
  germany_cagr_summary$hs2_code
)
# Save the validated CAGR result 
############################################################
# Step 6: Save validated CAGR summary
############################################################

readr::write_csv(
  germany_cagr_summary,
  "data_clean/germany_cagr_2010_2024.csv"
)
file.exists(
  "data_clean/germany_cagr_2010_2024.csv"
)
germany_cagr_check <- readr::read_csv(
  "data_clean/germany_cagr_2010_2024.csv",
  show_col_types = FALSE
)
dim(germany_cagr_check)
sort(
  germany_cagr_check$hs2_code
)
############################################################
# Step 7: Save Germany historical feature dataset
############################################################

readr::write_csv(
  germany_features,
  "data_clean/germany_historical_features_2010_2024.csv"
)
file.exists(
  "data_clean/germany_historical_features_2010_2024.csv"
)
germany_features_check <- readr::read_csv(
  "data_clean/germany_historical_features_2010_2024.csv",
  show_col_types = FALSE
)
nrow(germany_features_check)
sort(
  unique(germany_features_check$hs2_code)
)
germany_features_check %>%
  count(
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)

############################################################
# Step 8: Load validated competitor dataset
############################################################

competitor_features <- readr::read_csv(
  "data_clean/competitor_rankings.csv",
  show_col_types = FALSE
)
dim(competitor_features)
sort(
  unique(competitor_features$country)
)
range(
  competitor_features$year,
  na.rm = TRUE
)
sort(
  unique(competitor_features$hs2_code)
)
# Check uniqueness 
competitor_feature_duplicates <- competitor_features %>%
  count(
    country,
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)

nrow(competitor_feature_duplicates)
# Missing values 
competitor_missing_check <- competitor_features %>%
  summarise(
    missing_country =
      sum(is.na(country)),
    
    missing_year =
      sum(is.na(year)),
    
    missing_hs2 =
      sum(is.na(hs2_code)),
    
    missing_export_value =
      sum(is.na(export_value_usd)),
    
    missing_comparison_share =
      sum(is.na(comparison_export_share)),
    
    missing_competitor_rank =
      sum(is.na(competitor_rank))
  )
print(
  competitor_missing_check,
  width = Inf
)
########################################
############################################################
# Step 9: Standardize competitor product labels
############################################################

competitor_features <- competitor_features %>%
  mutate(
    product_group = case_when(
      hs2_code == 30 ~
        "Pharmaceutical products",
      
      hs2_code == 84 ~
        "Machinery and mechanical appliances",
      
      hs2_code == 85 ~
        "Electrical machinery and equipment",
      
      hs2_code == 87 ~
        "Vehicles and parts",
      
      TRUE ~ product_group
    )
  )
competitor_features %>%
  distinct(
    hs2_code,
    product_group
  ) %>%
  arrange(hs2_code)
# Four products mapping 
competitor_features %>%
  distinct(
    hs2_code,
    product_group
  ) %>%
  nrow()

############################################################
# Step 10: Calculate competitor Year-over-Year export growth
############################################################

competitor_features <- competitor_features %>%
  arrange(
    country,
    hs2_code,
    year
  ) %>%
  group_by(
    country,
    hs2_code
  ) %>%
  mutate(
    previous_year_export_value_usd =
      lag(export_value_usd),
    
    export_value_change_usd =
      export_value_usd -
      previous_year_export_value_usd,
    
    export_yoy_growth =
      if_else(
        !is.na(previous_year_export_value_usd) &
          previous_year_export_value_usd != 0,
        
        export_value_usd /
          previous_year_export_value_usd - 1,
        
        NA_real_
      ),
    
    export_yoy_growth_pct =
      export_yoy_growth * 100
  ) %>%
  ungroup()
#############################################
# Validation 
########################################
dim(competitor_features)
# Validating the first year 

competitor_features %>%
  filter(year == 2010) %>%
  select(
    country,
    year,
    hs2_code,
    export_value_usd,
    previous_year_export_value_usd,
    export_yoy_growth_pct
  ) %>%
  arrange(
    country,
    hs2_code
  )
# Count missing YoY growth 
sum(
  is.na(
    competitor_features$export_yoy_growth_pct
  )
)
# Checking for unexpected missing values 
unexpected_missing_competitor_growth <-
  competitor_features %>%
  filter(
    year > 2010,
    is.na(export_yoy_growth_pct)
  )

nrow(unexpected_missing_competitor_growth)

############################################################
# Step 11: Calculate competitor comparison-share change
############################################################

competitor_features <- competitor_features %>%
  arrange(
    country,
    hs2_code,
    year
  ) %>%
  group_by(
    country,
    hs2_code
  ) %>%
  mutate(
    previous_year_comparison_share =
      lag(comparison_export_share),
    
    comparison_share_change =
      comparison_export_share -
      previous_year_comparison_share,
    
    comparison_share_change_pct_points =
      comparison_share_change * 100
  ) %>%
  ungroup()

##############################################
# Validation 
##############################################
dim(competitor_features)
# Vaidating first year 
competitor_features %>%
  filter(year == 2010) %>%
  select(
    country,
    year,
    hs2_code,
    comparison_export_share,
    previous_year_comparison_share,
    comparison_share_change_pct_points
  ) %>%
  arrange(
    country,
    hs2_code
  )
# Counting missing share change values 
sum(
  is.na(
    competitor_features$comparison_share_change_pct_points
  )
)
# Check for unexpected missing values after 2010
unexpected_missing_comparison_share <-
  competitor_features %>%
  filter(
    year > 2010,
    is.na(comparison_share_change_pct_points)
  )

nrow(unexpected_missing_comparison_share)

############################################################
# Step 12: Calculate competitor rank change
############################################################

competitor_features <- competitor_features %>%
  arrange(
    country,
    hs2_code,
    year
  ) %>%
  group_by(
    country,
    hs2_code
  ) %>%
  mutate(
    previous_year_competitor_rank =
      lag(competitor_rank),
    
    competitor_rank_change =
      competitor_rank -
      previous_year_competitor_rank
  ) %>%
  ungroup()
################################################
# Validation 
################################################
dim(competitor_features)
# Validating the first years 
competitor_features %>%
  filter(year == 2010) %>%
  select(
    country,
    year,
    hs2_code,
    competitor_rank,
    previous_year_competitor_rank,
    competitor_rank_change
  ) %>%
  arrange(
    country,
    hs2_code
  )
# Count missing rank changes values 
sum(
  is.na(
    competitor_features$competitor_rank_change
  )
)
# Check unexpected missing values after 2010
unexpected_missing_competitor_rank <-
  competitor_features %>%
  filter(
    year > 2010,
    is.na(competitor_rank_change)
  )

nrow(unexpected_missing_competitor_rank)
######################################
# Add rank-direction label 
#######################################
competitor_features <- competitor_features %>%
  mutate(
    competitor_rank_direction =
      case_when(
        is.na(competitor_rank_change) ~ NA_character_,
        competitor_rank_change < 0 ~ "Improved",
        competitor_rank_change > 0 ~ "Declined",
        competitor_rank_change == 0 ~ "Unchanged"
      )
  )
table(
  competitor_features$competitor_rank_direction,
  useNA = "ifany"
)
############################################################
# Step 13: Calculate competitor CAGR
# Period: 2010-2024
############################################################

competitor_cagr <- competitor_features %>%
  group_by(
    country,
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
    
    number_of_years =
      max(year) - min(year),
    
    .groups = "drop"
  ) %>%
  mutate(
    export_cagr =
      (
        end_export_value_usd /
          start_export_value_usd
      )^(1 / number_of_years) - 1,
    
    export_cagr_pct =
      export_cagr * 100
  ) %>%
  arrange(
    hs2_code,
    desc(export_cagr_pct)
  )
############################################
# Validation 
###########################################
dim(competitor_cagr)
# Validate the period 
competitor_cagr %>%
  count(
    start_year,
    end_year,
    number_of_years
  )
# Check for invalid CAGR values 
invalid_competitor_cagr <- competitor_cagr %>%
  filter(
    is.na(export_cagr) |
      is.infinite(export_cagr)
  )

nrow(invalid_competitor_cagr)
# Confirm all 24 country product combinations 
competitor_cagr %>%
  summarise(
    countries =
      n_distinct(country),
    
    products =
      n_distinct(hs2_code),
    
    combinations =
      n()
  )

############################################################
# Step 14: Save validated competitor CAGR dataset
############################################################

readr::write_csv(
  competitor_cagr,
  "data_clean/competitor_cagr_2010_2024.csv"
)

file.exists(
  "data_clean/competitor_cagr_2010_2024.csv"
)
competitor_cagr_check <- readr::read_csv(
  "data_clean/competitor_cagr_2010_2024.csv",
  show_col_types = FALSE
)

dim(competitor_cagr_check)
# Confirm coverage 
competitor_cagr_check %>%
  summarise(
    countries =
      n_distinct(country),
    
    products =
      n_distinct(hs2_code),
    
    combinations =
      n()
  )
# Confirm no duplicate country product combinations 
competitor_cagr_check %>%
  count(
    country,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)

############################################################
# Step 15: Save competitor historical feature dataset
############################################################

readr::write_csv(
  competitor_features,
  "data_clean/competitor_historical_features_2010_2024.csv"
)
file.exists(
  "data_clean/competitor_historical_features_2010_2024.csv"
)
competitor_features_check <- readr::read_csv(
  "data_clean/competitor_historical_features_2010_2024.csv",
  show_col_types = FALSE
)
nrow(competitor_features_check)

competitor_features_check %>%
  summarise(
    countries =
      n_distinct(country),
    
    products =
      n_distinct(hs2_code),
    
    years =
      n_distinct(year)
  )
competitor_features_check %>%
  count(
    country,
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)


################################################
############################################################
# Step 16.1: Create competitor CAGR benchmark
# Germany is excluded from competitor benchmark calculations
############################################################

competitor_cagr_benchmark <- competitor_cagr %>%
  filter(
    country != "Germany"
  ) %>%
  group_by(
    hs2_code,
    product_group
  ) %>%
  summarise(
    competitor_average_cagr_pct =
      mean(export_cagr_pct, na.rm = TRUE),
    
    competitor_median_cagr_pct =
      median(export_cagr_pct, na.rm = TRUE),
    
    fastest_competitor_cagr_pct =
      max(export_cagr_pct, na.rm = TRUE),
    
    fastest_competitor =
      country[which.max(export_cagr_pct)],
    
    slowest_competitor_cagr_pct =
      min(export_cagr_pct, na.rm = TRUE),
    
    competitor_count =
      n_distinct(country),
    
    .groups = "drop"
  )
############################################################
# Step 16.2: Extract Germany CAGR
############################################################

germany_cagr_comparison <- competitor_cagr %>%
  filter(
    country == "Germany"
  ) %>%
  select(
    hs2_code,
    product_group,
    germany_start_export_value_usd =
      start_export_value_usd,
    germany_end_export_value_usd =
      end_export_value_usd,
    germany_cagr_pct =
      export_cagr_pct
  )
############################################################
# Step 16.3: Build Germany-versus-competitor comparison
############################################################

germany_vs_competitors_cagr <-
  germany_cagr_comparison %>%
  left_join(
    competitor_cagr_benchmark,
    by = c(
      "hs2_code",
      "product_group"
    )
  ) %>%
  mutate(
    cagr_gap_vs_competitor_average_pct_points =
      germany_cagr_pct -
      competitor_average_cagr_pct,
    
    cagr_gap_vs_fastest_competitor_pct_points =
      germany_cagr_pct -
      fastest_competitor_cagr_pct,
    
    germany_growth_position = case_when(
      germany_cagr_pct >
        competitor_average_cagr_pct ~
        "Above competitor average",
      
      germany_cagr_pct <
        competitor_average_cagr_pct ~
        "Below competitor average",
      
      TRUE ~
        "Equal to competitor average"
    )
  ) %>%
  arrange(
    hs2_code
  )

# Results 
print(
  germany_vs_competitors_cagr,
  width = Inf
)
###########################
# Validation 
###############################
dim(
  germany_vs_competitors_cagr
)
germany_vs_competitors_cagr %>%
  select(
    hs2_code,
    competitor_count
  )
colSums(
  is.na(germany_vs_competitors_cagr)
)
sort(
  germany_vs_competitors_cagr$hs2_code
)
############################################################
# Step 17: Save Germany-versus-competitor CAGR comparison
############################################################

readr::write_csv(
  germany_vs_competitors_cagr,
  "data_clean/germany_vs_competitors_cagr_2010_2024.csv"
)
file.exists(
  "data_clean/germany_vs_competitors_cagr_2010_2024.csv"
)
germany_vs_competitors_cagr_check <- readr::read_csv(
  "data_clean/germany_vs_competitors_cagr_2010_2024.csv",
  show_col_types = FALSE
)
dim(
  germany_vs_competitors_cagr_check
)
sort(
  germany_vs_competitors_cagr_check$hs2_code
)
sum(
  is.na(
    germany_vs_competitors_cagr_check
  )
)

############################################################
# Step 18: Create annual competitor benchmark
# Germany is excluded from benchmark calculations
############################################################

annual_competitor_benchmark <- competitor_features %>%
  filter(
    country != "Germany"
  ) %>%
  group_by(
    year,
    hs2_code,
    product_group
  ) %>%
  summarise(
    competitor_total_export_value_usd =
      sum(export_value_usd, na.rm = TRUE),
    
    competitor_average_export_value_usd =
      mean(export_value_usd, na.rm = TRUE),
    
    competitor_median_export_value_usd =
      median(export_value_usd, na.rm = TRUE),
    
    competitor_average_yoy_growth_pct =
      mean(export_yoy_growth_pct, na.rm = TRUE),
    
    competitor_average_comparison_share =
      mean(comparison_export_share, na.rm = TRUE),
    
    leading_competitor =
      country[which.max(export_value_usd)],
    
    leading_competitor_export_value_usd =
      max(export_value_usd, na.rm = TRUE),
    
    competitor_count =
      n_distinct(country),
    
    .groups = "drop"
  ) %>%
  arrange(
    hs2_code,
    year
  )
########################################
# Standardize initial period values 
#####################################
annual_competitor_benchmark <- annual_competitor_benchmark %>%
  mutate(
    competitor_average_yoy_growth_pct =
      if_else(
        is.nan(competitor_average_yoy_growth_pct),
        NA_real_,
        competitor_average_yoy_growth_pct
      )
  )
dim(
  annual_competitor_benchmark
)
annual_competitor_benchmark %>%
  count(
    competitor_count,
    name = "number_of_groups"
  )
# Validate years and product coverage 
annual_competitor_benchmark %>%
  summarise(
    years =
      n_distinct(year),
    
    products =
      n_distinct(hs2_code),
    
    groups =
      n()
  )
# Check uniqueness 
annual_competitor_benchmark %>%
  count(
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)
# Validate missing YoY benchmark values 
annual_competitor_benchmark %>%
  filter(
    is.na(competitor_average_yoy_growth_pct)
  ) %>%
  select(
    year,
    hs2_code,
    product_group,
    competitor_average_yoy_growth_pct
  )
sum(
  is.na(
    annual_competitor_benchmark$
      competitor_average_yoy_growth_pct
  )
)
############################################################
# Step 19: Save validated annual competitor benchmark
############################################################

readr::write_csv(
  annual_competitor_benchmark,
  "data_clean/annual_competitor_benchmark_2010_2024.csv"
)
file.exists(
  "data_clean/annual_competitor_benchmark_2010_2024.csv"
)
annual_competitor_benchmark_check <- readr::read_csv(
  "data_clean/annual_competitor_benchmark_2010_2024.csv",
  show_col_types = FALSE
)
dim(
  annual_competitor_benchmark_check
)
annual_competitor_benchmark_check %>%
  count(
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)
############################################################
# Step 20: Join Germany with annual competitor benchmark
############################################################

germany_competitor_comparison <- germany_features %>%
  left_join(
    annual_competitor_benchmark,
    by = c(
      "year",
      "hs2_code",
      "product_group"
    )
  ) %>%
  arrange(
    hs2_code,
    year
  )
dim(germany_competitor_comparison)
# Comparing one row per year and product 
comparison_duplicates <- germany_competitor_comparison %>%
  count(
    year,
    hs2_code,
    name = "record_count"
  ) %>%
  filter(record_count != 1)

nrow(comparison_duplicates)

# Whether the join matched all 60 rows 
comparison_unmatched_rows <- germany_competitor_comparison %>%
  filter(
    is.na(competitor_count)
  )

nrow(comparison_unmatched_rows)
# Confirm five competitors per row 
germany_competitor_comparison %>%
  count(
    competitor_count,
    name = "number_of_rows"
  )

############################################################
# Step 21: Calculate annual performance gaps
############################################################

germany_competitor_comparison <-
  germany_competitor_comparison %>%
  mutate(
    export_value_gap_vs_competitor_average_usd =
      export_value_usd -
      competitor_average_export_value_usd,
    
    export_value_ratio_vs_competitor_average =
      if_else(
        competitor_average_export_value_usd != 0,
        export_value_usd /
          competitor_average_export_value_usd,
        NA_real_
      ),
    
    export_value_gap_vs_leader_usd =
      export_value_usd -
      leading_competitor_export_value_usd,
    
    yoy_growth_gap_vs_competitor_average_pct_points =
      export_yoy_growth_pct -
      competitor_average_yoy_growth_pct,
    
    comparison_share_gap_vs_competitor_average_pct_points =
      (
        comparison_export_share -
          competitor_average_comparison_share
      ) * 100
  )
# Failed#####
####################################################
#Create the required Germany Table 
####################################################
############################################################
# Step 21A: Extract Germany comparison-share history
############################################################

germany_comparison_share <- competitor_features %>%
  filter(
    country == "Germany"
  ) %>%
  select(
    year,
    hs2_code,
    germany_comparison_export_share =
      comparison_export_share
  )
###################################
# Validation 
###############################
germany_comparison_share_validation <- validate_dataset(
  data = germany_comparison_share,
  dataset_name = "Germany Comparison Share",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "hs2_code",
    "germany_comparison_export_share"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4
)
print(
  germany_comparison_share_validation,
  width = Inf
)
############################################################
# Step 21B: Add Germany comparison share
############################################################

germany_competitor_comparison <-
  germany_competitor_comparison %>%
  left_join(
    germany_comparison_share,
    by = c(
      "year",
      "hs2_code"
    )
  )

germany_competitor_comparison_validation <-
  validate_dataset(
    data = germany_competitor_comparison,
    dataset_name = "Germany Competitor Comparison",
    
    key_columns = c(
      "year",
      "hs2_code"
    ),
    
    critical_columns = c(
      "year",
      "hs2_code",
      "product_group",
      "export_value_usd",
      "germany_comparison_export_share",
      "competitor_average_export_value_usd",
      "competitor_average_comparison_share",
      "leading_competitor",
      "leading_competitor_export_value_usd"
    ),
    
    expected_rows = 60,
    expected_years = 15,
    expected_products = 4
  )
print(
  germany_competitor_comparison_validation,
  width = Inf
)
############################################################
# Step 21B.1: Inspect comparison-share columns after join
############################################################

names(germany_competitor_comparison)[
  grepl(
    "comparison",
    names(germany_competitor_comparison),
    ignore.case = TRUE
  )
]
############################################################
# Step 21B.2: Compare duplicated comparison-share columns
############################################################

all.equal(
  germany_competitor_comparison$
    germany_comparison_export_share.x,
  
  germany_competitor_comparison$
    germany_comparison_export_share.y
)
############################################################
# Check missing values in duplicated columns
############################################################

germany_competitor_comparison %>%
  summarise(
    missing_x =
      sum(
        is.na(
          germany_comparison_export_share.x
        )
      ),
    
    missing_y =
      sum(
        is.na(
          germany_comparison_export_share.y
        )
      )
  )

############################################################
# Step 21B.3: Consolidate duplicated comparison-share columns
############################################################

germany_competitor_comparison <-
  germany_competitor_comparison %>%
  rename(
    germany_comparison_export_share =
      germany_comparison_export_share.x
  ) %>%
  select(
    -germany_comparison_export_share.y
  )
############################################################
# Validate corrected comparison dataset
############################################################

comparison_column_fix_validation <- validate_dataset(
  data = germany_competitor_comparison,
  dataset_name = "Germany Competitor Comparison - Column Fix",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "hs2_code",
    "product_group",
    "export_value_usd",
    "germany_comparison_export_share",
    "competitor_average_export_value_usd",
    "competitor_average_comparison_share"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)
print(
  comparison_column_fix_validation,
  width = Inf
)
names(germany_competitor_comparison)[
  grepl(
    "germany_comparison_export_share",
    names(germany_competitor_comparison),
    ignore.case = TRUE
  )
]

############################################################
# Step 21C: Calculate annual performance gaps
############################################################

germany_competitor_comparison <-
  germany_competitor_comparison %>%
  mutate(
    export_value_gap_vs_competitor_average_usd =
      export_value_usd -
      competitor_average_export_value_usd,
    
    export_value_ratio_vs_competitor_average =
      if_else(
        competitor_average_export_value_usd != 0,
        export_value_usd /
          competitor_average_export_value_usd,
        NA_real_
      ),
    
    export_value_gap_vs_leader_usd =
      export_value_usd -
      leading_competitor_export_value_usd,
    
    yoy_growth_gap_vs_competitor_average_pct_points =
      export_yoy_growth_pct -
      competitor_average_yoy_growth_pct,
    
    comparison_share_gap_vs_competitor_average_pct_points =
      (
        germany_comparison_export_share -
          competitor_average_comparison_share
      ) * 100
  )
############################################################
# Validate annual performance-gap dataset
############################################################

performance_gap_validation <- validate_dataset(
  data = germany_competitor_comparison,
  dataset_name = "Germany Annual Performance Gaps",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "hs2_code",
    "product_group",
    "export_value_usd",
    "competitor_average_export_value_usd",
    "germany_comparison_export_share",
    "competitor_average_comparison_share",
    "leading_competitor",
    "leading_competitor_export_value_usd",
    "export_value_gap_vs_competitor_average_usd",
    "export_value_ratio_vs_competitor_average",
    "export_value_gap_vs_leader_usd",
    "comparison_share_gap_vs_competitor_average_pct_points"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)

print(
  performance_gap_validation,
  width = Inf
)
yoy_gap_missing_check <- sum(
  is.na(
    germany_competitor_comparison$
      yoy_growth_gap_vs_competitor_average_pct_points
  )
)

yoy_gap_missing_check
unexpected_yoy_gap_missing <-
  germany_competitor_comparison %>%
  filter(
    year > 2010,
    is.na(
      yoy_growth_gap_vs_competitor_average_pct_points
    )
  )

nrow(
  unexpected_yoy_gap_missing
)
# Check invalid ratios
invalid_performance_ratios <-
  germany_competitor_comparison %>%
  filter(
    is.na(
      export_value_ratio_vs_competitor_average
    ) |
      is.infinite(
        export_value_ratio_vs_competitor_average
      ) |
      export_value_ratio_vs_competitor_average < 0
  )

nrow(
  invalid_performance_ratios
)
############################################################
# Step 22: Add annual performance labels
############################################################

germany_competitor_comparison <-
  germany_competitor_comparison %>%
  mutate(
    export_value_position =
      case_when(
        export_value_ratio_vs_competitor_average > 1 ~
          "Above competitor average",
        
        export_value_ratio_vs_competitor_average < 1 ~
          "Below competitor average",
        
        TRUE ~
          "Equal to competitor average"
      ),
    
    yoy_growth_position =
      case_when(
        is.na(
          yoy_growth_gap_vs_competitor_average_pct_points
        ) ~
          NA_character_,
        
        yoy_growth_gap_vs_competitor_average_pct_points > 0 ~
          "Above competitor average",
        
        yoy_growth_gap_vs_competitor_average_pct_points < 0 ~
          "Below competitor average",
        
        TRUE ~
          "Equal to competitor average"
      ),
    
    comparison_share_position =
      case_when(
        comparison_share_gap_vs_competitor_average_pct_points > 0 ~
          "Above competitor average",
        
        comparison_share_gap_vs_competitor_average_pct_points < 0 ~
          "Below competitor average",
        
        TRUE ~
          "Equal to competitor average"
      ),
    
    competitor_leadership_status =
      case_when(
        export_value_gap_vs_leader_usd == 0 ~
          "Germany leads",
        
        export_value_gap_vs_leader_usd < 0 ~
          "Competitor leads",
        
        TRUE ~
          "Review required"
      )
  )

############################################################
# Validate annual performance labels
############################################################

performance_label_validation <- validate_dataset(
  data = germany_competitor_comparison,
  dataset_name = "Germany Annual Performance Labels",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "hs2_code",
    "product_group",
    "export_value_position",
    "comparison_share_position",
    "competitor_leadership_status"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)
# Results 
print(
  performance_label_validation,
  width = Inf
)
sum(
  is.na(
    germany_competitor_comparison$
      yoy_growth_position
  )
)
unexpected_missing_growth_labels <-
  germany_competitor_comparison %>%
  filter(
    year > 2010,
    is.na(yoy_growth_position)
  )

nrow(
  unexpected_missing_growth_labels
)
# Inspect lable distributions 
table(
  germany_competitor_comparison$
    export_value_position,
  useNA = "ifany"
)
table(
  germany_competitor_comparison$
    yoy_growth_position,
  useNA = "ifany"
)
table(
  germany_competitor_comparison$
    comparison_share_position,
  useNA = "ifany"
)
table(
  germany_competitor_comparison$
    competitor_leadership_status,
  useNA = "ifany"
)
############################################################
# Step 22.4: Inspect Review required records
############################################################

review_required_records <- germany_competitor_comparison %>%
  filter(
    competitor_leadership_status == "Review required"
  ) %>%
  select(
    year,
    hs2_code,
    product_group,
    export_value_usd,
    leading_competitor,
    leading_competitor_export_value_usd,
    export_value_gap_vs_leader_usd,
    competitor_leadership_status
  ) %>%
  arrange(
    hs2_code,
    year
  )
nrow(review_required_records)
print(
  review_required_records,
  n = Inf,
  width = Inf
)



# Problems- Logical 
###############################
############################################################
# Step 22.5: Correct competitor leadership-status logic
############################################################

germany_competitor_comparison <-
  germany_competitor_comparison %>%
  mutate(
    competitor_leadership_status =
      case_when(
        export_value_gap_vs_leader_usd > 0 ~
          "Germany leads",
        
        export_value_gap_vs_leader_usd < 0 ~
          "Competitor leads",
        
        export_value_gap_vs_leader_usd == 0 ~
          "Tied with leading competitor",
        
        TRUE ~
          NA_character_
      )
  )
table(
  germany_competitor_comparison$
    competitor_leadership_status,
  useNA = "ifany"
)
sum(
  germany_competitor_comparison$
    competitor_leadership_status ==
    "Review required",
  na.rm = TRUE
)
sum(
  is.na(
    germany_competitor_comparison$
      competitor_leadership_status
  )
)

############################################################
# Step 22.9: Validate corrected performance labels
############################################################

corrected_performance_label_validation <- validate_dataset(
  data = germany_competitor_comparison,
  dataset_name = "Corrected Germany Performance Labels",
  
  key_columns = c(
    "year",
    "hs2_code"
  ),
  
  critical_columns = c(
    "year",
    "hs2_code",
    "product_group",
    "export_value_position",
    "comparison_share_position",
    "competitor_leadership_status"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)

print(
  corrected_performance_label_validation,
  width = Inf
)

############################################################
# Step 23: Save validated annual comparison dataset
############################################################

readr::write_csv(
  germany_competitor_comparison,
  "data_clean/germany_competitor_comparison_2010_2024.csv"
)
file.exists(
  "data_clean/germany_competitor_comparison_2010_2024.csv"
)

germany_competitor_comparison_check <- readr::read_csv(
  "data_clean/germany_competitor_comparison_2010_2024.csv",
  show_col_types = FALSE
)
###################
# Validation 
#####################
saved_comparison_validation <- validate_dataset(
  data = germany_competitor_comparison_check,
  dataset_name = "Saved Germany Competitor Comparison",
  
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
    "germany_comparison_export_share",
    "competitor_average_export_value_usd",
    "leading_competitor",
    "export_value_gap_vs_competitor_average_usd",
    "export_value_ratio_vs_competitor_average",
    "comparison_share_gap_vs_competitor_average_pct_points",
    "export_value_position",
    "comparison_share_position",
    "competitor_leadership_status"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)
print(
  saved_comparison_validation,
  width = Inf
)
############################################################
# Step 24: Save corrected annual comparison dataset
############################################################

readr::write_csv(
  germany_competitor_comparison,
  "data_clean/germany_competitor_comparison_2010_2024.csv"
)

corrected_comparison_check <- readr::read_csv(
  "data_clean/germany_competitor_comparison_2010_2024.csv",
  show_col_types = FALSE
)
############################################
# Validation 
#######################################
corrected_comparison_validation <- validate_dataset(
  data = corrected_comparison_check,
  dataset_name = "Corrected Saved Germany Competitor Comparison",
  
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
    "germany_comparison_export_share",
    "competitor_average_export_value_usd",
    "leading_competitor",
    "export_value_gap_vs_competitor_average_usd",
    "export_value_ratio_vs_competitor_average",
    "comparison_share_gap_vs_competitor_average_pct_points",
    "export_value_position",
    "comparison_share_position",
    "competitor_leadership_status"
  ),
  
  expected_rows = 60,
  expected_years = 15,
  expected_products = 4,
  expected_countries = 1
)

print(
  corrected_comparison_validation,
  width = Inf
)

table(
  corrected_comparison_check$competitor_leadership_status,
  useNA = "ifany"
)

sum(
  corrected_comparison_check$competitor_leadership_status ==
    "Review required",
  na.rm = TRUE
)

############################################
# Identify saved competitor-comparison file
############################################

list.files(
  "data_clean",
  pattern = "competitor|comparison",
  full.names = FALSE
)

############################################################
# Step 25: Validate Phase 7 output files
############################################################

phase7_files <- c(
  "data_clean/germany_historical_features_2010_2024.csv",
  "data_clean/germany_cagr_2010_2024.csv",
  "data_clean/competitor_historical_features_2010_2024.csv",
  "data_clean/competitor_cagr_2010_2024.csv",
  "data_clean/germany_vs_competitors_cagr_2010_2024.csv",
  "data_clean/annual_competitor_benchmark_2010_2024.csv",
  "data_clean/germany_competitor_comparison_2010_2024.csv"
)

phase7_file_check <- data.frame(
  file = basename(phase7_files),
  exists = file.exists(phase7_files),
  size_bytes = ifelse(
    file.exists(phase7_files),
    file.info(phase7_files)$size,
    NA
  )
)

phase7_file_check
#########################################
# Automated PASS/FAIL

phase7_files_valid <- all(
  phase7_file_check$exists &
    phase7_file_check$size_bytes > 0
)

phase7_files_valid
# Show only problems 
phase7_file_check %>%
  filter(
    !exists |
      is.na(size_bytes) |
      size_bytes <= 0
  )