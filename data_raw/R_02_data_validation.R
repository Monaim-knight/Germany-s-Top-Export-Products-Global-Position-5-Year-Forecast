# Checking datasets dimensions 
dim(trade_master)
dim(market_share)
dim(country_rankings)
# Validating countries list 
unique(trade_master$country)
names(market_share)
names(sector_lookup)
names(country_rankings)
names(trade_master)
length(unique(trade_master$country))
# Validating years 
range(trade_master$year)
# Validating product groups 
trade_master %>%
  distinct(
    hs2_code,
    product_group
  )
# Count 
length(unique(trade_master$hs2_code))
# Validation- Missing values 
colSums(
  is.na(trade_master)
)
# Validation - Duplicate Business Keys 
trade_master %>%
  count(
    country,
    year,
    hs2_code
  ) %>%
  filter(n > 1)
# Validation- Market Share Check 
market_share %>%
  group_by(
    year,
    hs2_code
  ) %>%
  summarise(
    total_share =
      sum(market_share)
  )
#######################################################
# Correct metric Terminoly
# Share among selected competitor countries 
#######################################################
comparison_share <- trade_master %>%
  group_by(
    year,
    hs2_code
  ) %>%
  mutate(
    comparison_total_exports_usd =
      sum(export_value_usd, na.rm = TRUE),
    
    comparison_export_share =
      export_value_usd /
      comparison_total_exports_usd
  ) %>%
  ungroup()
####################################################
# Competitor Ranking 
##################################################
competitor_rankings <- comparison_share %>%
  group_by(
    year,
    hs2_code
  ) %>%
  mutate(
    competitor_rank =
      min_rank(desc(export_value_usd))
  ) %>%
  ungroup()
# Validation - Rankings 
comparison_share %>%
  summarise(
    total_share = sum(comparison_export_share),
    .by = c(year, hs2_code)
  )
# Checking Ranking 
competitor_rankings %>%
  filter(year == 2024) %>%
  select(
    year,
    country,
    hs2_code,
    export_value_usd,
    comparison_export_share,
    competitor_rank
  ) %>%
  arrange(
    hs2_code,
    competitor_rank
  ) %>%
  print(n = 24)
# Validation- Ranking check 
country_rankings %>%
  filter(year == 2024) %>%
  arrange(
    hs2_code,
    rank
  )
####################################################
# Save validated competitor-comparison datasets
####################################################

write_csv(
  comparison_share,
  "data_clean/comparison_share.csv"
)

write_csv(
  competitor_rankings,
  "data_clean/competitor_rankings.csv"
)
# Verifying 
file.exists("data_clean/comparison_share.csv")
file.exists("data_clean/competitor_rankings.csv")
##############################################
# Removing the obsolete files 
#############################################
file.remove("data_clean/market_share.csv")
file.remove("data_clean/country_rankings.csv")
