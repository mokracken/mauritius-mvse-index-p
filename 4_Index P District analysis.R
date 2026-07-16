# =============================================================================
# Script 4 — District aggregation of locality-month MVSE Index P

# Title: Divergent climate suitability profiles for dengue and chikungunya 
# transmission by Aedes albopictus in Mauritius

# Purpose:
# Link locality-month MVSE Index P estimates to districts using a
# locality-to-district lookup file and generate district-level summaries
# and time-series outputs for dengue and chikungunya.

# Input:
#   Locality-month MVSE Index P files generated in Script 2:
#   MVSE_indexP_denv_monthly_FINAL.csv
#   MVSE_indexP_chik_monthly_FINAL.csv

#   Locality-to-district lookup CSV containing:
#   site_id and district

# Outputs:
#   IndexP_district_summary_long.csv
#   IndexP_district_summary_FINAL.csv
#   IndexP_district_summary_WITH_POSTERIOR_BOUNDS_FINAL.csv
#   IndexP_national_pooled_summary_WITH_POSTERIOR_BOUNDS_FINAL.csv


# Author: Amar Teeluck, Benn Sartorius, Oyelola Adegboye, Stephan Karl, Emma McBryde, 
# *Eloise Skinner

# Institution: University of Queensland
# Date: 06/03/2026
# R version: 4.4.1

# Note: The median lower and upper posterior bounds are retained as descriptive
# summaries only. They are not formal district- or national-level credible
# intervals.
#==============================================================================
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

#==============================================================================
# 1) Load locality-month Index P data
#==============================================================================
monthly_denv <- read_csv(file.choose(), show_col_types = FALSE) %>%
  mutate(
    month = floor_date(as.Date(date), "month"),
    virus = "DENV",
    site_id = str_trim(as.character(site_id))
  )

monthly_chik <- read_csv(file.choose(), show_col_types = FALSE) %>%
  mutate(
    month = floor_date(as.Date(date), "month"),
    virus = "CHIK",
    site_id = str_trim(as.character(site_id))
  )

monthly <- bind_rows(monthly_denv, monthly_chik) %>%
  mutate(site_id = str_trim(as.character(site_id)))

# Check required Index P columns

required_cols <- c(
  "month",
  "virus",
  "site_id",
  "indexP_med",
  "indexP_p025",
  "indexP_p975"
)

missing_cols <- setdiff(required_cols, names(monthly))

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns in Index P data: ",
    paste(missing_cols, collapse = ", ")
  )
}

#==============================================================================
# 2) Load and join district information
#==============================================================================
district_lookup <- read_csv(file.choose(), show_col_types = FALSE) %>%
  mutate(
    site_id = str_trim(as.character(site_id)),
    district = str_trim(as.character(district))
  )

# Check the district lookup

required_lookup_cols <- c("site_id", "district")
missing_lookup_cols <- setdiff(required_lookup_cols, names(district_lookup))

if (length(missing_lookup_cols) > 0) {
  stop(
    "Missing required columns in district lookup: ",
    paste(missing_lookup_cols, collapse = ", ")
  )
}

dup_lookup <- district_lookup %>%
  count(site_id) %>%
  filter(n > 1)

if (nrow(dup_lookup) > 0) {
  cat("\nDuplicate site_id values found in district lookup:\n")
  print(dup_lookup)
  stop("Duplicate site_id values found in district lookup. Fix before continuing.")
}

# Join localities to districts

monthly_district <- monthly %>%
  left_join(district_lookup, by = "site_id")

missing_districts <- monthly_district %>%
  filter(is.na(district)) %>%
  distinct(site_id) %>%
  arrange(site_id)


if (nrow(missing_districts) > 0) {
  print(missing_districts)
  stop("Some site_id values did not match to district lookup.")
}

#==============================================================================
# 3) Create district summary tables
#==============================================================================

district_summary <- monthly_district %>%
  group_by(virus, district) %>%
  summarise(
    Median = round(median(indexP_med, na.rm = TRUE), 2),
    Mean   = round(mean(indexP_med, na.rm = TRUE), 2),
    SD     = round(sd(indexP_med, na.rm = TRUE), 2),
    Min    = round(min(indexP_med, na.rm = TRUE), 2),
    Max    = round(max(indexP_med, na.rm = TRUE), 2),
    IQR    = round(IQR(indexP_med, na.rm = TRUE), 2),
    P95    = round(quantile(indexP_med, 0.95, na.rm = TRUE), 2),
    P99    = round(quantile(indexP_med, 0.99, na.rm = TRUE), 2),
    n_locality_months = n(),
    .groups = "drop"
  ) %>%
  arrange(virus, desc(Median))

print(district_summary)

write_csv(
  district_summary,
  "IndexP_district_summary_long.csv"
)

# Format the district summary in wide form

district_table <- monthly_district %>%
  group_by(virus, district) %>%
  summarise(
    Mean   = round(mean(indexP_med, na.rm = TRUE), 2),
    SD     = round(sd(indexP_med, na.rm = TRUE), 2),
    Median = round(median(indexP_med, na.rm = TRUE), 2),
    Min    = round(min(indexP_med, na.rm = TRUE), 2),
    Max    = round(max(indexP_med, na.rm = TRUE), 2),
    IQR    = round(IQR(indexP_med, na.rm = TRUE), 2),
    P95    = round(quantile(indexP_med, 0.95, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    `Mean (SD)` = paste0(Mean, " (", SD, ")"),
    `Median (Range)` = paste0(Median, " (", Min, "–", Max, ")")
  ) %>%
  select(
    virus,
    district,
    `Mean (SD)`,
    `Median (Range)`,
    IQR,
    P95
  ) %>%
  pivot_wider(
    names_from = virus,
    values_from = c(`Mean (SD)`, `Median (Range)`, IQR, P95)
  ) %>%
  rename(
    `CHIK Mean (SD)` = `Mean (SD)_CHIK`,
    `CHIK Median (Range)` = `Median (Range)_CHIK`,
    `CHIK IQR` = `IQR_CHIK`,
    `CHIK P95` = `P95_CHIK`,
    `DENV Mean (SD)` = `Mean (SD)_DENV`,
    `DENV Median (Range)` = `Median (Range)_DENV`,
    `DENV IQR` = `IQR_DENV`,
    `DENV P95` = `P95_DENV`
  ) %>%
  arrange(district)

print(district_table)

write_csv(
  district_table,
  "IndexP_district_summary_FINAL.csv"
)

# Add descriptive posterior bounds

district_bounds <- monthly_district %>%
  group_by(virus, district) %>%
  summarise(
    bound_low  = round(median(indexP_p025, na.rm = TRUE), 2),
    bound_high = round(median(indexP_p975, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    `Median posterior bounds` = paste0(bound_low, "–", bound_high)
  ) %>%
  select(virus, district, `Median posterior bounds`) %>%
  pivot_wider(
    names_from = virus,
    values_from = `Median posterior bounds`
  ) %>%
  rename(
    `CHIK median posterior bounds` = CHIK,
    `DENV median posterior bounds` = DENV
  )

district_table_with_bounds <- district_table %>%
  left_join(district_bounds, by = "district") %>%
  select(
    district,
    `CHIK Mean (SD)`,
    `CHIK Median (Range)`,
    `CHIK IQR`,
    `CHIK P95`,
    `CHIK median posterior bounds`,
    `DENV Mean (SD)`,
    `DENV Median (Range)`,
    `DENV IQR`,
    `DENV P95`,
    `DENV median posterior bounds`
  )

print(district_table_with_bounds)

write_csv(
  district_table_with_bounds,
  "IndexP_district_summary_WITH_POSTERIOR_BOUNDS_FINAL.csv"
)

#==============================================================================
# 4) Create pooled national summaries
#==============================================================================


national_pooled_summary <- monthly %>%
  group_by(virus) %>%
  summarise(
    level    = "National pooled locality-month",
    district = "All",
    Median   = round(median(indexP_med, na.rm = TRUE), 2),
    Mean     = round(mean(indexP_med, na.rm = TRUE), 2),
    SD       = round(sd(indexP_med, na.rm = TRUE), 2),
    Min      = round(min(indexP_med, na.rm = TRUE), 2),
    Max      = round(max(indexP_med, na.rm = TRUE), 2),
    IQR      = round(IQR(indexP_med, na.rm = TRUE), 2),
    P95      = round(quantile(indexP_med, 0.95, na.rm = TRUE), 2),
    P99      = round(quantile(indexP_med, 0.99, na.rm = TRUE), 2),
    n_locality_months = n(),
    .groups  = "drop"
  )

print(national_pooled_summary)

# Add descriptive posterior bounds

national_pooled_bounds <- monthly %>%
  group_by(virus) %>%
  summarise(
    posterior_bound_low  = round(median(indexP_p025, na.rm = TRUE), 2),
    posterior_bound_high = round(median(indexP_p975, na.rm = TRUE), 2),
    `Median posterior bounds` =
      paste0(posterior_bound_low, "–", posterior_bound_high),
    .groups = "drop"
  )

national_pooled_final <- national_pooled_summary %>%
  left_join(national_pooled_bounds, by = "virus")

print(national_pooled_final)

write_csv(
  national_pooled_final,
  "IndexP_national_pooled_summary_WITH_POSTERIOR_BOUNDS_FINAL.csv"
)
