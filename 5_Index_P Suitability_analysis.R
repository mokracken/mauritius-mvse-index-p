# =============================================================================
# Script 4 — MVSE INDEX P COMPARISON AND SPATIAL SUMMARIES

# Title:
# Divergent climate suitability profiles for dengue and chikungunya
# transmission by Aedes albopictus in Mauritius

# Purpose:
# Compare dengue and chikungunya Index P estimates across time and space,
# including national monthly suitability, annual transmission-window duration,
# locality-level median and peak suitability, and calendar-month summaries
# for mapping.

# Inputs:
#   1. MVSE_indexP_denv_monthly_FINAL.csv
#   2. MVSE_indexP_chik_monthly_FINAL.csv

# Required columns:
#   date, site_id, indexP_med

# Outputs:
#   Figures:
#     National_IndexP_IQR.png
#     Transmission_window_duration_across_years_500dpi.png (Supplementary Figure S4)

#   National summaries:
#     IndexP_national_yearly_summary.csv
#     Transmission_window_national_year.csv

#   Locality-level mapping outputs:
#     IndexP_spatial_metrics.csv
#     IndexP_peak_scaled.csv
#     IndexP_peak_metrics_DENV.csv
#     IndexP_peak_metrics_CHIK.csv
#     IndexP_monthly_values_wide_DENV.csv
#     IndexP_monthly_values_wide_CHIK.csv
#     IndexP_monthly_scaled.csv
#     IndexP_monthly_class_wide_DENV.csv
#     IndexP_monthly_class_wide_CHIK.csv
#     IndexP_monthly_p95.csv
#     IndexP_monthly_p95_wide_DENV.csv
#     IndexP_monthly_p95_wide_CHIK.csv
#     Supplementary_Table_Seasonal_Monthly_IndexP_values.csv (Supplementary S5)
#     Supplementary_Table_Spatial_IndexP_values.csv (Supplementary S6)

# =============================================================================
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggpattern)
#==============================================================================
# 1) Load locality-monthly Index P data
#==============================================================================
denv <- read_csv(file.choose()) %>%
  mutate(date = as.Date(date), virus = "DENV") %>%
  rename(month = date)

chik <- read_csv(file.choose()) %>%
  mutate(date = as.Date(date), virus = "CHIK") %>%
  rename(month = date)

monthly <- bind_rows(denv, chik) %>%
  mutate(site_id = as.character(site_id))

lookup <- read_csv(file.choose()) %>%
  mutate(site_id = as.character(site_id))

monthly <- monthly %>%
  left_join(lookup, by = "site_id")

if (any(is.na(monthly$district))) {
  stop("Some site_id values were not matched to a district.")
}

#==============================================================================
# 2) Plot national monthly suitability
#==============================================================================
national <- monthly %>%
  group_by(virus, month) %>%
  summarise(
    P_med  = median(indexP_med, na.rm = TRUE),
    P_low  = quantile(indexP_med, 0.25, na.rm = TRUE),
    P_high = quantile(indexP_med, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(virus, month)


p_nat <- ggplot(national, aes(x = month)) +
  geom_ribbon(
    aes(ymin = P_low, ymax = P_high, fill = virus),
    alpha = 0.25
  ) +
  geom_line(
    aes(y = P_med, color = virus),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c("DENV" = "steelblue", "CHIK" = "firebrick"),
    labels = c("DENV" = "Dengue", "CHIK" = "Chikungunya"),
    name = "Virus"
  ) +
  scale_fill_manual(
    values = c("DENV" = "steelblue", "CHIK" = "firebrick"),
    labels = c("DENV" = "Dengue", "CHIK" = "Chikungunya"),
    name = "Virus"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  labs(
    title = "National Climate-Driven Transmission Suitability (Index P)",
    x = "Year",
    y = "Index P"
  )

print(p_nat)

ggsave(
  filename = "National_IndexP_IQR.png",
  plot = p_nat,
  width = 10,
  height = 5,
  dpi = 1200,
  units = "in",
  bg = "white"
)
#==============================================================================
# 3) Summarise national suitability by year
#==============================================================================
national_summary <- monthly %>%
  mutate(year = year(month)) %>%
  group_by(virus, year) %>%
  summarise(
    supportive_locality_months = sum(indexP_med > 0.5, na.rm = TRUE),
    peak_suitability   = quantile(indexP_med, 0.95, na.rm = TRUE),
    median_suitability = median(indexP_med, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(national_summary, "IndexP_national_yearly_summary.csv")

#==============================================================================
# 4) Calculate and export locality-level spatial metrics
#==============================================================================
peak_metrics <- monthly %>%
  group_by(virus, site_id) %>%
  summarise(
    median_suitability = round(median(indexP_med, na.rm=TRUE),3),
    peak_suitability   = round(quantile(indexP_med,0.95,na.rm=TRUE),3),
    .groups="drop"
  )

write_csv(peak_metrics, "IndexP_spatial_metrics.csv")

peak_scaled <- peak_metrics %>%
  group_by(virus) %>%
  mutate(

    low_threshold = 0.5,
    
    upper_limit = quantile(peak_suitability, 0.95, na.rm = TRUE),
    
    break1 = low_threshold + (upper_limit - low_threshold)/3,
    break2 = low_threshold + 2*(upper_limit - low_threshold)/3,
    
    suit_class = case_when(
      peak_suitability < low_threshold ~ "Low",
      peak_suitability < break1 ~ "Moderate",
      peak_suitability < break2 ~ "High",
      TRUE ~ "Extreme"
    ),
    
    suit_class = factor(
      suit_class,
      levels = c("Low","Moderate","High","Extreme")
    )
  ) %>%
  ungroup()


write_csv(peak_scaled, "IndexP_peak_scaled.csv")


# DENV
write_csv(
  peak_metrics %>%
    filter(virus == "DENV") %>%
    select(site_id, median_suitability, peak_suitability),
  "IndexP_peak_metrics_DENV.csv"
)

# CHIK
write_csv(
  peak_metrics %>%
    filter(virus == "CHIK") %>%
    select(site_id, median_suitability, peak_suitability),
  "IndexP_peak_metrics_CHIK.csv"
)
#==============================================================================
# 5) Calculate and export calendar-month suitability
#==============================================================================
monthly <- monthly %>%
  mutate(month_name = factor(month.abb[month(month)], levels=month.abb))

monthly_median <- monthly %>%
  group_by(virus, site_id, month_name) %>%
  summarise(
    monthly_median = round(median(indexP_med, na.rm=TRUE),3),
    .groups="drop"
  )


write_csv(
  monthly_median %>%
    filter(virus == "DENV") %>%
    pivot_wider(names_from = month_name, values_from = monthly_median),
  "IndexP_monthly_values_wide_DENV.csv"
)

write_csv(
  monthly_median %>%
    filter(virus == "CHIK") %>%
    pivot_wider(names_from = month_name, values_from = monthly_median),
  "IndexP_monthly_values_wide_CHIK.csv"
)


monthly_median_scaled <- monthly_median %>%
  group_by(virus) %>%
  mutate(
    low_threshold = 0.5,
    upper_limit = quantile(monthly_median, 0.95, na.rm = TRUE),
    break1 = low_threshold + (upper_limit - low_threshold) / 3,
    break2 = low_threshold + 2 * (upper_limit - low_threshold) / 3,
    
    suit_class = case_when(
      monthly_median < low_threshold ~ "Low",
      monthly_median < break1 ~ "Moderate",
      monthly_median < break2 ~ "High",
      TRUE ~ "Extreme"
    ),
    
    suit_class = factor(suit_class, levels = c("Low","Moderate","High","Extreme"))
  ) %>%
  ungroup()

write_csv(monthly_median_scaled, "IndexP_monthly_scaled.csv")


write_csv(
  monthly_median_scaled %>%
    filter(virus == "DENV") %>%
    select(site_id, month_name, suit_class) %>%
    pivot_wider(names_from = month_name, values_from = suit_class),
  "IndexP_monthly_class_wide_DENV.csv"
)

write_csv(
  monthly_median_scaled %>%
    filter(virus == "CHIK") %>%
    select(site_id, month_name, suit_class) %>%
    pivot_wider(names_from = month_name, values_from = suit_class),
  "IndexP_monthly_class_wide_CHIK.csv"
)

#==============================================================================
# 6) Calculate and export calendar-month peak suitability
#==============================================================================
monthly_p95 <- monthly %>%
  group_by(virus, site_id, month_name) %>%
  summarise(
    monthly_p95 = round(quantile(indexP_med,0.95,na.rm=TRUE),3),
    .groups="drop"
  )

write_csv(monthly_p95, "IndexP_monthly_p95.csv")


write_csv(
  monthly_p95 %>%
    filter(virus == "DENV") %>%
    pivot_wider(names_from = month_name, values_from = monthly_p95),
  "IndexP_monthly_p95_wide_DENV.csv"
)

write_csv(
  monthly_p95 %>%
    filter(virus == "CHIK") %>%
    pivot_wider(names_from = month_name, values_from = monthly_p95),
  "IndexP_monthly_p95_wide_CHIK.csv"
)
#==============================================================================
# 7) Plot annual transmission-window duration
#==============================================================================
national_window_year <- monthly %>%
  group_by(virus, month) %>%
  summarise(P_med = median(indexP_med, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year = year(month),
    supportive = as.integer(P_med > 0.5)
  ) %>%
  group_by(virus, year) %>%
  summarise(
    months_observed = n(),
    months_supportive = sum(supportive),
    .groups = "drop"
  )

write_csv(national_window_year, "Transmission_window_national_year.csv")

national_window_year_plot <- national_window_year %>%
  mutate(
    virus = factor(virus, levels = c("CHIK", "DENV")),
    year_status = ifelse(months_observed < 12, "Partial year", "Complete year"),
    year_status = factor(year_status, levels = c("Complete year", "Partial year"))
  )

p_transmission_window_year <- ggplot(
  national_window_year_plot,
  aes(
    x = factor(year),
    y = months_supportive
  )
) +
  geom_col_pattern(
    aes(pattern = year_status),
    width = 0.75,
    fill = "grey70",
    colour = "grey30",
    linewidth = 0.25,
    pattern_fill = "grey25",
    pattern_colour = "grey25",
    pattern_angle = 45,
    pattern_density = 0.20,
    pattern_spacing = 0.035,
    pattern_key_scale_factor = 0.6
  ) +
  facet_wrap(~ virus, nrow = 1) +
  scale_pattern_manual(
    values = c(
      "Complete year" = "none",
      "Partial year" = "stripe"
    ),
    name = "Year coverage"
  ) +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = seq(0, 12, by = 2),
    expand = c(0, 0)
  ) +
  labs(
    title = "Transmission Window Duration Across Years",
    x = "Year",
    y = "Months with national median Index P > 0.5"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.background = element_rect(fill = "white", colour = "grey40"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p_transmission_window_year)

ggsave(
  filename = "Transmission_window_duration_across_years_500dpi.png",
  plot = p_transmission_window_year,
  width = 9,
  height = 4.5,
  dpi = 500,
  units = "in",
  bg = "white"
)

# =============================================================================
# 8) Export supplementary locality-level Index P tables
# =============================================================================
supp_seasonal_indexp_values <- monthly %>%
  mutate(
    month_number = month(month),
    month_name = factor(month.abb[month_number], levels = month.abb)
  ) %>%
  group_by(virus, site_id, district, month_number, month_name) %>%
  summarise(
    median_IndexP = round(median(indexP_med, na.rm = TRUE), 3),
    median_p025 = round(median(indexP_p025, na.rm = TRUE), 3),
    median_p975 = round(median(indexP_p975, na.rm = TRUE), 3),
    n_years_available = sum(!is.na(indexP_med)),
    .groups = "drop"
  ) %>%
  arrange(virus, district, site_id, month_number)

write_csv(
  supp_seasonal_indexp_values,
  "Supplementary_Table_Seasonal_Monthly_IndexP_values.csv"
)

supp_spatial_indexp_values <- monthly %>%
  group_by(virus, site_id, district) %>%
  summarise(
    median_monthly_IndexP = round(median(indexP_med, na.rm = TRUE), 3),
    median_monthly_p025 = round(median(indexP_p025, na.rm = TRUE), 3),
    median_monthly_p975 = round(median(indexP_p975, na.rm = TRUE), 3),
    peak_IndexP_p95 = round(quantile(indexP_med, 0.95, na.rm = TRUE), 3),
    n_months = sum(!is.na(indexP_med)),
    .groups = "drop"
  ) %>%
  arrange(virus, district, site_id)

write_csv(
  supp_spatial_indexp_values,
  "Supplementary_Table_Spatial_IndexP_values.csv"
)
