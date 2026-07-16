############################################################
# CLIMATE EXPLANATION — DISTRICT LEVEL (FINAL)
# Monthly climatology plots, no smoothing
############################################################

rm(list = ls()); gc()

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

############################################################
# 1) LOAD DAILY CLIMATE DATA
############################################################

climate_raw <- read_csv(file.choose()) %>%
  mutate(
    date    = as.Date(date),
    site_id = str_trim(tolower(site_id)),
    tmean   = as.numeric(tmean),
    tdew    = as.numeric(tdew)
  )

############################################################
# 2) LOAD LOCALITY → DISTRICT LOOKUP
############################################################

lookup <- read_csv(file.choose()) %>%
  mutate(
    site_id  = str_trim(tolower(site_id)),
    district = str_trim(toupper(district))
  )

############################################################
# 3) REMOVE 2013
############################################################

climate_raw <- climate_raw %>%
  filter(date >= as.Date("2014-01-01"))

############################################################
# 4) CONVERT TO MONTHLY LOCALITY-LEVEL CLIMATE
# This follows the same RH logic used in your MVSE / Index P workflow:
# daily tmean + daily tdew -> monthly mean tmean + monthly mean tdew
############################################################

climate_monthly <- climate_raw %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(site_id, month) %>%
  summarise(
    tmean = mean(tmean, na.rm = TRUE),
    tdew  = mean(tdew,  na.rm = TRUE),
    .groups = "drop"
  )

############################################################
# 5) RELATIVE HUMIDITY FROM MONTHLY TMEAN AND MONTHLY TDEW
############################################################

sat_vap <- function(T) {
  6.112 * exp((17.67 * T) / (T + 243.5))
}

climate_monthly <- climate_monthly %>%
  mutate(
    RH = 100 * (sat_vap(tdew) / sat_vap(tmean)),
    RH = pmin(pmax(RH, 0), 100)
  )

############################################################
# 6) JOIN DISTRICT
############################################################

climate_monthly <- climate_monthly %>%
  left_join(lookup, by = "site_id")

print(paste("Missing districts:", sum(is.na(climate_monthly$district))))

############################################################
# 7) LONG-TERM MONTHLY CLIMATOLOGY BY DISTRICT
# Values are monthly medians across locality-month observations.
# Months are ordered from Nov to Oct to show the warm/humid season first.
############################################################

climate_district <- climate_monthly %>%
  mutate(month_num = month(month)) %>%
  group_by(district, month_num) %>%
  summarise(
    tmean = median(tmean, na.rm = TRUE),
    RH    = median(RH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month_lab = factor(
      month_num,
      levels = c(11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr",
                 "May", "Jun", "Jul", "Aug", "Sep", "Oct")
    )
  ) %>%
  arrange(district, month_lab)

############################################################
# 8) DEFINE REGIONS
############################################################

east_districts <- c("FLACQ", "GRAND PORT")
west_districts <- c("BLACK RIVER", "PORT LOUIS")

climate_district <- climate_district %>%
  mutate(
    region_group = case_when(
      district %in% east_districts ~ "East",
      district %in% west_districts ~ "West",
      TRUE ~ "Other"
    )
  )

############################################################
# 9) TEMPERATURE FACET PLOT
############################################################

p_temp_facet <- ggplot(
  climate_district,
  aes(x = month_lab, y = tmean, group = 1)
) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ district, ncol = 3) +
  labs(
    title = "Monthly Temperature Climatology by District",
    x = "Month",
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_temp_facet)

############################################################
# 10) RELATIVE HUMIDITY FACET PLOT
############################################################

p_rh_facet <- ggplot(
  climate_district,
  aes(x = month_lab, y = RH, group = 1)
) +
  geom_line(color = "darkgreen", linewidth = 1.1) +
  geom_point(color = "darkgreen", size = 2) +
  facet_wrap(~ district, ncol = 3) +
  labs(
    title = "Monthly Relative Humidity Climatology by District",
    x = "Month",
    y = "Relative Humidity (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_rh_facet)

############################################################
# 11) SAVE FACET PLOTS AT 500 DPI
############################################################

ggsave(
  filename = "Temperature_facet_500dpi.png",
  plot = p_temp_facet,
  width = 10,
  height = 7,
  dpi = 500,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "Humidity_facet_500dpi.png",
  plot = p_rh_facet,
  width = 10,
  height = 7,
  dpi = 500,
  units = "in",
  bg = "white"
)

############################################################
# 12) OPTIONAL: SAVE PDF VERSION FOR THESIS/WORD
############################################################

ggsave(
  filename = "Temperature_facet.pdf",
  plot = p_temp_facet,
  width = 10,
  height = 7,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "Humidity_facet.pdf",
  plot = p_rh_facet,
  width = 10,
  height = 7,
  units = "in",
  bg = "white"
)

cat("\n✅ DONE — climate climatology plots saved at 500 dpi\n")