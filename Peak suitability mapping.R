############################################################
# FINAL SCRIPT — PEAK SUITABILITY MAPS FROM SPATIAL METRICS
# Clean virus labels: Dengue / Chikungunya
############################################################

rm(list = ls()); gc()

library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(stringr)

############################################################
# 1) LOAD DATA
############################################################

# Select MVCA shapefile / geopackage / spatial layer
mvca <- st_read(file.choose())

# Select IndexP_spatial_metrics.csv
# Required columns:
# site_id, virus, median_suitability, peak_suitability
spatial_metrics <- read_csv(file.choose(), show_col_types = FALSE)

############################################################
# 2) CLEAN KEYS AND VIRUS LABELS
############################################################

mvca <- mvca %>%
  mutate(
    VCA = str_trim(as.character(VCA))
  )

spatial_metrics <- spatial_metrics %>%
  mutate(
    site_id = str_trim(as.character(site_id)),
    virus = str_trim(as.character(virus)),
    peak_suitability = as.numeric(peak_suitability),
    
    # Recode virus labels for final outputs
    virus = case_when(
      virus %in% c("DENV", "denv", "Dengue", "dengue") ~ "Dengue",
      virus %in% c("CHIK", "chik", "CHIKV", "chikv", 
                   "Chikungunya", "chikungunya") ~ "Chikungunya",
      TRUE ~ virus
    )
  )

############################################################
# 3) BASIC CHECKS
############################################################

cat("\nVirus labels in spatial_metrics:\n")
print(unique(spatial_metrics$virus))

cat("\nColumns in spatial_metrics:\n")
print(names(spatial_metrics))

cat("\nPeak suitability range by virus:\n")
print(
  spatial_metrics %>%
    group_by(virus) %>%
    summarise(
      n = n(),
      min_peak = min(peak_suitability, na.rm = TRUE),
      median_peak = median(peak_suitability, na.rm = TRUE),
      max_peak = max(peak_suitability, na.rm = TRUE),
      .groups = "drop"
    )
)

############################################################
# 4) CHECK JOIN BETWEEN CSV AND SHAPEFILE
############################################################

join_check <- spatial_metrics %>%
  anti_join(
    mvca %>% st_drop_geometry(),
    by = c("site_id" = "VCA")
  )

cat("\nNumber of site_id values in CSV not matching VCA in shapefile:\n")
print(nrow(join_check))

if (nrow(join_check) > 0) {
  cat("\nExamples of unmatched site_id values:\n")
  print(head(join_check$site_id, 20))
}

############################################################
# 5) PEAK SUITABILITY MAP FUNCTION
############################################################

plot_peak_map <- function(spatial_metrics, mvca, virus_name) {
  
  df <- mvca %>%
    left_join(
      spatial_metrics %>% filter(virus == virus_name),
      by = c("VCA" = "site_id")
    )
  
  cat("\n", virus_name, "matched localities:", sum(!is.na(df$peak_suitability)), "\n")
  cat(virus_name, "missing localities:", sum(is.na(df$peak_suitability)), "\n")
  
  ggplot(df) +
    geom_sf(
      aes(fill = peak_suitability),
      color = "grey30",
      size = 0.1
    ) +
    scale_fill_gradientn(
      colours = c(
        "#fff7bc", "#fee391", "#fec44f",
        "#fe9929", "#ec7014", "#cc4c02", "#990000"
      ),
      values = scales::rescale(c(3, 6, 9, 12, 15, 18, 22)),
      breaks = c(3, 6, 9, 12, 15, 18, 22),
      labels = c("3", "6", "9", "12", "15", "18", "22"),
      limits = c(3, 22),
      name = "Peak suitability\n(Index P)",
      na.value = "grey90"
    ) +
    ggtitle(paste0(virus_name, " - Spatial Peak Suitability (p95)")) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 8),
      legend.position = "right"
    )
}

############################################################
# 6) CREATE MAPS
############################################################

dengue_peak_map <- plot_peak_map(
  spatial_metrics = spatial_metrics,
  mvca = mvca,
  virus_name = "Dengue"
)

chikungunya_peak_map <- plot_peak_map(
  spatial_metrics = spatial_metrics,
  mvca = mvca,
  virus_name = "Chikungunya"
)

print(dengue_peak_map)
print(chikungunya_peak_map)

############################################################
# 7) SAVE OUTPUT AT 500 DPI
############################################################

ggsave(
  filename = "Dengue_Peak_Suitability_Map_500dpi.png",
  plot = dengue_peak_map,
  width = 8,
  height = 6.5,
  dpi = 500,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "Chikungunya_Peak_Suitability_Map_500dpi.png",
  plot = chikungunya_peak_map,
  width = 8,
  height = 6.5,
  dpi = 500,
  units = "in",
  bg = "white"
)

############################################################
# 8) OPTIONAL PDF OUTPUT
############################################################

ggsave(
  filename = "Dengue_Peak_Suitability_Map.pdf",
  plot = dengue_peak_map,
  width = 8,
  height = 6.5,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "Chikungunya_Peak_Suitability_Map.pdf",
  plot = chikungunya_peak_map,
  width = 8,
  height = 6.5,
  units = "in",
  bg = "white"
)

cat("\n✅ DONE — peak suitability maps saved with Dengue / Chikungunya labels\n")