############################################################
# FINAL SCRIPT — DENV + CHIK (CLEAN + ONE LEGEND OUTSIDE)
############################################################

rm(list = ls()); gc()

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)
library(patchwork)
library(grid)

############################################################
# 1) LOAD DATA
############################################################

mvca <- st_read(file.choose())
denv_wide <- read_csv(file.choose())
chik_wide <- read_csv(file.choose())

############################################################
# 2) CLEAN KEYS
############################################################

mvca$VCA <- trimws(mvca$VCA)
denv_wide$site_id <- trimws(denv_wide$site_id)
chik_wide$site_id <- trimws(chik_wide$site_id)

############################################################
# 3) PREP FUNCTION
############################################################

prepare_data <- function(data_wide, mvca) {
  
  data_long <- data_wide %>%
    pivot_longer(
      cols = Jan:Dec,
      names_to = "month_name",
      values_to = "monthly_median"
    )
  
  mvca %>%
    left_join(data_long, by = c("VCA" = "site_id"))
}

denv_map <- prepare_data(denv_wide, mvca)
chik_map <- prepare_data(chik_wide, mvca)

############################################################
# 4) PLOT FUNCTION (ALL PLOTS HAVE LEGEND)
############################################################

plot_month <- function(data_map, month_selected, virus_name) {
  
  df <- data_map %>%
    filter(month_name == month_selected)
  
  ggplot(df) +
    geom_sf(aes(fill = monthly_median), color = "grey30", size = 0.1) +
    
    scale_fill_gradientn(
      colours = c(
        "#3b6fb6", "#5fa8d3", "#8ecae6", "#bde0fe", "#ffffff",
        "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"
      ),
      values = scales::rescale(c(
        0, 0.1, 0.2, 0.3, 0.4, 0.5,
        2, 3.5, 5, 6.5, 8, 9
      )),
      breaks = c(0, 0.5, 2, 3.5, 5, 6.5, 8),
      labels = c("0", "0.5", "2", "3.5", "5", "6.5", "≥8"),
      limits = c(0, 9),
      name = "Index P",
      guide = guide_colourbar(
        direction = "vertical",
        barheight = unit(5.5, "cm"),
        barwidth  = unit(0.5, "cm")
      )
    ) +
    
    ggtitle(month_selected) +
    
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 7),
      legend.position = "right"
    )
}

############################################################
# 5) SEASONS
############################################################

summer_months <- c("Nov","Dec","Jan","Feb","Mar","Apr")
winter_months <- c("May","Jun","Jul","Aug","Sep","Oct")

############################################################
# 6) PANEL FUNCTION (COLLECT LEGEND PROPERLY)
############################################################

make_panels <- function(data_map, virus_name) {
  
  # Summer
  summer_plots <- lapply(summer_months, function(m)
    plot_month(data_map, m, virus_name))
  
  summer_map <- wrap_plots(summer_plots, ncol = 3) +
    plot_layout(guides = "collect") +
    plot_annotation(title = paste(virus_name, "- Summer (Nov–Apr)"))
  
  summer_map <- summer_map + theme(legend.position = "right")
  
  # Winter
  winter_plots <- lapply(winter_months, function(m)
    plot_month(data_map, m, virus_name))
  
  winter_map <- wrap_plots(winter_plots, ncol = 3) +
    plot_layout(guides = "collect") +
    plot_annotation(title = paste(virus_name, "- Winter (May–Oct)"))
  
  winter_map <- winter_map + theme(legend.position = "right")
  
  list(summer = summer_map, winter = winter_map)
}

############################################################
# 7) CREATE MAPS
############################################################

denv_maps <- make_panels(denv_map, "Dengue")
chik_maps <- make_panels(chik_map, "Chikungunya")

############################################################
# 8) SAVE OUTPUT
############################################################

png("DENV_Summer.png", width = 3000, height = 2400, res = 300)
print(denv_maps$summer)
dev.off()

png("DENV_Winter.png", width = 3000, height = 2400, res = 300)
print(denv_maps$winter)
dev.off()

png("CHIK_Summer.png", width = 3000, height = 2400, res = 300)
print(chik_maps$summer)
dev.off()

png("CHIK_Winter.png", width = 3000, height = 2400, res = 300)
print(chik_maps$winter)
dev.off()