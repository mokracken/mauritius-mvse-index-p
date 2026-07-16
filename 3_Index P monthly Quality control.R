# ==============================================================================
# SCRIPT 3 — MVSE INDEX P QUALITY CONTROL AND DESCRIPTIVE ANALYSIS

# Title: Divergent climate suitability profiles for dengue and chikungunya 
# transmission by Aedes albopictus in Mauritius

# Purpose:
# Perform quality control and descriptive analysis of locality-month posterior 
# Median Index P estimates for dengue and chikungunya generated in Script 2, 
# including their distribution, uncertainty, percentile thresholds, 
# extreme values, and diagnostic plots.

# Input:
#   Locality-month posterior Index P output files generated in Script 2:
#   MVSE_indexP_denv_monthly_FINAL.csv
#   MVSE_indexP_chik_monthly_FINAL.csv

# Output:
#   Virus-specific descriptive, uncertainty, percentile, and extreme-value
#   summary CSV files for DENV and CHIK.

#   Virus-specific histogram, density, and boxplot figures on original
#   and log10 scales.

#   Supplementary Figure S3 national DENV–CHIK boxplot in PNG and PDF formats.

# Author: Amar Teeluck, Benn Sartorius, Oyelola Adegboye, Stephan Karl, Emma McBryde, 
# *Eloise Skinner

# Institution: The University of Queensland
# Date: 06/03/2026
# R version: 4.4.1
# ==============================================================================
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# ==============================================================================
# 1) Define descriptive summary and plotting function
# ==============================================================================
summary_stats <- function(data, var){
  
  data %>%
    summarise(
      n_rows = n(),
      n_nonmissing = sum(!is.na({{var}})),
      min = min({{var}}, na.rm = TRUE),
      p01 = quantile({{var}}, .01, na.rm = TRUE),
      p05 = quantile({{var}}, .05, na.rm = TRUE),
      p25 = quantile({{var}}, .25, na.rm = TRUE),
      median = median({{var}}, na.rm = TRUE),
      mean = mean({{var}}, na.rm = TRUE),
      p75 = quantile({{var}}, .75, na.rm = TRUE),
      p90 = quantile({{var}}, .90, na.rm = TRUE),
      p95 = quantile({{var}}, .95, na.rm = TRUE),
      p99 = quantile({{var}}, .99, na.rm = TRUE),
      p995 = quantile({{var}}, .995, na.rm = TRUE),
      max = max({{var}}, na.rm = TRUE),
      sd = sd({{var}}, na.rm = TRUE),
      mean_to_median_ratio = mean({{var}}, na.rm = TRUE) /
        median({{var}}, na.rm = TRUE),
      n_above_0_5 = sum({{var}} > 0.5, na.rm = TRUE),
      prop_above_0_5 = mean({{var}} > 0.5, na.rm = TRUE),
      n_above_1 = sum({{var}} > 1, na.rm = TRUE),
      prop_above_1 = mean({{var}} > 1, na.rm = TRUE),
      n_above_2 = sum({{var}} > 2, na.rm = TRUE),
      prop_above_2 = mean({{var}} > 2, na.rm = TRUE)
    )
}


make_plots <- function(data, var_name, prefix){
  
  data_plot <- data %>%
    mutate(
      value = .data[[var_name]],
      log10_value = log10(value + 1e-6)
    )
  
  ggsave(
    paste0("Hist_", prefix, ".png"),
    ggplot(data_plot, aes(x = value)) +
      geom_histogram(bins = 80, fill = "steelblue") +
      theme_minimal() +
      labs(
        title = paste0(prefix, " Index P distribution"),
        x = var_name,
        y = "Count"
      ),
    width = 8, height = 5, dpi = 300, bg = "white"
  )
  
  ggsave(
    paste0("Hist_", prefix, "_log10.png"),
    ggplot(data_plot, aes(x = log10_value)) +
      geom_histogram(bins = 80, fill = "darkorange") +
      theme_minimal() +
      labs(
        title = paste0(prefix, " Index P distribution, log10 scale"),
        x = paste0("log10(", var_name, " + 1e-6)"),
        y = "Count"
      ),
    width = 8, height = 5, dpi = 300, bg = "white"
  )
  
  ggsave(
    paste0("Density_", prefix, ".png"),
    ggplot(data_plot, aes(x = value)) +
      geom_density(fill = "skyblue", alpha = 0.6) +
      theme_minimal() +
      labs(
        title = paste0(prefix, " Index P density"),
        x = var_name,
        y = "Density"
      ),
    width = 8, height = 5, dpi = 300, bg = "white"
  )
  
  ggsave(
    paste0("Box_", prefix, ".png"),
    ggplot(data_plot, aes(y = value)) +
      geom_boxplot(fill = "lightgreen") +
      theme_minimal() +
      labs(
        title = paste0(prefix, " Index P boxplot"),
        y = var_name
      ),
    width = 5, height = 6, dpi = 300, bg = "white"
  )
  
  ggsave(
    paste0("Box_", prefix, "_log10.png"),
    ggplot(data_plot, aes(y = log10_value)) +
      geom_boxplot(fill = "orchid") +
      theme_minimal() +
      labs(
        title = paste0(prefix, " Index P boxplot, log10 scale"),
        y = paste0("log10(", var_name, " + 1e-6)")
      ),
    width = 5, height = 6, dpi = 300, bg = "white"
  )
}

# ==============================================================================
# 2) Process one virus-specific Index P dataset
# ==============================================================================
process_mvse <- function(file_path, virus_name){
  
  

  monthly <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      date = as.Date(date),
      site_id = str_trim(as.character(site_id))
    )
  
  
  required_cols <- c("date", "site_id", "indexP_med", "indexP_p025", "indexP_p975")
  missing_cols <- setdiff(required_cols, names(monthly))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ",
      virus_name,
      ": ",
      paste(missing_cols, collapse = ", ")
    )
  }


  monthly <- monthly %>%
    mutate(
      uncertainty_width = indexP_p975 - indexP_p025,
      uncertainty_ratio = ifelse(
        indexP_med > 0,
        uncertainty_width / indexP_med,
        NA_real_
      )
    )
  
 

  stats <- summary_stats(monthly, indexP_med)
  
 
  write_csv(
    stats,
    paste0("IndexP_monthly_stats_", virus_name, ".csv")
  )
  

  uncertainty_stats <- monthly %>%
    summarise(
      n = n(),
      median_uncertainty_width = median(uncertainty_width, na.rm = TRUE),
      p95_uncertainty_width = quantile(uncertainty_width, 0.95, na.rm = TRUE),
      p99_uncertainty_width = quantile(uncertainty_width, 0.99, na.rm = TRUE),
      max_uncertainty_width = max(uncertainty_width, na.rm = TRUE),
      median_uncertainty_ratio = median(uncertainty_ratio, na.rm = TRUE),
      p95_uncertainty_ratio = quantile(uncertainty_ratio, 0.95, na.rm = TRUE),
      max_uncertainty_ratio = max(uncertainty_ratio, na.rm = TRUE)
    )
  
 
  write_csv(
    uncertainty_stats,
    paste0("IndexP_uncertainty_stats_", virus_name, ".csv")
  )
  

  cuts <- quantile(
    monthly$indexP_med,
    probs = c(.50, .75, .90, .95, .99, .995),
    na.rm = TRUE
  )
  
  percentile_summary <- data.frame(
    Percentile = names(cuts),
    Value = round(as.numeric(cuts), 3)
  )
  
  
  write_csv(
    percentile_summary,
    paste0("IndexP_percentile_summary_", virus_name, ".csv")
  )
  
  
  p99_cut <- quantile(monthly$indexP_med, 0.99, na.rm = TRUE)
  
  extreme_rows <- monthly %>%
    filter(indexP_med >= p99_cut) %>%
    arrange(desc(indexP_med))
  
  write_csv(
    extreme_rows,
    paste0("IndexP_extreme_top_1percent_", virus_name, ".csv")
  )
  
  
  
  make_plots(
    data = monthly,
    var_name = "indexP_med",
    prefix = paste0("monthly_", virus_name)
  )
  
 }

# ==============================================================================
# 3) Process dengue and chikungunya Index P outputs
# ==============================================================================
file_denv <- file.choose()

file_chik <- file.choose()

process_mvse(file_denv, "DENV")
process_mvse(file_chik, "CHIK")

# =============================================================================
# 4) Compare national locality-month Index P distributions
# =============================================================================

boxplot_denv <- read_csv(
  file_denv,
  show_col_types = FALSE
) %>%
  transmute(
    virus = "Dengue",
    indexP_med = as.numeric(indexP_med)
  )

boxplot_chik <- read_csv(
  file_chik,
  show_col_types = FALSE
) %>%
  transmute(
    virus = "Chikungunya",
    indexP_med = as.numeric(indexP_med)
  )

indexp_boxplot <- bind_rows(
  boxplot_denv,
  boxplot_chik
) %>%
  filter(
    !is.na(indexP_med),
    indexP_med > 0
  ) %>%
  mutate(
    virus = factor(
      virus,
      levels = c("Dengue", "Chikungunya")
    )
  )

p_national_boxplot <- ggplot(
  indexp_boxplot,
  aes(
    x = virus,
    y = indexP_med
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.alpha = 0.25,
    outlier.size = 0.8
  ) +
  scale_y_log10() +
  labs(
    title = "National distribution of locality-month posterior median Index P",
    x = "Virus",
    y = "Posterior median Index P (log10 scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )

print(p_national_boxplot)

ggsave(
  filename = "Supplementary_Figure_S3_National_IndexP_boxplot.png",
  plot = p_national_boxplot,
  width = 7,
  height = 6,
  dpi = 500,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "Supplementary_Figure_S3_National_IndexP_boxplot.pdf",
  plot = p_national_boxplot,
  width = 7,
  height = 6,
  units = "in",
  bg = "white"
)