# ==============================================================================
# Script 2 — MVSE INDEX P ESTIMATION

# Title: Divergent climate suitability profiles for dengue and chikungunya 
# transmission by Aedes albopictus in Mauritius

# Purpose:
#   Estimate locality-month posterior Index P for dengue and chikungunya
#   using monthly climate data and MVSE priors from Script 1.

# Adapted from:
#   Nakase GitHub MVSE tutorial/workflow, modified for Aedes albopictus,
#   dengue/chikungunya, and Mauritius locality-level climate data.
# Nakase, T. (2023). https://github.com/TaishiNakase/Index-P-estimation-and-applications

# Input:
#   Monthly locality-level climate CSV with month, Locality, tmean, and RH.
#   MVSE prior table exported from Script 1: MVSE_priors_from_climate.csv
#
# Output:
#   Locality-month posterior Index P summaries for dengue and chikungunya:
#   MVSE_indexP_denv_monthly_FINAL.csv
#   MVSE_indexP_chik_monthly_FINAL.csv

# Author: Amar Teeluck, Benn Sartorius, Oyelola Adegboye, Stephan Karl, Emma McBryde, 
# *Eloise Skinner
# Institution: University of Queensland
# Date: 06/03/2026
# R version: 4.4.1
# ==============================================================================
library(MVSE)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(furrr)
library(progressr)

# ==============================================================================
# 1) Setting up Parallel processing and Progress bar
# ==============================================================================
plan(multisession, workers = 16)
handlers("txtprogressbar")

# ==============================================================================
# 2) Define and save Posterior Draws
# ==============================================================================
SAVE_POSTERIOR_DRAWS <- TRUE

draw_dir <- "MVSE_posterior_draws"

if (SAVE_POSTERIOR_DRAWS && !dir.exists(draw_dir)) {
  dir.create(draw_dir, recursive = TRUE)
}

# ==============================================================================
# 3) Prepare climate data using 
#    ERA-5 Land Daily Aggregated - ECMWF Climate Reanalysis
# ==============================================================================
climate_raw <- read_csv(file.choose()) %>%
  mutate(
    date    = as.Date(date),
    site_id = str_trim(site_id),
    tmean   = as.numeric(tmean),
    tdew    = as.numeric(tdew)
  )

# Study period: January 2014 - October 2024
climate_raw <- climate_raw %>%
  filter(date >= as.Date("2014-01-01"))


climate_monthly <- climate_raw %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(site_id, month) %>%
  summarise(
    tmean = mean(tmean, na.rm = TRUE),
    tdew  = mean(tdew,  na.rm = TRUE),
    .groups = "drop"
  )


# Calculate monthly relative humidity using Tetens approximation
sat_vap <- function(T) 6.112 * exp((17.67 * T) / (T + 243.5))

climate_monthly <- climate_monthly %>%
  mutate(
    RH = 100 * (sat_vap(tdew) / sat_vap(tmean))
  )

# ==============================================================================
# 4) Import MVSE Prior Table from Script 1
# ==============================================================================
priors_tbl <- read_csv(file.choose())

# ==============================================================================
# 5) Define MVSE functions to fit MVSE model for one locality
# ==============================================================================
prepare_mvse <- function(df_site){
  
  out <- df_site %>%
    transmute(
      date = month,
      T    = tmean,
      H    = RH / 100
    ) %>%
    arrange(date)
  
  if (anyNA(out$T) | anyNA(out$H) | nrow(out) < 12)
    stop("Invalid site data")
  
  out
}


make_priors <- function(priors_tbl, virus_name){
  
  p <- priors_tbl %>%
    dplyr::filter(.data$virus == .env$virus_name)
  
  list(
    mosq_life_exp    = list(
      pars = c(mean = p$mean[p$param == "mosq_life_exp"],
               sd   = p$sd[p$param == "mosq_life_exp"]),
      dist = "normal"
    ),
    mosq_biting_freq = list(
      pars = c(mean = p$mean[p$param == "mosq_biting_freq"],
               sd   = p$sd[p$param == "mosq_biting_freq"]),
      dist = "normal"
    ),
    mosq_inc_per     = list(
      pars = c(mean = p$mean[p$param == "mosq_inc_per"],
               sd   = p$sd[p$param == "mosq_inc_per"]),
      dist = "normal"
    ),
    human_life_exp   = list(
      pars = c(mean = p$mean[p$param == "human_life_exp"],
               sd   = p$sd[p$param == "human_life_exp"]),
      dist = "normal"
    ),
    human_inc_per    = list(
      pars = c(mean = p$mean[p$param == "human_inc_per"],
               sd   = p$sd[p$param == "human_inc_per"]),
      dist = "normal"
    ),
    human_inf_per    = list(
      pars = c(mean = p$mean[p$param == "human_inf_per"],
               sd   = p$sd[p$param == "human_inf_per"]),
      dist = "normal"
    )
  )
}


run_mvse_one <- function(site, data, priors_albo, vc_value, virus_name){
  
  tryCatch({
    
    df_site <- data %>% filter(site_id == site)
    
    climate_mvse <- prepare_mvse(df_site)
    
    mvse_mod <- mvse_model(
      model_name     = paste0("MVSE_", virus_name, "_", site),
      climate_data   = climate_mvse,
      priors         = priors_albo,
      model_category = "custom",
      warning        = FALSE
    )
    
    fit <- sampling(
      mvse_mod,
      iter    = 20000,
      samples = 500,
      seed    = 123,
      verbose = FALSE
    )
    
    ext <- MVSE::extract(fit)
    
    indexP_df  <- ext$indexP
    indexP_num <- as.matrix(indexP_df[, -1])
    storage.mode(indexP_num) <- "numeric"
    

    indexP_num_vc <- indexP_num * vc_value
    

    indexP_med  <- apply(indexP_num_vc, 1, median, na.rm = TRUE)
    indexP_p025 <- apply(indexP_num_vc, 1, quantile, 0.025, na.rm = TRUE)
    indexP_p975 <- apply(indexP_num_vc, 1, quantile, 0.975, na.rm = TRUE)
    

    if (SAVE_POSTERIOR_DRAWS) {
      
      draw_wide <- as.data.frame(indexP_num_vc)
      names(draw_wide) <- paste0("draw_", seq_len(ncol(draw_wide)))
      
      draw_wide <- dplyr::bind_cols(
        tibble::tibble(
          date    = as.Date(indexP_df[[1]]),
          site_id = site,
          virus   = toupper(virus_name)
        ),
        draw_wide
      )
      
      safe_site <- gsub("[^A-Za-z0-9_]", "_", site)
      
      saveRDS(
        draw_wide,
        file.path(
          draw_dir,
          paste0(
            "posterior_draws_",
            toupper(virus_name),
            "_",
            safe_site,
            ".rds"
          )
        )
      )
    }
    

     tibble(
      date        = as.Date(indexP_df[[1]]),
      site_id     = site,
      indexP_med  = indexP_med,
      indexP_p025 = indexP_p025,
      indexP_p975 = indexP_p975
    )
    
  }, error = function(e){
    
    message("Failed locality: ", site, " | ", conditionMessage(e))
    
    tibble(
      date        = as.Date(NA),
      site_id     = site,
      indexP_med  = NA_real_,
      indexP_p025 = NA_real_,
      indexP_p975 = NA_real_
    )
  })
}

#===============================================================================
# 6) Run MVSE models for all localities for dengue and chikungunya
#===============================================================================
sites <- sort(unique(climate_monthly$site_id))

for (virus_name in c("denv", "chik")) {
  
  message("Running ", virus_name)
  
  priors_albo <- make_priors(priors_tbl, virus_name)
  
  vc_value <- priors_tbl %>%
    dplyr::filter(
      .data$virus == .env$virus_name,
      .data$param == "vector_competence"
    ) %>%
    dplyr::pull(mean)
  
  if (length(vc_value) != 1) {
    stop("VC selection failed for ", virus_name,
         ". Pulled values: ", paste(vc_value, collapse = ", "))
  }
  

  with_progress({
    
    p <- progressor(along = sites)
    
    results_all <- future_map_dfr(
      sites,
      function(s){
        p(sprintf("Running %s", s))
        run_mvse_one(
          site = s,
          data = climate_monthly,
          priors_albo = priors_albo,
          vc_value = vc_value,
          virus_name = virus_name
        )
      },
      .options = furrr_options(seed = TRUE)
    )
    
  })
 
# ==============================================================================
# 7) Save output
# ============================================================================== 
    write_csv(
    results_all,
    paste0("MVSE_indexP_", virus_name, "_monthly_FINAL.csv")
  )
  
}