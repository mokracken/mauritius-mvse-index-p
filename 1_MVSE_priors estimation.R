# ==============================================================================
# Script 1 — MVSE PRIOR DERIVATION

# Title: Divergent climate suitability profiles for dengue and chikungunya 
# transmission by Aedes albopictus in Mauritius

# Purpose:
# Derives climate-informed MVSE priors for dengue and chikungunya
# transmission by Aedes albopictus in Mauritius.

# Adapted from:
#   Nakase GitHub MVSE tutorial/workflow, modified for Aedes albopictus,
#   dengue/chikungunya, and Mauritius locality-level climate data.
# Nakase, T. (2023). https://github.com/TaishiNakase/Index-P-estimation-and-applications

# Input:
#   Daily locality-level climate data containing:
#   date, site_id, tmean, and tdew

# Output:
#   MVSE_priors_from_climate.csv

# Author: Amar Teeluck, Benn Sartorius, Oyelola Adegboye, Stephan Karl, Emma McBryde, 
# *Eloise Skinner

# Institution: University of Queensland
# Date: 06/03/2026
# R version: 4.4.1
# ==============================================================================
library(readr)
library(dplyr)
library(stringr)

# ==============================================================================
# 1) Prepare climate data using 
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

# Calculate relative humidity using Tetens approximation
sat_vap <- function(T) 6.112 * exp((17.67 * T) / (T + 243.5))

climate_raw <- climate_raw %>%
  mutate(
    RH = 100 * (sat_vap(tdew) / sat_vap(tmean))
  )

mean_sd <- function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE))
}

# ==============================================================================
# 2) Deterministic temperature-dependent trait functions for Aedes albopictus
# ==============================================================================
# Ae. albopictus biting rate (Brière) - Mordecai et al., 2017
BITE_ALBO <- function(T){
  c <- 1.93e-4
  Tmin <- 10.25
  Tmax <- 38.32
  bite <- c * T * (T - Tmin) * sqrt(pmax(Tmax - T, 0))
  bite[T <= Tmin | T >= Tmax] <- 0
  pmax(bite, 0)
}

# Ae. albopictus EIP - DENV (Brière) - Mordecai et al., 2017
EIP_ALBO_DENV <- function(T){
  c <- 1.09e-4
  Tmin <- 10.39
  Tmax <- 43.05
  pdr <- c * T * (T - Tmin) * sqrt(pmax(Tmax - T, 0))
  pdr[T <= Tmin | T >= Tmax] <- NA
  pmin(pmax(1 / pdr, 1), 60)
}

# Ae. albopictus EIP - CHIK - Tegar et al., 2026 - Reunion Island
# Bound EIP between 1 and 60 days to avoid extreme values
# at marginal temperatures where pathogen development is very slow.
EIP_ALBO_CHIK <- function(T){
  q <- 0.00032
  Tmin <- 9.62
  Tmax <- 36.23
  pdr <- q * T * (T - Tmin) * sqrt(pmax(Tmax - T, 0))
  pdr[T <= Tmin | T >= Tmax] <- NA
  pmin(pmax(1 / pdr, 1), 60)
}

# Adult Ae. albopictus lifespan (Lacroix et al., 2009 - Reunion Island)
mean_ale <- 17.71
sd_ale   <- 4.39

# ==============================================================================
# 4) Derive Temperature-dependent Aedes albopictus priors from climate
# ==============================================================================
bite_vals <- BITE_ALBO(climate_raw$tmean)
eip_denv  <- EIP_ALBO_DENV(climate_raw$tmean)
eip_chik  <- EIP_ALBO_CHIK(climate_raw$tmean)

bite_stats <- mean_sd(bite_vals)
eip_denv_stats <- mean_sd(eip_denv)
eip_chik_stats <- mean_sd(eip_chik)

# ==============================================================================
# 5) Build MVSE Prior Table for use in Script 2
# ==============================================================================
priors_out <- bind_rows(
  tibble(
    virus = "denv",
    param = c("mosq_life_exp", "mosq_biting_freq", "mosq_inc_per",
              "human_life_exp", "human_inc_per", "human_inf_per", "vector_competence"),
    mean  = c(mean_ale, bite_stats["mean"], eip_denv_stats["mean"],
              75, 5.9, 4.0, 0.354),
    sd    = c(sd_ale, bite_stats["sd"], eip_denv_stats["sd"],
              2, 1.79, 0.51, NA_real_),
    dist  = c("normal", "normal", "normal", "normal", "gamma", "normal", "fixed")
  ),
  tibble(
    virus = "chik",
    param = c("mosq_life_exp", "mosq_biting_freq", "mosq_inc_per",
              "human_life_exp", "human_inc_per", "human_inf_per", "vector_competence"),
    mean  = c(mean_ale, bite_stats["mean"], eip_chik_stats["mean"],
              75, 3.0, 5.0, 0.625),
    sd    = c(sd_ale, bite_stats["sd"], eip_chik_stats["sd"],
              2, 1.3, 1.0, NA_real_),
    dist  = c("normal", "normal", "lognormal", "normal", "lognormal", "normal", "fixed")
  )
)

# ==============================================================================
# 8) Save output
# ==============================================================================
write_csv(priors_out, "MVSE_priors_from_climate.csv")