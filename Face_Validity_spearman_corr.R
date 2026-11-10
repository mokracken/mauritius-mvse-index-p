
# Face-validity analysis
# Spearman correlation: national median Index P vs dengue cases
# Period: January 2024 to October 2024
# Lags 0–4 months


library(dplyr)
library(ggplot2)

# Load and filter January–October 2024
indexp <- read.csv(file.choose()) |>
  mutate(date = as.Date(date)) |>
  filter(date >= "2024-01-01", date <= "2024-10-01")

cases <- read.csv(file.choose()) |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) |>
  filter(date >= "2024-01-01", date <= "2024-10-01",
         case_type == "reported_cases") |>
  group_by(date) |>
  summarise(cases = sum(reported_cases, na.rm = TRUE), .groups = "drop")

# National median Index P and monthly cases
dat <- indexp |>
  group_by(date) |>
  summarise(indexp_med = median(indexP_med, na.rm = TRUE),
            .groups = "drop") |>
  inner_join(cases, by = "date") |>
  arrange(date)

if (nrow(dat) != 10 || any(diff(dat$date) > 31))
  stop("Check missing months in January–October 2024.")

# Spearman correlations, lags 0–4 months
res <- bind_rows(lapply(0:4, function(k) {
  d <- dat |>
    mutate(cases_lead = lead(cases, k)) |>
    filter(!is.na(indexp_med), !is.na(cases_lead))
  
  ct <- if (nrow(d) >= 3 && n_distinct(d$indexp_med) > 1 &&
            n_distinct(d$cases_lead) > 1)
    cor.test(d$indexp_med, d$cases_lead,
             method = "spearman", exact = FALSE) else NULL
  
  data.frame(period = "Jan-Oct 2024", lag_months = k,
             rho = if (is.null(ct)) NA_real_ else unname(ct$estimate),
             p = if (is.null(ct)) NA_real_ else ct$p.value,
             n = nrow(d))
}))

print(res)
write.csv(res, "Reviewer3_Spearman_2024.csv", row.names = FALSE)

# Time-series overlay
sf <- max(dat$cases, na.rm = TRUE) / max(dat$indexp_med, na.rm = TRUE)

p <- ggplot(dat, aes(date)) +
  geom_col(aes(y = cases / sf), fill = "grey75") +
  geom_line(aes(y = indexp_med), colour = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous("National median Index P (DENV)",
                     sec.axis = sec_axis(~ . * sf, name = "Monthly dengue cases")) +
  labs(x = NULL) + theme_classic()

print(p)
ggsave("Reviewer3_IndexP_vs_Cases_2024.png",
       p, width = 8, height = 4, dpi = 300)
