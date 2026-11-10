# DENV VC = 0.354; CHIK VC = 0.625
# Compare Index P with and without VC

library(dplyr)
library(ggplot2)

# Load CSVs: DENV first, CHIK second
denv <- read.csv(file.choose()) |>
  mutate(date = as.Date(date), virus = "DENV", VC = 0.354)

chik <- read.csv(file.choose()) |>
  mutate(date = as.Date(date), virus = "CHIK", VC = 0.625)

# National monthly median Index P
monthly <- bind_rows(denv, chik) |>
  group_by(virus, date, VC) |>
  summarise(with_VC = median(indexP_med, na.rm = TRUE),
            .groups = "drop") |>
  mutate(without_VC = with_VC / VC)

# Compare median values across study months
summary <- monthly |>
  group_by(virus) |>
  summarise(
    with_VC = median(with_VC),
    without_VC = median(without_VC),
    .groups = "drop"
  )

print(summary)

# CHIK:DENV ratio with and without VC
ratio <- data.frame(
  setting = c("With VC", "Without VC"),
  CHIK_DENV_ratio = c(
    summary$with_VC[summary$virus == "CHIK"] /
      summary$with_VC[summary$virus == "DENV"],
    summary$without_VC[summary$virus == "CHIK"] /
      summary$without_VC[summary$virus == "DENV"]
  )
)

print(ratio)

write.csv(monthly, "IndexP_VC_monthly_comparison.csv", row.names = FALSE)
write.csv(summary, "IndexP_VC_summary.csv", row.names = FALSE)
write.csv(ratio, "IndexP_VC_ratio.csv", row.names = FALSE)

# Plot national Index P with and without VC
plot_data <- bind_rows(
  monthly |> transmute(date, virus, setting = "With VC",
                       IndexP = with_VC),
  monthly |> transmute(date, virus, setting = "Without VC",
                       IndexP = without_VC)
)

p <- ggplot(plot_data, aes(date, IndexP, colour = virus)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ setting, ncol = 1) +
  scale_colour_manual(values = c(DENV = "steelblue",
                                 CHIK = "firebrick")) +
  labs(x = "Year", y = "National median Index P",
       colour = "Virus") +
  theme_classic()

print(p)

ggsave("IndexP_with_without_VC.png", p,
       width = 10, height = 6, dpi = 300)
