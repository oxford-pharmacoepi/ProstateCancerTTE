
library(omopgenerics)
library(visOmopResults)
library(ggplot2)
library(scales)
library(dplyr)
library(CohortCharacteristics)
library(tidyr)

result <- importSummarisedResult("data")

attrition <- result |>
  filterSettings(result_type == "summarise_cohort_attrition") |>
  filterGroup(cohort_name == "prostate_cancer") |>
  tidy() |>
  pivot_wider(names_from = "variable_name", values_from = "count") |>
  select("reason", "number_records", "number_subjects", "excluded_records", "excluded_subjects")

followUp <- result |>
  filterSettings(result_type == "follow_up_reason_time") |>
  tidy() |>
  mutate(
    time = as.integer(variable_level) / 365,
    variable_name = case_when(
      variable_name == "death" ~ "censor",
      variable_name == "future_observation" ~ "censor",
      variable_name %in% c("end_surveillance", "no prostatectomy", "no radiotheraphy") ~ "no_adherence",
      .default = variable_name
    )
  ) |>
  group_by(cohort_name, variable_name, time) |>
  summarise(percentage = sum(percentage), .groups = "drop") |>
  mutate(
    variable_name = factor(
      variable_name,
      levels = c("in_observation", "no_adherence", "radiotheraphy", "prostatectomy", "censor")
    )
  )

pal <- c(
  "in_observation" = "#C84C24",
  "no_adherence"   = "#355C7D",
  "radiotheraphy"   = "#5C86B5",
  "prostatectomy"  = "#8FB9E3",
  "censor"         = "#203864"
)

ggplot(followUp, aes(x = time, y = percentage, fill = variable_name, group = variable_name)) +
  geom_area( na.rm = TRUE) +
  facet_wrap(~cohort_name, ncol = 3) +
  scale_fill_manual(values = pal, drop = FALSE) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 100), expand = FALSE) +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(breaks = seq(0, 100, 25), labels = label_number(suffix = "%")) +
  labs(
    x = "Time since baseline (years)",
    y = "Percentage",
    fill = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey92", colour = NA),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "grey20"),
    legend.position = "top",
    legend.box = "horizontal"
  )

Ref <- "radiotheraphy"

nco <- result |>
  filterSettings(result_type == "cox_regression") |>
  tidy() |>
  filter(reference == Ref) |>
  select("weight_type", "exposed", "hr", "se_coef")

plot_funnel_calibration <- function(data,
                                    x = hr,
                                    y = se_coef,
                                    row_facet = exposed,
                                    col_facet = weight_type,
                                    alpha = 0.05,
                                    xlim = c(0.1, 10),
                                    ylim = c(0, 1.5)) {
  # Tidy data
  df <- data |>
    mutate(
      rr = {{ x }},
      se = {{ y }},
      row_facet = {{ row_facet }},
      col_facet = {{ col_facet }}
    ) |>
    filter(is.finite(rr), is.finite(se), rr > 0, se >= 0) |>
    mutate(
      lcl = exp(log(rr) - qnorm(1 - alpha / 2) * se),
      ucl = exp(log(rr) + qnorm(1 - alpha / 2) * se),
      ci_includes_1 = lcl <= 1 & ucl >= 1
    )

  # Calibration boundary
  zone <- tibble(
    rr = exp(seq(log(xlim[1]), log(xlim[2]), length.out = 500))
  ) |>
    mutate(
      boundary = abs(log(rr)) / qnorm(1 - alpha / 2)
    )

  # Facet-level annotations
  ann <- df |>
    group_by(row_facet, col_facet) |>
    summarise(
      n_estimates = n(),
      pct_ci_includes_1 = mean(ci_includes_1, na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    mutate(
      label = sprintf(
        "%d estimates\n%.1f%% of CIs include 1",
        n_estimates, pct_ci_includes_1
      ),
      x = xlim[1] * 1.25,
      y = ylim[2] * 0.95
    )

  ggplot(df, aes(x = rr, y = se)) +
    # calibration zone (significant region)
    geom_ribbon(
      data = zone,
      aes(x = rr, ymin = 0, ymax = boundary),
      inherit.aes = FALSE,
      fill = "#F4B266",
      alpha = 0.75
    ) +
    geom_line(
      data = zone,
      aes(x = rr, y = boundary),
      inherit.aes = FALSE,
      color = "#E67E22",
      linewidth = 1.05
    ) +
    geom_vline(xintercept = 1, color = "grey55", linewidth = 0.4) +
    geom_point(color = "#2B3A67", alpha = 0.65, size = 1.9) +
    geom_label(
      data = ann,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 3.4,
      label.size = 0.25,
      fill = scales::alpha("white", 0.92),
      color = "black"
    ) +
    facet_grid(rows = vars(row_facet), cols = vars(col_facet)) +
    scale_x_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10),
      labels = c("0.1", "0.25", "0.5", "1", "2", "4", "6", "8", "10"),
      limits = xlim
    ) +
    coord_cartesian(ylim = ylim, expand = FALSE) +
    labs(
      x = "Hazard ratio",
      y = "Standard error"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey20"),
      legend.position = "none"
    )
}

ggplot(data = nco, mapping = aes(x = hr, y = se_coef)) +
  geom_point() +
  facet_grid(exposed ~ weight_type) +
  coord_cartesian(xlim = c(0.1, 10), ylim = c(0, 1.5)) +
  scale_x_log10()

plot_funnel_calibration(nco)
