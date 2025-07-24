library(dplyr)
library(tidyr)
library(ggplot2)

events <- tribble(
  ~person_id, ~event_date, ~event_type,
  1,   0, "prostate cancer",
  2,   0, "prostate cancer",
  3,   0, "prostate cancer",
  4,   0, "prostate cancer",
  5,   0, "prostate cancer",
  6,   0, "prostate cancer",
  1, 400, "radiotheraphy",
  4, 170, "radiotheraphy",
  3,  75, "prostatectomy",
  4, 300, "prostatectomy",
  5,  90, "prostatectomy",
  2, 150, "outcome",
  5, 390, "outcome",
  6, 500, "outcome",
  3, 200, "death/end observation"
)

sep <- 1
x_max <- 500
y_max <- 25
n_ind <- length(unique(events$person_id))
col_surveillance     <- "#1b9e77"  # Cool teal green
col_radiotherapy     <- "#d95f02"  # Warm orange
col_prostatectomy    <- "#7570b3"  # Muted purple
col_prostate_cancer  <- "#e7298a"  # Vibrant magenta (same as now — works well)
col_outcome          <- "#66a61e"  # Olive green (adjusted to avoid duplication with others)
col_censor <- "#000"

events_pivot <- events |>
  mutate(
    event_type = str_replace(event_type, "/", "_") |>
      str_replace(" ", "_")
  ) |>
  pivot_wider(names_from = "event_type", values_from = "event_date") |>
  mutate(death_end_observation = coalesce(death_end_observation, x_max))

followup_surveillance <- events_pivot |>
  mutate(end = pmin(radiotheraphy, prostatectomy, outcome, death_end_observation, na.rm = TRUE)) |>
  select("person_id", start = "prostate_cancer", "end")

followup_radiotheraphy <- events_pivot |>
  mutate(
    end = pmin(prostatectomy, outcome, death_end_observation, na.rm = TRUE),
    end = if_else((is.na(radiotheraphy) | radiotheraphy > 365) & end > 365, 365, end)
  ) |>
  select("person_id", start = "prostate_cancer", "end")

followup_prostatectomy <- events_pivot |>
  mutate(
    end = pmin(radiotheraphy, outcome, death_end_observation, na.rm = TRUE),
    end = if_else((is.na(prostatectomy) | prostatectomy > 365) & end > 365, 365, end)
  ) |>
  select("person_id", start = "prostate_cancer", "end")

followup <- followup_surveillance |>
  mutate(
    group = "surveillance",
    y = 2 * sep + 2 * n_ind + person_id
  ) |>
  union_all(
    followup_radiotheraphy |>
      mutate(
        group = "radiotheraphy",
        y = sep + n_ind + person_id
      )
  ) |>
  union_all(
    followup_prostatectomy |>
      mutate(
        group = "prostatectomy",
        y = person_id
      )
  ) |>
  mutate(group_id = row_number()) |>
  pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")

events_plot <- events |>
  inner_join(
    followup |>
      group_by(person_id, y) |>
      summarise(end = max(x), .groups = "drop"),
    by = "person_id",
    relationship = "many-to-many"
  ) |>
  mutate(alpha = if_else(event_date > end, 0.5, 1)) |>
  filter(event_type != "prostate cancer")

y_m <- max(followup$y) + sep

p <- ggplot(data = followup) +
  geom_line(
    mapping = aes(x = x, y = y, group = group_id, colour = group),
    linewidth = 1
  ) +
  geom_point(
    mapping = aes(x = event_date, y = y, colour = event_type, alpha = alpha),
    data = events_plot,
    size = 2.5
  ) +
  geom_text(
    mapping = aes(x = -10, y = y, label = person_id),
    family = "Graphik",
    size = 4
  ) +
  geom_line(
    mapping = aes(x = x, y = y),
    data = tibble(x = 365, y = c(0, y_m)),
    inherit.aes = FALSE,
    linetype = "dashed",
    colour = "gray50"
  ) +
  # legend
  geom_text(
    mapping = aes(x = x, y = y, label = text),
    data = tibble(
      x = c(100, 195, 295, 363) - 40,
      y = 22,
      text = c("radiotheraphy", "prostatectomy", "outcome", "death/end observation")
    ),
    size = 4,
    family = "Graphik",
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.35,
    nudge_x = 8
  ) +
  geom_point(
    mapping = aes(x = x, y = y, colour = col),
    data = tibble(
      x = c(100, 195, 295, 363) - 40,
      y = 22,
      col = c("radiotheraphy", "prostatectomy", "outcome", "death/end observation")
    ),
    size = 2.5,
    inherit.aes = FALSE
  ) +
  # legend 2
  geom_text(
    mapping = aes(x = x, y = y, label = text),
    data = tibble(
      x = c(100, 200, 310) + 20,
      y = 24.5,
      text = c("surveillance", "radiotheraphy", "prostatectomy")
    ),
    size = 4,
    family = "Graphik",
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.35,
    nudge_x = 4
  ) +
  geom_line(
    mapping = aes(x = x, y = y, colour = col),
    data = tibble(
      x_s = c(100, 200, 310) + 20,
      x_e = x_s - 20,
      y = 24.5,
      col = c("surveillance", "radiotheraphy", "prostatectomy")
    ) |>
      pivot_longer(c("x_s", "x_e"), names_to = NULL, values_to = "x"),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c(
    surveillance = col_surveillance, prostatectomy = col_prostatectomy,
    radiotheraphy = col_radiotherapy, "prostate cancer" = col_prostate_cancer,
    outcome = col_outcome, "death/end observation" = col_censor
  )) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500)) +
  scale_alpha_identity(name = "Transparency") +
  labs(x = "Time (days)") +
  coord_cartesian(xlim = c(0, x_max), ylim = c(1, y_max), clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x.bottom = element_line(linewidth = 0.5),
    axis.text.x = element_text(family = "Graphik", size = 10),
    axis.title.x = element_text(family = "Graphik", size = 10)
  )

ggsave(
  filename = here("CloneCensorWeight", "figures", "diagram.png"),
  plot = p,
  width = 748*3,
  height = 468*3,
  units = "px",
  dpi = 300
)
