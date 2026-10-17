# Visualizations: transition goals


# 1 Library loading -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(here)

# paths relative to the repository root
raw_dir <- here("data", "raw")
processed_dir <- here("data", "processed")
graphs_dir <- here("graphs")
dir.create(graphs_dir, showWarnings = FALSE)

# main color (can be set to something better later)
main_color <- "#C2410C"


# 2 Data loading --------------------------------------------------------------

# final dataset (one row per pupil)
final_dataset <- read_excel(file.path(processed_dir, "final_dataset.xlsx"),
  sheet = "final_dataset")

# transition goals list (title rows above the headers)
goals_basic_raw <- read_excel(file.path(raw_dir, "Selected learning goals.xlsx"),
  sheet = "Basic Goals - First Two Years", skip = 2)
goals_expected_raw <- read_excel(file.path(raw_dir, "Selected learning goals.xlsx"),
  sheet = "Goals Before Grade One", skip = 2)


# 3 Data preparation ----------------------------------------------------------

## Goals list ----

# stack both sheets, goal type kept from the sheet
goals_clean <- bind_rows(basic = goals_basic_raw,
                         expected = goals_expected_raw,
                         .id = "goal_type") |>
  rename(
    goal_number = No.,
    goal = `Learning goal`,
    goal_code = `Code leerdoelentool`
  ) |>
  mutate(goal_code = str_squish(goal_code))

## Results per goal ----

# one row per pupil and observed goal (unobserved goals left out)
pupil_goals <- final_dataset |>
  select(pupil_name, starts_with("goal_")) |>
  pivot_longer(
    -pupil_name,
    names_to = "goal_code",
    names_prefix = "goal_",
    values_to = "last_score",
    values_transform = as.numeric,
    values_drop_na = TRUE
  )

# pupils mastering (last score 3 or higher) and mean score per goal
goal_summary <- pupil_goals |>
  summarise(
    n_observed = n(),
    n_mastered = sum(last_score >= 3),
    pct_mastered = 100 * n_mastered / n_observed,
    mean_score = mean(last_score),
    .by = goal_code
  ) |>
  # goals without observations are not in the charts
  inner_join(goals_clean, by = join_by(goal_code)) |>
  mutate(goal_type = factor(goal_type, levels = c("basic", "expected"),
                            labels = c("Basic goals", "Expected goals")))


# 4 Custom functions ----------------------------------------------------------

# bar chart of % pupils mastering each goal, for one goal type
plot_mastered <- function(data, type) {
  data |>
    filter(goal_type == type) |>
    ggplot(aes(x = pct_mastered,
               y = fct_reorder(goal_code, pct_mastered),
               fill = pct_mastered)) +
    geom_col(width = 0.6) +
    scale_fill_gradientn(
      colors = mastery_colors,
      values = scales::rescale(c(50, 80, 100)),
      limits = c(50, 100),
      oob = scales::squish,
      breaks = c(50, 80, 100),
      labels = c("50% or lower", "80%", "100%"),
      name = "Pupils mastering"
    ) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                       labels = \(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                 barwidth = unit(8, "cm"),
                                 barheight = unit(0.35, "cm"))) +
    labs(
      title = type,
      subtitle = str_c(
        "Percentage of pupils mastering each learning goal by end of kindergarten\n",
        "Mastered = last observed score of 3 or higher in groups 1-2"
      ),
      x = "Pupils mastering the goal (% of observed pupils)",
      y = NULL
    ) +
    theme_goals
}


# 5 Visualizations ------------------------------------------------------------

## Shared theme ----

# minimal theme with extra white space around all elements
theme_goals <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15,
                              margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey35", lineheight = 1.3,
                                 margin = margin(b = 20)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "grey20", size = 10,
                               margin = margin(r = 8)),
    axis.text.x = element_text(color = "grey35", margin = margin(t = 6)),
    axis.title.x = element_text(color = "grey20", margin = margin(t = 14)),
    strip.text = element_text(face = "bold", hjust = 0),
    strip.placement = "outside",
    legend.position = "bottom",
    legend.title = element_text(color = "grey20", size = 10,
                                margin = margin(b = 6)),
    legend.text = element_text(color = "grey35", size = 9),
    legend.margin = margin(t = 12),
    plot.margin = margin(30, 35, 30, 30),
    plot.background = element_rect(fill = "white", color = NA)
  )

## Percentage of pupils mastering each goal ----

# gradient: dark orange (50% or lower) -> yellow (80%) -> green (100%)
mastery_colors <- c("#C2410C", "#EAB308", "#15803D")

# basic goals
plot_mastered_basic <- plot_mastered(goal_summary, "Basic goals")
plot_mastered_basic

ggsave(file.path(graphs_dir, "goals_pupils_mastering_basic.jpg"),
       plot_mastered_basic, width = 9, height = 9, dpi = 300)

# expected goals
plot_mastered_expected <- plot_mastered(goal_summary, "Expected goals")
plot_mastered_expected

ggsave(file.path(graphs_dir, "goals_pupils_mastering_expected.jpg"),
       plot_mastered_expected, width = 9, height = 7.5, dpi = 300)

## Average score per goal ----

plot_mean <- goal_summary |>
  ggplot(aes(x = mean_score,
             y = fct_reorder(goal_code, mean_score))) +
  geom_col(fill = main_color, width = 0.7) +
  # mastery level
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey40") +
  # goal type as a centred heading above each block of bars
  facet_wrap(~ goal_type, ncol = 1, scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 4), breaks = 0:4,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Average score reached by end of kindergarten",
    x = "Average score",
    y = NULL
  ) +
  theme_goals +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 20)),
    strip.text = element_text(face = "bold", size = 12, hjust = 0.5,
                              margin = margin(t = 12, b = 8))
  )

plot_mean

ggsave(file.path(graphs_dir, "goals_average_score.jpg"), plot_mean,
       width = 8, height = 9, dpi = 300)
