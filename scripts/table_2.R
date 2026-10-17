# Table 2: transition goals achieved at the start of group 3


# 1 Library loading -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(here)
library(writexl)

# paths relative to the repository root (script in scripts/, data in data/)
raw_dir <- here("data", "raw")
processed_dir <- here("data", "processed")
output_dir <- here("output")
dir.create(output_dir, showWarnings = FALSE)


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

## Goal scores per pupil and goal ----

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
  ) |>
  left_join(goals_clean, by = join_by(goal_code), relationship = "many-to-one") |>
  # achieved = last score 3 or higher
  mutate(achieved = last_score >= 3)


## Percentage achieved per pupil ----

# per pupil and goal type, and over all goals
pupil_pct <- bind_rows(
  pupil_goals |>
    summarise(n_observed = n(), pct_achieved = 100 * mean(achieved),
              .by = c(pupil_name, goal_type)),
  pupil_goals |>
    summarise(n_observed = n(), pct_achieved = 100 * mean(achieved),
              .by = pupil_name) |>
    mutate(goal_type = "all")
)


# 4 Tables --------------------------------------------------------------------

## Table 2a: summary per goal type ----

# number of goals per type in the goals list
n_goals <- bind_rows(
  count(goals_clean, goal_type, name = "n_goals"),
  tibble(goal_type = "all", n_goals = nrow(goals_clean))
)

table_2a <- pupil_pct |>
  summarise(
    n_pupils = n(),
    mean_observed = mean(n_observed),
    mean_pct = mean(pct_achieved),
    sd_pct = sd(pct_achieved),
    min_pct = min(pct_achieved),
    max_pct = max(pct_achieved),
    .by = goal_type
  ) |>
  left_join(n_goals, by = join_by(goal_type)) |>
  mutate(goal_type = factor(goal_type, levels = c("basic", "expected", "all"),
                            labels = c("Basic goals", "Expected goals", "All goals"))) |>
  arrange(goal_type) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  select(
    `Goal type` = goal_type,
    `Number of goals` = n_goals,
    `N pupils` = n_pupils,
    `Goals observed per pupil (M)` = mean_observed,
    `% achieved (M)` = mean_pct,
    `% achieved (SD)` = sd_pct,
    `% achieved (min)` = min_pct,
    `% achieved (max)` = max_pct
  )

## Table 2b: results per goal ----

table_2b <- goals_clean |>
  left_join(
    pupil_goals |>
      summarise(
        n_pupils = n(),
        pct_achieved = 100 * mean(achieved),
        mean_score = mean(last_score),
        .by = goal_code
      ),
    by = join_by(goal_code)
  ) |>
  # goals without observations get 0 pupils
  mutate(n_pupils = replace_na(n_pupils, 0L)) |>
  # lowest % achieved first within each goal type
  arrange(goal_type, pct_achieved) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  select(
    `Goal type` = goal_type,
    `Goal number` = goal_number,
    `Goal code` = goal_code,
    Goal = goal,
    `N pupils observed` = n_pupils,
    `% achieved` = pct_achieved,
    `Mean last score (0-4)` = mean_score
  )


# 5 Export --------------------------------------------------------------------

# both tables in one Excel file
write_xlsx(
  list(table_2a_summary = table_2a, table_2b_goals = table_2b),
  file.path(output_dir, "table_2.xlsx")
)

