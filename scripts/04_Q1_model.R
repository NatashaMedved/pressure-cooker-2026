# 04_Q1_model_.R
# Analysis for Question 1
# Do pupils who stayed longer in kindergarten have better Cito results at the
# end of group 3, and do they need less extra support?
#
# Main comparison: Stayed longer vs Standard track
#   - Each pupil is compared with peers born in the same month.
#   - Stayed longer = more than 6 months longer in kindergarten than those peers
#     (a 3-month cut-off gives exactly the same groups).
#   - Pupils who moved up early (more than 6 months shorter) are left out:
#     they are not part of the school's question.
# Extra check: months in kindergarten as a number (continuous)
# Controls: cohort (group 3 year), starting level, kindergarten class
#
# No control for age at the start of group 3: age and months in kindergarten are
# almost the same thing here (all pupils started school at age 4).
# NOTE: results are associations, not proven effects (teachers decide who stays longer),
#       and the "stayed longer" group is small (6 pupils with Cito).

# 1 Setup ---------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(writexl)
library(broom)
library(sandwich)
library(lmtest)

processed_dir <- here("data/processed")
output_dir    <- here("output/question_1")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

main_color <- "#C2410C"

# p-values in APA style: ".034" and "<.001"
apa_p <- \(p) str_replace(scales::pvalue(p, accuracy = 0.001), "0\\.", ".")


# 2 Data ----------------------------------------------------------------------

final_dataset <- read_excel(file.path(processed_dir, "final_dataset.xlsx"),
                            sheet = "final_dataset") |>
  mutate(across(c(date_of_birth, start_date, group3_start), as_date))


# 3 Data prep -----------------------------------------------------------------

## Kindergarten track ----
# Step 1: use ALL pupils with months in kindergarten (104), so the "typical"
#         months per birth month is the same as in the summary tables.
# Step 2: keep only pupils with at least one Cito score at the end of group 3.
# Step 3: leave out pupils who moved up early.

analysis_data <- final_dataset |>
  filter(!is.na(months_kindergarten)) |>
  mutate(birth_month = month(date_of_birth)) |>
  mutate(typical_months = median(months_kindergarten), .by = birth_month) |>
  mutate(
    diff_from_peers = months_kindergarten - typical_months,
    track = case_when(
      diff_from_peers >  6 ~ "Stayed longer",
      diff_from_peers < -6 ~ "Moved up early",
      TRUE                 ~ "Standard track"
    ),
    cohort = factor(group3_year)
  ) |>
  filter(!is.na(mathematics_g3_end_score) | !is.na(tmt_g3_end_score) |
           !is.na(spelling_non_verbs_g3_end_score))

# how many pupils are left out because they moved up early?
count(analysis_data, track)

analysis_data <- analysis_data |>
  filter(track != "Moved up early") |>
  # Standard track first = reference group, so results read
  # "stayed longer compared with standard track"
  mutate(track = factor(track, levels = c("Standard track", "Stayed longer")))

## Extra support in the group 3 year ----
# Source: data/raw/Support interventions_care team.xlsx
# - only interventions that actually started ("Started?" == "Yes")
# - only in the school year the pupil was in group 3
# - "learning support" = Need is Learning, Language or Speech & language
#   (so motor therapy, behaviour etc. don't count)
# The file only covers school years 2023/24 - 2025/26, so pupils who were in
# group 3 before 2023 get NA (unknown), not "no support".

support_raw <- read_excel(here("data/raw/Support interventions_care team.xlsx"),
                          skip = 1)            # first row is not the header

support_rows <- support_raw |>
  filter(`Started?` == "Yes", !is.na(`School yr`)) |>
  transmute(
    name_key      = str_to_lower(str_squish(`Student name`)),   # same name key as Alice
    school_year   = 2000 + as.integer(str_sub(as.character(`School yr`), 1, 2)),  # 2324 -> 2023
    learning_need = Need %in% c("Learning", "Language", "Speech & language")
  )

support_g3 <- analysis_data |>
  transmute(pupil_id, group3_year,
            name_key = str_to_lower(str_squish(pupil_name))) |>
  left_join(support_rows, by = join_by(name_key), relationship = "one-to-many") |>
  summarise(
    support_g3_any      = any(school_year == group3_year, na.rm = TRUE),
    support_g3_learning = any(school_year == group3_year & learning_need, na.rm = TRUE),
    .by = c(pupil_id, group3_year)
  ) |>
  # no support data for group 3 years before 2023/24 -> unknown
  mutate(across(starts_with("support_g3"), \(x) if_else(group3_year >= 2023, x, NA))) |>
  select(-group3_year)

analysis_data <- analysis_data |>
  left_join(support_g3, by = join_by(pupil_id), relationship = "one-to-one")

## One dataset per test ----
# drop_na() keeps only pupils with all variables, so every model of one test
# uses exactly the same pupils

maths_data <- analysis_data |>
  select(score = mathematics_g3_end_score, track, months_kindergarten,
         cohort, starting_level, kindergarten_class) |>
  drop_na()

tmt_data <- analysis_data |>
  select(score = tmt_g3_end_score, track, months_kindergarten,
         cohort, starting_level, kindergarten_class) |>
  drop_na()

spelling_data <- analysis_data |>
  select(score = spelling_non_verbs_g3_end_score, track, months_kindergarten,
         cohort, starting_level, kindergarten_class) |>
  drop_na()


# 4 Descriptives --------------------------------------------------------------

count(analysis_data, cohort)
count(analysis_data, track)

# averages per track (the easiest table for the report)
track_summary <- analysis_data |>
  summarise(
    N                        = n(),
    `Months in kindergarten` = mean(months_kindergarten),
    `Starting level`         = mean(starting_level, na.rm = TRUE),
    Mathematics              = mean(mathematics_g3_end_score, na.rm = TRUE),
    TMT                      = mean(tmt_g3_end_score, na.rm = TRUE),
    `Spelling - Non-verbs`   = mean(spelling_non_verbs_g3_end_score, na.rm = TRUE),
    .by = track
  ) |>
  arrange(track) |>
  mutate(across(where(is.double), \(x) round(x, 1)))

track_summary


# 5 Models --------------------------------------------------------------------
# Per test:
#   model A: track + cohort
#   model B: + starting level and kindergarten class (fairer comparison)
#   months:  model B with months (number) instead of track (extra check)

## Mathematics ----
maths_a      <- lm(score ~ track + cohort, data = maths_data)
maths_b      <- lm(score ~ track + cohort + starting_level + kindergarten_class, data = maths_data)
maths_months <- lm(score ~ months_kindergarten + cohort + starting_level + kindergarten_class, data = maths_data)

## TMT ----
tmt_a      <- lm(score ~ track + cohort, data = tmt_data)
tmt_b      <- lm(score ~ track + cohort + starting_level + kindergarten_class, data = tmt_data)
tmt_months <- lm(score ~ months_kindergarten + cohort + starting_level + kindergarten_class, data = tmt_data)

## Spelling ----
spelling_a      <- lm(score ~ track + cohort, data = spelling_data)
spelling_b      <- lm(score ~ track + cohort + starting_level + kindergarten_class, data = spelling_data)
spelling_months <- lm(score ~ months_kindergarten + cohort + starting_level + kindergarten_class, data = spelling_data)

# look at one model in full
summary(maths_b)

## Checks on model B ----
# diagnostic plots (look for strange patterns or single extreme pupils)
par(mfrow = c(2, 2)); plot(maths_b);    par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(tmt_b);      par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(spelling_b); par(mfrow = c(1, 1))

# robust standard errors (HC3): safer with small, unequal groups
maths_b_hc3    <- coeftest(maths_b,    vcov = vcovHC(maths_b,    type = "HC3"))
tmt_b_hc3      <- coeftest(tmt_b,      vcov = vcovHC(tmt_b,      type = "HC3"))
spelling_b_hc3 <- coeftest(spelling_b, vcov = vcovHC(spelling_b, type = "HC3"))

## Extra support in group 3 (yes/no) ----
# Small groups, so a simple table + Fisher's exact test instead of a logistic regression
support_table <- analysis_data |>
  filter(!is.na(support_g3_any)) |>                 # only pupils with support data
  summarise(
    N                    = n(),
    `Any support`        = sum(support_g3_any),
    `% any support`      = round(100 * mean(support_g3_any), 1),
    `Learning support`   = sum(support_g3_learning),
    `% learning support` = round(100 * mean(support_g3_learning), 1),
    .by = track
  ) |>
  arrange(track)

support_table

# Fisher's exact test: stayed longer vs standard track, learning support
support_test <- analysis_data |>
  filter(!is.na(support_g3_learning)) |>
  with(fisher.test(table(track, support_g3_learning)))

support_test


# 6 Results tables ------------------------------------------------------------

## Main result: stayed longer vs standard track, in Cito points ----
results_track <- bind_rows(
  tidy(maths_a,    conf.int = TRUE) |> mutate(test = "Mathematics",          model = "A: + cohort",                n = nobs(maths_a)),
  tidy(maths_b,    conf.int = TRUE) |> mutate(test = "Mathematics",          model = "B: + starting level, class", n = nobs(maths_b)),
  tidy(tmt_a,      conf.int = TRUE) |> mutate(test = "TMT",                  model = "A: + cohort",                n = nobs(tmt_a)),
  tidy(tmt_b,      conf.int = TRUE) |> mutate(test = "TMT",                  model = "B: + starting level, class", n = nobs(tmt_b)),
  tidy(spelling_a, conf.int = TRUE) |> mutate(test = "Spelling - Non-verbs", model = "A: + cohort",                n = nobs(spelling_a)),
  tidy(spelling_b, conf.int = TRUE) |> mutate(test = "Spelling - Non-verbs", model = "B: + starting level, class", n = nobs(spelling_b))
) |>
  filter(term == "trackStayed longer") |>
  transmute(
    Test     = test,
    Model    = model,
    N        = n,
    `Difference (points)` = round(estimate, 1),
    `95% CI` = str_c("[", round(conf.low, 1), ", ", round(conf.high, 1), "]"),
    p        = apa_p(p.value)
  )

results_track

## Robustness: same model B, p-values with HC3 standard errors ----
results_hc3 <- bind_rows(
  tidy(maths_b_hc3)    |> mutate(Test = "Mathematics"),
  tidy(tmt_b_hc3)      |> mutate(Test = "TMT"),
  tidy(spelling_b_hc3) |> mutate(Test = "Spelling - Non-verbs")
) |>
  filter(term == "trackStayed longer") |>
  transmute(Test,
            `Difference (points)` = round(estimate, 1),
            `SE (HC3)` = round(std.error, 2),
            `p (HC3)`  = apa_p(p.value))

results_hc3

## Extra check: months in kindergarten as a number ----
# estimate = Cito points per extra month in kindergarten
results_months <- bind_rows(
  tidy(maths_months,    conf.int = TRUE) |> mutate(Test = "Mathematics",          N = nobs(maths_months)),
  tidy(tmt_months,      conf.int = TRUE) |> mutate(Test = "TMT",                  N = nobs(tmt_months)),
  tidy(spelling_months, conf.int = TRUE) |> mutate(Test = "Spelling - Non-verbs", N = nobs(spelling_months))
) |>
  filter(term == "months_kindergarten") |>
  transmute(Test, N,
            `Points per month` = round(estimate, 2),
            `95% CI` = str_c("[", round(conf.low, 2), ", ", round(conf.high, 2), "]"),
            p = apa_p(p.value))

results_months

## Extra support ----
results_support <- support_table |>
  mutate(`Fisher p (learning support)` =
           if_else(track == "Stayed longer", apa_p(support_test$p.value), NA))

results_support

## Export ----
write_xlsx(
  list(
    track_summary  = track_summary,
    track_results  = results_track,
    track_hc3      = results_hc3,
    months_check   = results_months,
    support_group3 = results_support
  ),
  file.path(output_dir, "analysis_q1_tables.xlsx")
)


# 7 Plots ---------------------------------------------------------------------

theme_analysis <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey35", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.margin = margin(30, 35, 30, 30),
    plot.background = element_rect(fill = "white", color = NA)
  )

# long format: one row per pupil per test
plot_data <- analysis_data |>
  select(track, months_kindergarten, late_born,
         Mathematics            = mathematics_g3_end_score,
         TMT                    = tmt_g3_end_score,
         `Spelling - Non-verbs` = spelling_non_verbs_g3_end_score) |>
  pivot_longer(c(Mathematics, TMT, `Spelling - Non-verbs`),
               names_to = "test", values_to = "score", values_drop_na = TRUE) |>
  mutate(test = factor(test, levels = c("Mathematics", "TMT", "Spelling - Non-verbs")))

## Main plot: Cito per track ----
plot_track <- ggplot(plot_data, aes(x = track, y = score)) +
  geom_jitter(width = 0.15, height = 0, size = 2.5, color = main_color, alpha = 0.7) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "grey30") +
  facet_wrap(~ test, scales = "free_y") +
  labs(title = "Cito at the end of group 3: stayed longer vs standard track",
       subtitle = "Each dot is one pupil. Bar = group average.",
       x = NULL, y = "Cito score (end of group 3)") +
  theme_analysis

plot_track
ggsave(file.path(output_dir, "analysis_q1_cito_by_track.jpg"),
       plot_track, width = 14, height = 6, dpi = 300)

## Extra plot: months in kindergarten (number) vs Cito ----
plot_months <- ggplot(plot_data, aes(x = months_kindergarten, y = score)) +
  geom_point(aes(color = as.logical(late_born)), size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8) +
  facet_wrap(~ test, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "grey65", "TRUE" = main_color),
                     labels = c("Other pupils", "Late-born (Oct-Dec)"), name = NULL) +
  labs(title = "Time in kindergarten and Cito at the end of group 3",
       subtitle = "Each dot is one pupil. Line = overall trend with 95% confidence band.",
       x = "Months in kindergarten", y = "Cito score (end of group 3)") +
  theme_analysis +
  theme(legend.position = "top")

plot_months
ggsave(file.path(output_dir, "analysis_q1_months_vs_cito.jpg"),
       plot_months, width = 14, height = 6, dpi = 300)