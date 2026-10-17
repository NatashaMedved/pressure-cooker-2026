# Analysis for Question 2


# 1 Setup ---------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(writexl)
library(broom)
library(sandwich)
library(lmtest)

# paths relative to the repository root (script in scripts/, data in data/)
raw_dir <- here("data", "raw")
processed_dir <- here("data", "processed")

# all tables and graphs of this script go to output/question_2
output_dir <- here("output", "question_2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# first year of the school year in which current_year_group was recorded
current_school_year_start <- 2026

# main color (for the graphs, can be set to something better later)
main_color <- "#C2410C"

# outcomes: Cito scores at the end of group 3
tests <- c(
  "Mathematics" = "mathematics_g3_end_score",
  "TMT" = "tmt_g3_end_score",
  "Spelling - Non-verbs" = "spelling_non_verbs_g3_end_score"
)


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

## Goal scores in long format ----

# one row per pupil and observed goal
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
  left_join(goals_clean, by = join_by(goal_code), relationship = "many-to-one")

## Readiness per pupil ----

# mean last score over the basic goals and over the expected goals
readiness <- pupil_goals |>
  summarise(mean = mean(last_score), .by = c(pupil_name, goal_type)) |>
  pivot_wider(names_from = goal_type, values_from = mean,
              names_glue = "{goal_type}_mean")

## Analysis dataset ----

analysis_data <- final_dataset |>
  mutate(
    # cohort = current group (all pupils in a group started group 3 in the same year)
    cohort = factor(current_year_group),
    # start of group 3: 1 September of the school year the pupil was in group 3
    group3_start = make_date(current_school_year_start - (current_year_group - 3), 9, 1),
    age_g3_months = time_length(interval(as_date(date_of_birth), group3_start), "month")
  ) |>
  left_join(readiness, by = join_by(pupil_name), relationship = "one-to-one") |>
  # only pupils with at least one Cito result at the end of group 3
  filter(!is.na(mathematics_g3_end_score) | !is.na(tmt_g3_end_score) |
           !is.na(spelling_non_verbs_g3_end_score))

## Model data per test  ----

maths_data <- analysis_data |>
  select(mathematics_g3_end_score, cohort, age_g3_months, basic_mean, expected_mean) |>
  drop_na()

tmt_data <- analysis_data |>
  select(tmt_g3_end_score, cohort, age_g3_months, basic_mean, expected_mean) |>
  drop_na()

spelling_data <- analysis_data |>
  select(spelling_non_verbs_g3_end_score, cohort, age_g3_months, basic_mean, expected_mean) |>
  drop_na()

# standardized versions (numeric variables as z-scores) for standardized betas
maths_data_std <- maths_data |> mutate(across(where(is.numeric), \(x) as.numeric(scale(x))))
tmt_data_std <- tmt_data |> mutate(across(where(is.numeric), \(x) as.numeric(scale(x))))
spelling_data_std <- spelling_data |> mutate(across(where(is.numeric), \(x) as.numeric(scale(x))))


# 4 Descriptive statistics ----------------------------------------------------

# pupils per cohort
count(analysis_data, cohort)

# outcomes and predictors
descriptives <- analysis_data |>
  select(
    Mathematics = mathematics_g3_end_score,
    TMT = tmt_g3_end_score,
    `Spelling - Non-verbs` = spelling_non_verbs_g3_end_score,
    `Readiness (basic goals)` = basic_mean,
    `Readiness (expected goals)` = expected_mean,
    `Age at start of group 3 (months)` = age_g3_months
  ) |>
  pivot_longer(everything(), names_to = "Variable", values_drop_na = TRUE) |>
  summarise(
    N = n(),
    M = mean(value),
    SD = sd(value),
    Min = min(value),
    Max = max(value),
    .by = Variable
  )

descriptives

# correlations between outcomes and readiness
correlations <- analysis_data |>
  select(
    Mathematics = mathematics_g3_end_score,
    TMT = tmt_g3_end_score,
    `Spelling - Non-verbs` = spelling_non_verbs_g3_end_score,
    `Readiness (basic goals)` = basic_mean,
    `Readiness (expected goals)` = expected_mean
  ) |>
  cor(use = "pairwise.complete.obs") |>
  as_tibble(rownames = "Variable")

correlations


# 5 Models --------------------------------------------------------------------

## 5.1 Effect of basic and expected goals on Cito ----
# per test three nested models: controls -> + basic goals -> + expected goals
# controled variables: cohort and age at the start of group 3

### Mathematics: model_1 to model_3 ----

model_1 <- lm(mathematics_g3_end_score ~ cohort + age_g3_months,
              data = maths_data)
model_2 <- lm(mathematics_g3_end_score ~ cohort + age_g3_months + basic_mean,
              data = maths_data)
model_3 <- lm(mathematics_g3_end_score ~ cohort + age_g3_months + basic_mean + expected_mean,
              data = maths_data)

# model comparison and full model
anova(model_1, model_2)
anova(model_2, model_3)
summary(model_3)

# standardized coefficients of the full model
model_3_std <- lm(mathematics_g3_end_score ~ cohort + age_g3_months + basic_mean + expected_mean,
                  data = maths_data_std)

# assumption checks: diagnostic plots, normality, influential cases
par(mfrow = c(2, 2)); plot(model_3); par(mfrow = c(1, 1))
shapiro.test(residuals(model_3))
max(cooks.distance(model_3))

# robust standard errors (HC3) and refit without outliers
model_3_hc3 <- coeftest(model_3, vcov = vcovHC(model_3, type = "HC3"))
model_3_hc3
model_3_no_outliers <- update(model_3, data = filter(maths_data, abs(rstandard(model_3)) <= 3))
summary(model_3_no_outliers)

### TMT: model_4 to model_6 ----

model_4 <- lm(tmt_g3_end_score ~ cohort + age_g3_months,
              data = tmt_data)
model_5 <- lm(tmt_g3_end_score ~ cohort + age_g3_months + basic_mean,
              data = tmt_data)
model_6 <- lm(tmt_g3_end_score ~ cohort + age_g3_months + basic_mean + expected_mean,
              data = tmt_data)

# model comparison and full model
anova(model_4, model_5)
anova(model_5, model_6)
summary(model_6)

# standardized coefficients of the full model
model_6_std <- lm(tmt_g3_end_score ~ cohort + age_g3_months + basic_mean + expected_mean,
                  data = tmt_data_std)

# assumption checks: diagnostic plots, normality, influential cases
par(mfrow = c(2, 2)); plot(model_6); par(mfrow = c(1, 1))
shapiro.test(residuals(model_6))
max(cooks.distance(model_6))

# robust standard errors (HC3) and refit without outliers
model_6_hc3 <- coeftest(model_6, vcov = vcovHC(model_6, type = "HC3"))
model_6_hc3
model_6_no_outliers <- update(model_6, data = filter(tmt_data, abs(rstandard(model_6)) <= 3))
summary(model_6_no_outliers)

### Spelling - Non-verbs: model_7 to model_9 ----

model_7 <- lm(spelling_non_verbs_g3_end_score ~ cohort + age_g3_months,
              data = spelling_data)
model_8 <- lm(spelling_non_verbs_g3_end_score ~ cohort + age_g3_months + basic_mean,
              data = spelling_data)
model_9 <- lm(spelling_non_verbs_g3_end_score ~ cohort + age_g3_months + basic_mean + expected_mean,
              data = spelling_data)

# model comparison and full model
anova(model_7, model_8)
anova(model_8, model_9)
summary(model_9)

# standardized coefficients of the full model
model_9_std <- lm(spelling_non_verbs_g3_end_score ~ cohort + age_g3_months + basic_mean + expected_mean,
                  data = spelling_data_std)

# assumption checks: diagnostic plots, normality, influential cases
par(mfrow = c(2, 2)); plot(model_9); par(mfrow = c(1, 1))
shapiro.test(residuals(model_9))
max(cooks.distance(model_9))

# robust standard errors (HC3) and refit without outliers
model_9_hc3 <- coeftest(model_9, vcov = vcovHC(model_9, type = "HC3"))
model_9_hc3
model_9_no_outliers <- update(model_9, data = filter(spelling_data, abs(rstandard(model_9)) <= 3))
summary(model_9_no_outliers)

## 5.2 Effect of individual goals on Cito (exploratory) ----------------------------------
# one model per goal and test: z(Cito) ~ z(goal score) + cohort + age

goal_vars <- str_subset(names(analysis_data), "^goal_")
goal_results <- list()

for (test in names(tests)) {
  for (goal in goal_vars) {
    outcome <- tests[[test]]

    # pupils with a score on this goal and this test
    goal_data <- analysis_data |>
      select(all_of(c(outcome, goal)), cohort, age_g3_months) |>
      drop_na()

    # skip goals with too few pupils or without variation in the goal score
    if (nrow(goal_data) < 10 || n_distinct(goal_data[[goal]]) < 2) next

    # Cito and goal score as z-scores (standardized beta)
    goal_data[[outcome]] <- as.numeric(scale(goal_data[[outcome]]))
    goal_data[[goal]] <- as.numeric(scale(goal_data[[goal]]))

    # cohort only as control if the pupils come from more than one cohort
    controls <- if (n_distinct(goal_data$cohort) > 1) c("cohort", "age_g3_months") else "age_g3_months"

    goal_model <- lm(reformulate(c(goal, controls), outcome), data = goal_data)

    goal_results[[str_c(test, goal)]] <- tidy(goal_model, conf.int = TRUE) |>
      filter(term == goal) |>
      mutate(test = test, n = nrow(goal_data))
  }
}

goal_effects <- bind_rows(goal_results) |>
  transmute(
    test,
    goal_code = str_remove(term, "^goal_"),
    n,
    beta = estimate,
    ci_low = conf.low,
    ci_high = conf.high,
    p = p.value
  ) |>
  # false discovery rate correction within each test
  mutate(p_fdr = p.adjust(p, method = "BH"), .by = test) |>
  left_join(goals_clean, by = join_by(goal_code))

# goals with the strongest relation per test
goal_effects |>
  arrange(test, p) |>
  select(test, goal_code, n, beta, ci_low, ci_high, p, p_fdr) |>
  print(n = Inf)

## 5.3 Priority goals: mastery vs. effect on Cito (exploratory) ----

# % of observed pupils mastering each goal (last score 3 or higher)
goal_mastery <- pupil_goals |>
  summarise(pct_mastered = 100 * mean(last_score >= 3), .by = goal_code)

priority_goals <- goal_effects |>
  left_join(goal_mastery, by = join_by(goal_code)) |>
  select(test, goal_type, goal_code, goal, pct_mastered, beta, ci_low, ci_high, p, p_fdr)

# goals mastered by fewer than 80% of pupils and related to Cito (FDR < .10)
priority_goals |>
  filter(pct_mastered < 80, beta > 0, p_fdr < .10) |>
  arrange(test, pct_mastered) |>
  print(n = Inf)


# 6 Export to Excel -----------------------------------------------------------

## Readiness model results ----

model_names <- c("Controls", "+ Basic goals", "+ Expected goals")

# model comparison (R², change in R² and F test per step)
readiness_comparison <- bind_rows(
  "Mathematics" = bind_cols(
    model = model_names,
    list_rbind(map(list(model_1, model_2, model_3), glance)) |>
      select(n = nobs, r2 = r.squared, adj_r2 = adj.r.squared),
    bind_rows(tidy(anova(model_1, model_2)), tidy(anova(model_2, model_3))[2, ]) |>
      select(df1 = df, df2 = df.residual, f_change = statistic, p = p.value)
  ),
  "TMT" = bind_cols(
    model = model_names,
    list_rbind(map(list(model_4, model_5, model_6), glance)) |>
      select(n = nobs, r2 = r.squared, adj_r2 = adj.r.squared),
    bind_rows(tidy(anova(model_4, model_5)), tidy(anova(model_5, model_6))[2, ]) |>
      select(df1 = df, df2 = df.residual, f_change = statistic, p = p.value)
  ),
  "Spelling - Non-verbs" = bind_cols(
    model = model_names,
    list_rbind(map(list(model_7, model_8, model_9), glance)) |>
      select(n = nobs, r2 = r.squared, adj_r2 = adj.r.squared),
    bind_rows(tidy(anova(model_7, model_8)), tidy(anova(model_8, model_9))[2, ]) |>
      select(df1 = df, df2 = df.residual, f_change = statistic, p = p.value)
  ),
  .id = "test"
) |>
  mutate(delta_r2 = r2 - lag(r2), .by = test, .after = adj_r2)

# coefficients of the full models with standardized beta
readiness_coefficients <- bind_rows(
  "Mathematics" = tidy(model_3, conf.int = TRUE) |>
    left_join(select(tidy(model_3_std), term, beta = estimate), by = join_by(term)),
  "TMT" = tidy(model_6, conf.int = TRUE) |>
    left_join(select(tidy(model_6_std), term, beta = estimate), by = join_by(term)),
  "Spelling - Non-verbs" = tidy(model_9, conf.int = TRUE) |>
    left_join(select(tidy(model_9_std), term, beta = estimate), by = join_by(term)),
  .id = "test"
) |>
  filter(term != "(Intercept)") |>
  # no standardized beta for the cohort categories
  mutate(beta = if_else(str_starts(term, "cohort"), NA, beta))

# robustness: HC3 standard errors and results without outliers
readiness_robustness <- bind_rows(
  "Mathematics" = tidy(model_3) |>
    select(term, b = estimate, se = std.error, p = p.value) |>
    left_join(select(tidy(model_3_hc3), term, se_hc3 = std.error, p_hc3 = p.value), by = join_by(term)) |>
    left_join(select(tidy(model_3_no_outliers), term, b_no_outliers = estimate, p_no_outliers = p.value), by = join_by(term)) |>
    mutate(outliers_removed = nobs(model_3) - nobs(model_3_no_outliers)),
  "TMT" = tidy(model_6) |>
    select(term, b = estimate, se = std.error, p = p.value) |>
    left_join(select(tidy(model_6_hc3), term, se_hc3 = std.error, p_hc3 = p.value), by = join_by(term)) |>
    left_join(select(tidy(model_6_no_outliers), term, b_no_outliers = estimate, p_no_outliers = p.value), by = join_by(term)) |>
    mutate(outliers_removed = nobs(model_6) - nobs(model_6_no_outliers)),
  "Spelling - Non-verbs" = tidy(model_9) |>
    select(term, b = estimate, se = std.error, p = p.value) |>
    left_join(select(tidy(model_9_hc3), term, se_hc3 = std.error, p_hc3 = p.value), by = join_by(term)) |>
    left_join(select(tidy(model_9_no_outliers), term, b_no_outliers = estimate, p_no_outliers = p.value), by = join_by(term)) |>
    mutate(outliers_removed = nobs(model_9) - nobs(model_9_no_outliers)),
  .id = "test"
) |>
  filter(term %in% c("basic_mean", "expected_mean"))

## Tables in APA format (for the report) ----
# p-values as ".034" and "<.001"

table_descriptives <- descriptives |>
  mutate(across(M:Max, \(x) round(x, 2)))

table_correlations <- correlations |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

table_readiness_comparison <- readiness_comparison |>
  transmute(
    Test = test,
    Model = model,
    N = n,
    `R²` = round(r2, 3),
    `Adj. R²` = round(adj_r2, 3),
    `ΔR²` = round(delta_r2, 3),
    `F change` = round(f_change, 2),
    df = if_else(is.na(df1), NA, str_c(df1, ", ", df2)),
    p = str_replace(scales::pvalue(p, accuracy = 0.001), "0\\.", ".")
  )

table_readiness_coefficients <- readiness_coefficients |>
  transmute(
    Test = test,
    Predictor = term,
    b = round(estimate, 2),
    SE = round(std.error, 2),
    `95% CI` = str_c("[", round(conf.low, 2), ", ", round(conf.high, 2), "]"),
    `β` = round(beta, 2),
    t = round(statistic, 2),
    p = str_replace(scales::pvalue(p.value, accuracy = 0.001), "0\\.", ".")
  )

table_readiness_robustness <- readiness_robustness |>
  transmute(
    Test = test,
    Predictor = if_else(term == "basic_mean", "Basic goals", "Expected goals"),
    b = round(b, 2),
    SE = round(se, 2),
    p = str_replace(scales::pvalue(p, accuracy = 0.001), "0\\.", "."),
    `SE (HC3)` = round(se_hc3, 2),
    `p (HC3)` = str_replace(scales::pvalue(p_hc3, accuracy = 0.001), "0\\.", "."),
    `Outliers removed` = outliers_removed,
    `b (without outliers)` = round(b_no_outliers, 2),
    `p (without outliers)` = str_replace(scales::pvalue(p_no_outliers, accuracy = 0.001), "0\\.", ".")
  )

table_individual_goals <- goal_effects |>
  arrange(test, desc(beta)) |>
  transmute(
    Test = test,
    `Goal type` = goal_type,
    `Goal code` = goal_code,
    Goal = goal,
    N = n,
    `β` = round(beta, 2),
    `95% CI` = str_c("[", round(ci_low, 2), ", ", round(ci_high, 2), "]"),
    p = str_replace(scales::pvalue(p, accuracy = 0.001), "0\\.", "."),
    `p (FDR)` = str_replace(scales::pvalue(p_fdr, accuracy = 0.001), "0\\.", ".")
  )

table_priority_goals <- priority_goals |>
  arrange(test, pct_mastered) |>
  transmute(
    Test = test,
    `Goal type` = goal_type,
    `Goal code` = goal_code,
    Goal = goal,
    `% mastered` = round(pct_mastered, 1),
    `β` = round(beta, 2),
    `p (FDR)` = str_replace(scales::pvalue(p_fdr, accuracy = 0.001), "0\\.", ".")
  )

# summary for the report: goals significant for at least one test after FDR correction,
# standardized beta per test, * = significant after FDR correction (p < .05)
table_significant_goals <- goal_effects |>
  filter(any(p_fdr < .05), .by = goal_code) |>
  mutate(beta_text = str_c(str_replace(sprintf("%.2f", beta), "0\\.", "."),
                           if_else(p_fdr < .05, "*", ""))) |>
  select(goal_type, goal_code, goal, test, beta_text) |>
  pivot_wider(names_from = test, values_from = beta_text) |>
  arrange(goal_type, goal_code) |>
  select(
    `Goal code` = goal_code,
    Goal = goal,
    `Goal type` = goal_type,
    all_of(names(tests))
  ) |>
  mutate(`Goal type` = str_to_sentence(`Goal type`))

table_significant_goals

write_xlsx(
  list(
    descriptives = table_descriptives,
    correlations = table_correlations,
    readiness_model_comparison = table_readiness_comparison,
    readiness_coefficients = table_readiness_coefficients,
    readiness_robustness = table_readiness_robustness,
    individual_goals = table_individual_goals,
    priority_goals = table_priority_goals,
    significant_goals = table_significant_goals
  ),
  file.path(output_dir, "analysis_goals_tables.xlsx")
)


# 7 Visualizations ------------------------------------------------------------

## Shared theme ----

theme_analysis <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey35", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    axis.text = element_text(color = "grey35"),
    axis.title.x = element_text(color = "grey20", margin = margin(t = 14)),
    axis.title.y = element_text(color = "grey20", margin = margin(r = 14)),
    strip.text = element_text(face = "bold", size = 12, hjust = 0.5,
                              margin = margin(t = 10, b = 8)),
    plot.margin = margin(30, 35, 30, 30),
    plot.background = element_rect(fill = "white", color = NA)
  )

## Effect of individual goals on Cito ----

plot_individual_goals <- goal_effects |>
  mutate(
    test = factor(test, levels = names(tests)),
    # sort goals by beta within each test (test added to the label, removed on the axis)
    goal_label = fct_reorder(str_c(goal_code, "___", test), beta)
  ) |>
  ggplot(aes(x = beta, y = goal_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0, color = "grey60") +
  geom_point(color = main_color, size = 2) +
  scale_y_discrete(labels = \(x) str_remove(x, "___.*$")) +
  facet_wrap(~ test, scales = "free_y") +
  labs(title = "Relation between each transition goal and Cito at the end of group 3",
       x = "Standardized beta (95% CI)", y = NULL) +
  theme_analysis +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 8))

plot_individual_goals

ggsave(file.path(output_dir, "analysis_individual_goals.jpg"),
       plot_individual_goals, width = 14, height = 10, dpi = 300)

## Mastery of the goals significantly related to Cito ----

# goals significant after FDR correction (for at least one test)
significant_mastery <- priority_goals |>
  filter(p_fdr < .05) |>
  distinct(goal_code, goal, pct_mastered) |>
  mutate(goal_label = str_wrap(str_c(goal_code, ": ", str_remove(goal, "\\.$")), 45))

plot_significant_mastery <- significant_mastery |>
  ggplot(aes(x = pct_mastered, y = fct_reorder(goal_label, pct_mastered),
             fill = pct_mastered)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = str_c(round(pct_mastered), "%")),
            hjust = -0.2, size = 4, color = "grey20") +
  # same gradient as the mastery charts: dark orange (50% or lower) -> yellow (80%) -> green (100%)
  scale_fill_gradientn(
    colors = c("#C2410C", "#EAB308", "#15803D"),
    values = scales::rescale(c(50, 80, 100)),
    limits = c(50, 100),
    oob = scales::squish,
    breaks = c(50, 80, 100),
    labels = c("50% or lower", "80%", "100%"),
    name = "Pupils mastering"
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = \(x) str_c(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = unit(8, "cm"), barheight = unit(0.35, "cm"))) +
  labs(
    title = "Percentage of pupils mastering each learning goal by end of kindergarten",
    subtitle = "Mastered = last observed score of 3 or higher in groups 1-2",
    x = "Pupils mastering the goal (% of observed pupils)",
    y = NULL
  ) +
  theme_analysis +
  theme(
    plot.subtitle = element_text(color = "grey35", lineheight = 1.3, margin = margin(b = 20)),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "grey20", size = 10, hjust = 0, lineheight = 1.1,
                               margin = margin(r = 8)),
    legend.position = "bottom",
    legend.title = element_text(color = "grey20", size = 10, margin = margin(b = 6)),
    legend.text = element_text(color = "grey35", size = 9),
    legend.margin = margin(t = 12)
  )

plot_significant_mastery

ggsave(file.path(output_dir, "analysis_significant_goals_mastery.jpg"),
       plot_significant_mastery, width = 10, height = 6, dpi = 300)
