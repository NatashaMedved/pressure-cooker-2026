# =============================================================================
# Pressure Cooker: data cleaning and analysis
# =============================================================================
#
# One script for the whole project (open the .Rproj first).
#
# Parts
#   0  Setup ............ packages, folders and shared settings
#   1  Data cleaning .... raw files -> data/processed/final_dataset.xlsx
#   2  Table 1 .......... time in kindergarten by birth period
#   3  Table 2 .......... transition goals achieved at the start of group 3
#   4  Question 1 ....... staying longer in kindergarten, Cito and extra support
#   5  Question 2 ....... kindergarten skills (transition goals) and Cito
#
# Output
#   data/processed/   cleaned data (final_dataset.xlsx with codebook, cito_clean.xlsx)
#   output/table_1/   Table 1 tables and plots
#   output/table_2/   Table 2 tables
#   output/question_1/, output/question_2/   model tables and plots
#


# =============================================================================
# PART 0: SETUP
# =============================================================================

## Packages ----
library(tidyverse)
library(readxl)
library(here)
library(writexl)
library(broom)      # tidy model results
library(sandwich)   # robust standard errors (HC3)
library(lmtest)     # coeftest() for robust standard errors

## Folders (relative to the project root) ----
raw_dir       <- here("data", "raw")
processed_dir <- here("data", "processed")
out_table1    <- here("output", "table_1")
out_table2    <- here("output", "table_2")
out_q1        <- here("output", "question_1")
out_q2        <- here("output", "question_2")

walk(c(processed_dir, out_table1, out_table2, out_q1, out_q2),
     \(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))

## Shared settings ----

# first year of the school year in which the pupil file was exported (2026/27)
current_school_year_start <- 2026

# main color for the graphs
main_color <- "#C2410C"

# p-values in APA style: ".034" and "<.001"
apa_p <- \(p) str_replace(scales::pvalue(p, accuracy = 0.001), "0\\.", ".")

# shared ggplot theme for the analysis graphs
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


# =============================================================================
# PART 1: DATA CLEANING
# =============================================================================

## 1.1 Pupils ----

#load data
pupils_raw <- read_excel(file.path(raw_dir, "Pupils.xlsx"), sheet = "Export")

# clean data
pupils_clean <- pupils_raw |>
  # rename columns to short names without spaces
  rename(
    pupil_id = `Pupil ID`,
    date_of_birth = `Date of birth`,
    current_class = `Current class`,
    current_year_group = `Current year group`,
    pupil_name = Pupil,
    start_date = `Start date`
  ) |>
  # year group from text to integer
  mutate(current_year_group = as.integer(current_year_group)) |>
  # date columns to date format
  mutate(across(c(date_of_birth, start_date), as_date)) |>
  # keep only pupils in group 3 or higher
  filter(current_year_group >= 3)


## 1.2 Cito ----

# load data
cito_file <- file.path(raw_dir, "Cito_UvA_EN.xlsx")

# one sheet per cohort, read as text so the sheets can be stacked
cito_2028_raw <- read_excel(cito_file, sheet = "CLASS_2028", col_types = "text")
cito_2029_raw <- read_excel(cito_file, sheet = "CLASS_2029", col_types = "text")
cito_2030_raw <- read_excel(cito_file, sheet = "CLASS_2030", col_types = "text")
cito_2031_raw <- read_excel(cito_file, sheet = "CLASS_2031", col_types = "text")

# stack the four cohorts into one long table
cito_raw <- bind_rows(cito_2028_raw, cito_2029_raw, cito_2030_raw, cito_2031_raw)

# clean data
cito_clean <- cito_raw |>
  # rename columns to short names without spaces
  rename(
    grade = Grade,
    period = `Assessment period`,
    test = Test,
    score = Score,
    ability_score = `Ability score`,
    band = `Performance band`,
    band_value = `Performance band value`,
    cohort_year = `Cohort year`,
    pupil_name = Name
  ) |>
  # decimal commas to points, then text to numbers
  mutate(across(c(score, ability_score, band_value),
                \(x) as.numeric(str_replace(x, ",", ".")))) |>
  # "Grade 3" -> 3, cohort year to integer
  mutate(
    grade = as.integer(parse_number(grade)),
    cohort_year = as.integer(cohort_year)
  ) |>
  # remove extra spaces in text columns
  mutate(across(c(period, test, band), str_squish)) |>
  # band to ordered factor
  mutate(band = factor(band, levels = c("V-", "V", "IV", "III", "II", "I", "I+"),
                       ordered = TRUE))

# save cleaned Cito data to data/processed
write_xlsx(cito_clean, file.path(processed_dir, "cito_clean.xlsx"))


## 1.3 Final dataset: pupils + Cito ----

# Cito results to wide format: one row per pupil, score and band per test and moment
cito_wide <- cito_clean |>
  # repeated grade: keep only the first school year per pupil, test and grade
  filter(str_sub(period, -9) == min(str_sub(period, -9)),
         .by = c(pupil_name, test, grade)) |>
  # short names for the column names ("Spelling – Non-verbs" -> "spelling_non_verbs")
  mutate(
    test = str_to_lower(test) |>
      str_replace_all("[^a-z]+", "_") |>
      str_remove_all("^_|_$"),
    moment = word(period, 1)
  ) |>
  # keep only tests that are taken from group 3
  filter(test %in% c("tmt", "mathematics", "spelling_non_verbs")) |>
  # keep only results from group 3 and 4
  filter(grade %in% 3:4) |>
  # column order: test, grade, mid before end
  arrange(test, grade, desc(moment)) |>
  pivot_wider(
    id_cols = pupil_name,
    names_from = c(test, grade, moment),
    values_from = c(score, band),
    names_glue = "{test}_g{grade}_{moment}_{.value}",  # e.g. mathematics_g3_end_score
    names_vary = "slowest"
  )

# start final dataset: Cito results joined to pupils by exact name (all pupils kept)
final_dataset <- pupils_clean |>
  select(pupil_id, pupil_name, date_of_birth, start_date, current_year_group) |>
  left_join(cito_wide, by = join_by(pupil_name), relationship = "one-to-one")


## 1.4 Support ----

# load data (headers start in row 2)
support_raw <- read_excel(file.path(raw_dir, "Support interventions_care team.xlsx"), skip = 1)

# clean data
support_clean <- support_raw |>
  # drop the row number, so identical records become exact duplicates
  select(-Record) |>
  # remove extra spaces and capitals in the started column ("Yes " -> "yes")
  mutate(`Started?` = str_to_lower(str_squish(`Started?`))) |>
  # remove exact duplicate rows
  distinct() |>
  # keep only interventions that started (no waitlist)
  filter(`Started?` == "yes")

# number of support interventions per pupil

# started interventions per pupil
support_per_pupil <- support_clean |>
  count(`Student name`, name = "n_support")

# add to final dataset (pupils without support get 0)
final_dataset <- final_dataset |>
  left_join(support_per_pupil,
            by = join_by(pupil_name == `Student name`),
            relationship = "one-to-one") |>
  mutate(n_support = replace_na(n_support, 0L))


## 1.5 Questionnaire ----

# load data
questionnaire_raw <- read_excel(file.path(raw_dir, "pupil questionnaire.xlsx"))

# clean data
questionnaire_clean <- questionnaire_raw |>
  # rename columns to English
  rename(
    pupil_name = Leerling,
    pupil_number = Leerlingnummer,
    year_group = Leerjaar,
    administration = Afname,
    cohort = Cohort,
    period = Periode
  ) |>
  # Vraag_1 -> q1
  rename_with(\(x) str_replace(x, "Vraag_", "q"), starts_with("Vraag_")) |>
  # number columns to numeric
  mutate(across(c(year_group, administration, cohort, q1:q7), as.numeric)) |>
  # keep only questionnaires from group 3
  filter(year_group == 3)

# average questionnaire score per pupil

# mean of all answers (q1-q7) over all group 3 questionnaires
questionnaire_per_pupil <- questionnaire_clean |>
  pivot_longer(q1:q7, values_to = "answer") |>
  summarise(questionnaire_mean_g3 = mean(answer, na.rm = TRUE), .by = pupil_name)

# add to final dataset (pupils without group 3 questionnaire stay empty)
final_dataset <- final_dataset |>
  left_join(questionnaire_per_pupil,
            by = join_by(pupil_name),
            relationship = "one-to-one")


## 1.6 Learning goals ----

# load data (title rows above the headers)
goals_basic_raw <- read_excel(file.path(raw_dir, "Selected learning goals.xlsx"),
                              sheet = "Basic Goals - First Two Years", skip = 2)
goals_expected_raw <- read_excel(file.path(raw_dir, "Selected learning goals.xlsx"),
                                 sheet = "Goals Before Grade One", skip = 2)

# clean data: stack both sheets, goal type kept from the sheet
goals_clean <- bind_rows(basic = goals_basic_raw,
                         expected = goals_expected_raw,
                         .id = "goal_type") |>
  rename(
    goal_number = No.,
    goal = `Learning goal`,
    goal_code = `Code leerdoelentool`
  ) |>
  # remove extra spaces in the codes
  mutate(goal_code = str_squish(goal_code))


## 1.7 Observations ----

# load data (empty cells and "NA" text as missing)
obs_raw <- read_excel(
  file.path(raw_dir, "Observaties_groep_1_2.xlsx"),
  na = c("", "NA"),
  col_types = c(rep("text", 7), rep("guess", 5))
)

# clean data
obs_clean <- obs_raw |>
  # rename columns to English
  rename(
    objective_code = Leercode,
    subject = Vak,
    domain = Domein,
    learning_path = Leerpad,
    learning_path_code = Leerpadcode,
    pupil_name = Leerling,
    score = Score,
    date = Datum,
    period = Periode,
    school_year = Jaargang,
    subject_type = Vaktype,
    class_group = Groep
  ) |>
  # set types
  mutate(
    score = as.integer(score),
    period = as.integer(period),
    date = as_date(date)
  ) |>
  # scores outside 0-4 are entry errors, set to missing
  mutate(score = if_else(score %in% 0:4, score, NA)) |>
  # move FL_ codes from learning_path to learning_path_code
  mutate(
    learning_path_code = if_else(str_starts(learning_path, "FL_") &
                                   is.na(learning_path_code),
                                 learning_path, learning_path_code),
    learning_path = if_else(str_starts(learning_path, "FL_"),
                            NA, learning_path)
  ) |>
  # remove extra spaces in the codes
  mutate(across(c(objective_code, learning_path_code), str_squish)) |>
  # remove exact duplicate rows
  distinct() |>
  # keep only the 34 transition goals (code in either code column)
  filter(learning_path_code %in% goals_clean$goal_code |
           objective_code %in% goals_clean$goal_code)

# last score per pupil and goal

# last observed score per pupil and goal (average if several on the last date)
goal_scores <- obs_clean |>
  # goal code from the column where it was found
  mutate(goal_code = if_else(learning_path_code %in% goals_clean$goal_code,
                             learning_path_code, objective_code)) |>
  # only observations with a score and a date
  filter(!is.na(score), !is.na(date)) |>
  # keep the last date per pupil and goal
  filter(date == max(date), .by = c(pupil_name, goal_code)) |>
  summarise(last_score = mean(score), .by = c(pupil_name, goal_code))

# one column per goal, in the order of the goals list (all 34 goals kept)
goal_scores_wide <- goal_scores |>
  mutate(goal_code = factor(goal_code, levels = goals_clean$goal_code)) |>
  pivot_wider(
    names_from = goal_code,
    values_from = last_score,
    names_prefix = "goal_",
    names_expand = TRUE
  )

# add to final dataset (pupils without observations stay empty)
final_dataset <- final_dataset |>
  left_join(goal_scores_wide,
            by = join_by(pupil_name),
            relationship = "one-to-one")

## 1.8 Analysis variables ----

# group 3 start and time in kindergarten

# school year in which each pupil started group 3 (first group 3 Cito test)
group3_year <- cito_clean |>
  filter(grade == 3) |>
  mutate(year = as.numeric(str_extract(period, "[0-9]{4}"))) |>
  group_by(pupil_name) |>
  summarise(group3_year = min(year))

final_dataset <- final_dataset |>
  left_join(group3_year, by = join_by(pupil_name), relationship = "one-to-one") |>
  # no Cito yet (current group 3): derive from current year group
  mutate(group3_year = if_else(is.na(group3_year),
                               current_school_year_start - (current_year_group - 3),
                               group3_year)) |>
  mutate(
    group3_start        = make_date(group3_year, 9, 1),
    months_kindergarten = time_length(interval(start_date, group3_start), "month"),
    age_start_group3    = time_length(interval(date_of_birth, group3_start), "year"),
    birth_quarter       = quarter(date_of_birth),
    late_born           = month(date_of_birth) >= 10   # born Oct-Dec
  )

# transition goals achieved (Table 2) and skill domains

final_dataset <- final_dataset |>
  mutate(
    goals_observed     = rowSums(!is.na(across(starts_with("goal_")))),
    goals_achieved     = rowSums(across(starts_with("goal_")) >= 3, na.rm = TRUE),
    goals_pct          = 100 * goals_achieved / goals_observed,
    domain_maths       = rowMeans(across(starts_with(c("goal_TG", "goal_MK"))), na.rm = TRUE),
    domain_language    = rowMeans(across(starts_with(c("goal_AO", "goal_BV", "goal_IT", "goal_VO"))), na.rm = TRUE),
    domain_fine_motor  = rowMeans(across(starts_with("goal_KM")), na.rm = TRUE),
    domain_social_emot = rowMeans(across(starts_with(c("goal_TZ", "goal_BT", "goal_SA"))), na.rm = TRUE)
  )

# kindergarten class and starting level (from all observations)

obs_all <- obs_raw |>
  mutate(score = suppressWarnings(as.numeric(Score)),
         date  = as_date(Datum)) |>
  filter(score %in% 0:4) |>
  select(pupil_name = Leerling, class_group = Groep, score, date)

# class the pupil was observed in most often
kindergarten_class <- obs_all |>
  count(pupil_name, class_group) |>
  group_by(pupil_name) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  select(pupil_name, kindergarten_class = class_group)

# average observation score in the first 90 days after starting school
starting_level <- obs_all |>
  left_join(select(final_dataset, pupil_name, start_date), by = join_by(pupil_name)) |>
  filter(date <= start_date + days(90)) |>
  group_by(pupil_name) |>
  summarise(starting_level   = mean(score),
            starting_level_n = n())

final_dataset <- final_dataset |>
  left_join(kindergarten_class, by = join_by(pupil_name), relationship = "one-to-one") |>
  left_join(starting_level,     by = join_by(pupil_name), relationship = "one-to-one")

## 1.9 Codebook ----

# full test names for the Cito columns
test_labels <- c(
  tmt = "TMT",
  mathematics = "Mathematics",
  spelling_non_verbs = "Spelling – Non-verbs"
)

# full moment names for the Cito columns
moment_labels <- c(mid = "middle", end = "end")

# one row per variable in the final dataset with a short description
codebook <- tibble(variable = names(final_dataset)) |>
  # parts of the Cito column names (e.g. mathematics_g3_end_score)
  mutate(
    cito_test = str_match(variable, "^(.+)_g\\d_(mid|end)_(score|band)$")[, 2],
    cito_grade = str_match(variable, "_g(\\d)_")[, 2],
    cito_moment = str_match(variable, "_(mid|end)_")[, 2],
    goal_code = str_remove(variable, "^goal_")
  ) |>
  # goal descriptions from the goals list
  left_join(goals_clean, by = join_by(goal_code)) |>
  mutate(description = case_when(
    variable == "pupil_id" ~ "Pupil ID from the pupils file",
    variable == "pupil_name" ~ "Pupil name (Name Surname), used to link all files",
    variable == "date_of_birth" ~ "Date of birth",
    variable == "start_date" ~ "Date the pupil started at the school",
    variable == "current_year_group" ~ "Current group (3-8) in this school year",
    str_ends(variable, "_score") & !is.na(cito_test) ~
      str_glue("Cito {test_labels[cito_test]} score at the {moment_labels[cito_moment]} of group {cito_grade}"),
    str_ends(variable, "_band") & !is.na(cito_test) ~
      str_glue("Cito {test_labels[cito_test]} performance band (I = highest, V = lowest) at the {moment_labels[cito_moment]} of group {cito_grade}"),
    variable == "n_support" ~
      "Number of started support interventions (duplicates and waitlist removed), 0 = none",
    variable == "questionnaire_mean_g3" ~
      "Mean answer on the pupil questionnaire (questions 1-7, scale 1-3) over all group 3 questionnaires",
    !is.na(goal) ~
      str_glue("Last observed score (0-4) in groups 1-2 on {goal_type} transition goal {goal_number}: {goal}"),
    variable == "group3_year" ~ "School year in which the pupil started group 3 (first group 3 Cito test; otherwise from current year group)",
    variable == "group3_start" ~ "Start of group 3, set to 1 September of group3_year",
    variable == "months_kindergarten" ~ "Months from school start date to start of group 3",
    variable == "age_start_group3" ~ "Age in years at the start of group 3",
    variable == "birth_quarter" ~ "Birth quarter (1 = Jan-Mar ... 4 = Oct-Dec)",
    variable == "late_born" ~ "TRUE if born October-December",
    variable == "goals_observed" ~ "Number of the 34 transition goals observed",
    variable == "goals_achieved" ~ "Number of transition goals with a last score of 3 or higher",
    variable == "goals_pct" ~ "Percentage of observed transition goals achieved",
    str_starts(variable, "domain_") ~ "Average last score (0-4) on the transition goals in this skill domain",
    variable == "kindergarten_class" ~ "Kindergarten class the pupil was observed in most often",
    variable == "starting_level" ~ "Average observation score (0-4) in the first 90 days after starting school",
    variable == "starting_level_n" ~ "Number of observations the starting level is based on",
    .default = NA
  )) |>
  mutate(description = as.character(description)) |>
  select(variable, description)


## 1.10 Export final dataset ----

# save final dataset with the codebook on the second sheet
write_xlsx(
  list(final_dataset = final_dataset, codebook = codebook),
  file.path(processed_dir, "final_dataset.xlsx")
)


# =============================================================================
# PART 2: TABLE 1 - TIME IN KINDERGARTEN
# =============================================================================
# table1:          time in kindergarten and age at the start of group 3 per birth
#                  period, with the number of pupils who stayed longer or moved up
#                  early compared with peers born in the same month
# late_born_table: the same figures for pupils born October-December only
# plot1:           histogram of months in kindergarten (appendix)
# plot2:           months in kindergarten per birth month, late-born highlighted
#
# Uses final_dataset from Part 1.

## 2.1 Overall average ----

table1_data <- final_dataset |>
  filter(!is.na(months_kindergarten))

table1_data |>
  summarise(
    pupils       = n(),
    avg_months   = mean(months_kindergarten),
    sd_months    = sd(months_kindergarten),
    min_months   = min(months_kindergarten),
    max_months   = max(months_kindergarten),
    avg_age_gr3  = mean(age_start_group3)
  )

## 2.2 Compare each pupil with peers born in the same month ----

# compare each pupil with peers born in the same month
# (time in kindergarten depends on birth month, so the overall average is not a fair comparison)
avg_months <- mean(table1_data$months_kindergarten)   # still used in the plots

table1_data <- table1_data |>
  mutate(birth_month = month(date_of_birth)) |>
  group_by(birth_month) |>
  mutate(typical_months = median(months_kindergarten)) |>
  ungroup() |>
  mutate(
    diff_from_peers = months_kindergarten - typical_months,
    deviation = case_when(
      diff_from_peers >  6 ~ "Stayed longer",
      diff_from_peers < -6 ~ "Moved up early",
      TRUE                 ~ "Standard track"
    )
  )

table1_data |>
  count(deviation)

## 2.3 Birth periods ----

table1_data <- table1_data |>
  mutate(birth_period = case_when(
    birth_quarter == 1 ~ "Jan–Mar",
    birth_quarter == 2 ~ "Apr–Jun",
    birth_quarter == 3 ~ "Jul–Sep",
    birth_quarter == 4 ~ "Oct–Dec"
  ))

## 2.4 Summary per birth period ----

table1_by_period <- table1_data |>
  group_by(birth_period) |>
  summarise(
    pupils         = n(),
    avg_months     = mean(months_kindergarten),
    min_months     = min(months_kindergarten),
    max_months     = max(months_kindergarten),
    n_stayed_longer  = sum(deviation == "Stayed longer"),
    n_moved_up_early = sum(deviation == "Moved up early"),
    avg_age_months   = mean(age_start_group3) * 12
  )

## 2.5 Add a total row ----

table1_total <- table1_data |>
  summarise(
    pupils         = n(),
    avg_months     = mean(months_kindergarten),
    min_months     = min(months_kindergarten),
    max_months     = max(months_kindergarten),
    n_stayed_longer  = sum(deviation == "Stayed longer"),
    n_moved_up_early = sum(deviation == "Moved up early"),
    avg_age_months = mean(age_start_group3) * 12
  ) |>
  mutate(birth_period = "All pupils")

table1 <- bind_rows(table1_by_period, table1_total)

table1 <- table1 |>
  mutate(across(c(avg_months, min_months, max_months), \(x) round(x, 1)),
         avg_age_gr3 = str_c(floor(round(avg_age_months) / 12), "y ",
                             round(avg_age_months) %% 12, "m")) |>
  select(-avg_age_months)

table1

write_csv(table1, file.path(out_table1, "table1_time_in_kindergarten.csv"))

## 2.6 Late-born pupils (Oct-Dec) only ----

late_born_table <- table1_data |>
  filter(birth_quarter == 4) |>
  group_by(deviation) |>
  summarise(
    pupils         = n(),
    avg_months     = round(mean(months_kindergarten), 1),
    avg_age_months = mean(age_start_group3) * 12
  ) |>
  mutate(avg_age_gr3 = str_c(floor(round(avg_age_months) / 12), "y ",
                             round(avg_age_months) %% 12, "m")) |>
  select(-avg_age_months)

late_born_table
write_csv(late_born_table, file.path(out_table1, "table1b_late_born.csv"))

## 2.7 Plots ----

# put the birth periods in calendar order (otherwise R sorts them alphabetically)
table1_data <- table1_data |>
  mutate(birth_period = factor(birth_period,
                               levels = c("Jan–Mar", "Apr–Jun", "Jul–Sep", "Oct–Dec")))

# How long do pupils spend in kindergarten?

plot1 <- ggplot(table1_data, aes(x = months_kindergarten)) +
  geom_histogram(binwidth = 2, fill = "#DF892E", colour = "white") +
  geom_vline(xintercept = avg_months, linetype = "dashed") +
  labs(title    = "Time spent in kindergarten",
       subtitle = "Dashed line = average.",
       x = "Months in kindergarten",
       y = "Number of pupils") +
  theme_minimal()

plot1
ggsave(file.path(out_table1, "plot1_months_histogram.jpg"), plot1, width = 10, height = 6, dpi = 300)

# Time in kindergarten by birth month

# Each pupil is compared with peers born in the same month (short dark line =
# typical months for that birth month).
# Colours (same in every Q1 graph): grey = context, orange = stayed longer,
# blue = moved up early. Groups are labelled directly in the plot, no legend.

ink    <- "#1F1F1F"
muted  <- "#4F4F4F"
grey   <- "#A6A6A6"
orange <- "#F6A70A"   # report template accent
blue   <- "#0072B2"
red    <- "#C0392B"   # late-born band (light), so orange only means "stayed longer"

# typical months per birth month (one short line per month)
typical_by_month <- table1_data |>
  distinct(birth_month, typical_months)

plot2 <- ggplot(table1_data, aes(x = birth_month, y = months_kindergarten)) +
  # late-born months (Oct-Dec) as a light red band
  annotate("rect", xmin = 9.5, xmax = 12.5, ymin = -Inf, ymax = Inf,
           fill = red, alpha = 0.08) +
  annotate("text", x = 11, y = 43, label = "Late-born\n(Oct-Dec)",
           colour = red, fontface = "bold", size = 4.4, lineheight = 0.9) +
  # typical time per birth month
  geom_segment(data = typical_by_month,
               aes(x = birth_month - 0.35, xend = birth_month + 0.35,
                   y = typical_months, yend = typical_months),
               linewidth = 1, colour = ink) +
  geom_jitter(aes(colour = deviation), width = 0.15, height = 0, size = 3.2) +
  # direct labels instead of a legend
  annotate("text", x = 5.3, y = 40.6, label = "Stayed longer (about +1 year)",
           colour = orange, fontface = "bold", hjust = 0, size = 4.6) +
  annotate("text", x = 2.45, y = 18.6, label = "Moved up early (about -1 year)",
           colour = blue, fontface = "bold", hjust = 0, size = 4.6) +
  annotate("text", x = 6.7, y = 31.2, label = "Typical time for pupils\nborn in that month",
           colour = muted, hjust = 0, size = 4.2, lineheight = 0.9) +
  annotate("curve", x = 6.65, y = 30.6, xend = 6.1, yend = 27.1,
           curvature = 0.3, colour = muted, linewidth = 0.5,
           arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(limits = c(15, 44.5), breaks = seq(15, 40, 5)) +
  scale_colour_manual(values = c("Standard track" = grey,
                                 "Stayed longer"  = orange,
                                 "Moved up early" = blue),
                      guide = "none") +
  labs(subtitle = "Each dot is one pupil",
       x = "Birth month",
       y = "Months in kindergarten") +
  theme_minimal(base_size = 15, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#ECECEC", linewidth = 0.4),
        axis.text = element_text(colour = muted),
        axis.title = element_text(colour = ink),
        plot.subtitle = element_text(colour = muted),
        plot.background = element_rect(fill = "white", colour = NA))

plot2
ggsave(file.path(out_table1, "plot2_months_by_birth_month.png"), plot2, width = 7.6, height = 4.6, dpi = 300)


# =============================================================================
# PART 3: TABLE 2 - TRANSITION GOALS ACHIEVED AT THE START OF GROUP 3
# =============================================================================
# Uses final_dataset and goals_clean (the 34 transition goals) from Part 1.

# all tables of this part go to output/table_2
output_dir <- out_table2

## 3.1 Goal scores per pupil and goal ----

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


## 3.2 Percentage achieved per pupil ----

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


## 3.3 Table 2a: summary per goal type ----

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

## 3.4 Table 2b: results per goal ----

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


## 3.5 Export ----

# both tables in one Excel file
write_xlsx(
  list(table_2a_summary = table_2a, table_2b_goals = table_2b),
  file.path(output_dir, "table_2.xlsx")
)


# =============================================================================
# PART 4: QUESTION 1 - STAYING LONGER IN KINDERGARTEN
# =============================================================================
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
# NOTE: the "stayed longer" group is small (6 pupils with Cito).

## 4.1 Data preparation ----

# kindergarten track
# Step 1: use ALL pupils with months in kindergarten (104), so the "typical"
#         months per birth month is the same as in the summary tables.
# Step 2: keep only pupils with at least one Cito score at the end of group 3.
# Step 3: leave out pupils who moved up early.

q1_data <- final_dataset |>
  mutate(across(c(date_of_birth, start_date, group3_start), as_date)) |>
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
count(q1_data, track)

q1_data <- q1_data |>
  filter(track != "Moved up early") |>
  # Standard track first = reference group, so results read
  # "stayed longer compared with standard track"
  mutate(track = factor(track, levels = c("Standard track", "Stayed longer")))

# extra support in the group 3 year
# Source: data/raw/Support interventions_care team.xlsx
# - only interventions that actually started (support_clean, Part 1)
# - only in the school year the pupil was in group 3
# - "learning support" = Need is Learning, Language or Speech & language
#   (so motor therapy, behaviour etc. don't count)
# The file only covers school years 2023/24 - 2025/26, so pupils who were in
# group 3 before 2023 get NA (unknown), not "no support".

# support_clean from Part 1: spaces/capitals fixed, duplicates and waitlist removed
support_rows <- support_clean |>
  filter(!is.na(`School yr`)) |>
  transmute(
    name_key      = str_to_lower(str_squish(`Student name`)),
    school_year   = 2000 + as.integer(str_sub(as.character(`School yr`), 1, 2)),  # 2324 -> 2023
    learning_need = Need %in% c("Learning", "Language", "Speech & language")
  )

support_g3 <- q1_data |>
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

q1_data <- q1_data |>
  left_join(support_g3, by = join_by(pupil_id), relationship = "one-to-one")

# one dataset per test
# Model A (track + cohort) uses every pupil with a score on that test.
# Model B also needs starting level and class: drop_na() drops 2 of the 6
# "stayed longer" pupils (no observations in their first 90 days), so model B
# is only a check on 4 pupils (3 for TMT).

q1_maths_data_a <- q1_data |>
  select(score = mathematics_g3_end_score, track, cohort) |>
  drop_na()

q1_tmt_data_a <- q1_data |>
  select(score = tmt_g3_end_score, track, cohort) |>
  drop_na()

q1_spelling_data_a <- q1_data |>
  select(score = spelling_non_verbs_g3_end_score, track, cohort) |>
  drop_na()

q1_maths_data <- q1_data |>
  select(score = mathematics_g3_end_score, track, months_kindergarten,
         cohort, starting_level, kindergarten_class) |>
  drop_na()

q1_tmt_data <- q1_data |>
  select(score = tmt_g3_end_score, track, months_kindergarten,
         cohort, starting_level, kindergarten_class) |>
  drop_na()

q1_spelling_data <- q1_data |>
  select(score = spelling_non_verbs_g3_end_score, track, months_kindergarten,
         cohort, starting_level, kindergarten_class) |>
  drop_na()


## 4.2 Descriptives ----

count(q1_data, cohort)
count(q1_data, track)

# averages per track (the easiest table for the report)
track_summary <- q1_data |>
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


## 4.3 Models ----
# Per test:
#   model A: track + cohort (all pupils with a score = main result)
#   model B: + starting level and kindergarten class (check, fewer pupils)
#   months:  model B with months (number) instead of track (extra check)

# mathematics
maths_a      <- lm(score ~ track + cohort, data = q1_maths_data_a)
maths_b      <- lm(score ~ track + cohort + starting_level + kindergarten_class, data = q1_maths_data)
maths_months <- lm(score ~ months_kindergarten + cohort + starting_level + kindergarten_class, data = q1_maths_data)

# TMT
tmt_a      <- lm(score ~ track + cohort, data = q1_tmt_data_a)
tmt_b      <- lm(score ~ track + cohort + starting_level + kindergarten_class, data = q1_tmt_data)
tmt_months <- lm(score ~ months_kindergarten + cohort + starting_level + kindergarten_class, data = q1_tmt_data)

# spelling
spelling_a      <- lm(score ~ track + cohort, data = q1_spelling_data_a)
spelling_b      <- lm(score ~ track + cohort + starting_level + kindergarten_class, data = q1_spelling_data)
spelling_months <- lm(score ~ months_kindergarten + cohort + starting_level + kindergarten_class, data = q1_spelling_data)

# look at one model in full
summary(maths_b)

# checks on model B
# diagnostic plots (look for strange patterns or single extreme pupils)
par(mfrow = c(2, 2)); plot(maths_b);    par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(tmt_b);      par(mfrow = c(1, 1))
par(mfrow = c(2, 2)); plot(spelling_b); par(mfrow = c(1, 1))

# robust standard errors (HC3): safer with small, unequal groups
maths_b_hc3    <- coeftest(maths_b,    vcov = vcovHC(maths_b,    type = "HC3"))
tmt_b_hc3      <- coeftest(tmt_b,      vcov = vcovHC(tmt_b,      type = "HC3"))
spelling_b_hc3 <- coeftest(spelling_b, vcov = vcovHC(spelling_b, type = "HC3"))

## 4.4 Extra support in group 3 (yes/no) ----
# Small groups, so a simple table + Fisher's exact test instead of a logistic regression
support_table <- q1_data |>
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
support_test <- q1_data |>
  filter(!is.na(support_g3_learning)) |>
  with(fisher.test(table(track, support_g3_learning)))

support_test


## 4.5 Results tables ----

# main result: stayed longer vs standard track, in Cito points
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

# robustness: same model B, p-values with HC3 standard errors
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

# extra check: months in kindergarten as a number
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

# extra support
results_support <- support_table |>
  mutate(`Fisher p (learning support)` =
           if_else(track == "Stayed longer", apa_p(support_test$p.value), "-"))

results_support

# export
write_xlsx(
  list(
    track_summary  = track_summary,
    track_results  = results_track,
    track_hc3      = results_hc3,
    months_check   = results_months,
    support_group3 = results_support
  ),
  file.path(out_q1, "analysis_q1_tables.xlsx")
)

## 4.6 Plots ----

# colours (same in every Q1 graph): grey = context, orange = stayed longer,
# blue = moved up early; colourblind-friendly (ink, muted, grey from Part 2)
q1_orange <- "#F6A70A"   # report template accent: stayed longer

# Q1 theme: white background, light horizontal gridlines only, large text
# (theme_analysis stays as it is for Question 2)
theme_q1 <- theme_minimal(base_size = 15, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 17, colour = ink, margin = margin(b = 8)),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_markdown(colour = muted, lineheight = 1.25,
                                             margin = margin(b = 14)),
    axis.text = element_text(colour = muted),
    axis.title = element_text(colour = ink),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#ECECEC", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 15, colour = ink),
    plot.margin = margin(15, 20, 15, 15),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# coloured words in a subtitle (instead of a legend)
in_colour <- \(text, colour) str_c("<span style='color:", colour, "'>**", text, "**</span>")

# long format: one row per pupil per test
q1_plot_data <- q1_data |>
  select(track, months_kindergarten,
         Mathematics            = mathematics_g3_end_score,
         TMT                    = tmt_g3_end_score,
         `Spelling - Non-verbs` = spelling_non_verbs_g3_end_score) |>
  pivot_longer(c(Mathematics, TMT, `Spelling - Non-verbs`),
               names_to = "test", values_to = "score", values_drop_na = TRUE) |>
  mutate(test = factor(test, levels = c("Mathematics", "TMT", "Spelling - Non-verbs")))

# main plot: Cito per track
# average and 95% confidence interval of the average per test and track
# (not adjusted for cohort; the adjusted comparison is plot_estimate below)
track_means <- q1_plot_data |>
  summarise(
    n = n(),
    mean = mean(score),
    half_ci = qt(0.975, n - 1) * sd(score) / sqrt(n),
    .by = c(test, track)
  ) |>
  mutate(x = as.numeric(track), low = mean - half_ci, high = mean + half_ci)

plot_track <- ggplot(q1_plot_data, aes(x = track, y = score, colour = track)) +
  geom_jitter(width = 0.12, height = 0, size = 2.8) +
  # average with whiskers (95% confidence interval), right of the dots
  geom_errorbar(data = track_means,
                aes(x = x + 0.32, ymin = low, ymax = high),
                inherit.aes = FALSE, width = 0.12, colour = ink, linewidth = 0.8) +
  geom_point(data = track_means, aes(x = x + 0.32, y = mean),
             inherit.aes = FALSE, colour = ink, size = 3.2) +
  # number of pupils per test and track, under each group
  geom_text(data = track_means, aes(x = x, y = -Inf, label = str_c("n = ", n)),
            inherit.aes = FALSE, vjust = -0.6, size = 4, colour = muted) +
  facet_wrap(~ test, scales = "free_y") +
  scale_x_discrete(labels = c("Standard track" = "Standard\ntrack",
                              "Stayed longer" = "Stayed\nlonger")) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.05))) +
  scale_colour_manual(values = c("Standard track" = grey, "Stayed longer" = q1_orange),
                      guide = "none") +
  labs(subtitle = str_c("Each dot is one pupil. Black point = average,<br>",
                        "whiskers = where the true average likely lies (95% confidence interval)."),
       x = NULL, y = "Cito score (end of group 3)") +
  theme_q1

plot_track
ggsave(file.path(out_q1, "analysis_q1_cito_by_track.jpg"),
       plot_track, width = 8.6, height = 4.6, dpi = 300)

# main plot 2: difference with 95% CI, model A and model B
# The tests have different scales, so the difference is shown in standard
# deviations of each test; the label gives the difference in Cito points.
estimate_models <- tribble(
  ~test,                  ~model, ~fit,
  "Mathematics",          "A",    maths_a,
  "Mathematics",          "B",    maths_b,
  "TMT",                  "A",    tmt_a,
  "TMT",                  "B",    tmt_b,
  "Spelling - Non-verbs", "A",    spelling_a,
  "Spelling - Non-verbs", "B",    spelling_b
)

estimate_data <- estimate_models |>
  mutate(result = map(fit, \(model) {
    sd_score <- sd(model$model$score)
    tidy(model, conf.int = TRUE) |>
      filter(term == "trackStayed longer") |>
      mutate(n_stayed = sum(model$model$track == "Stayed longer"),
             estimate_sd = estimate / sd_score,
             low_sd = conf.low / sd_score,
             high_sd = conf.high / sd_score)
  })) |>
  select(-fit) |>
  unnest(result) |>
  mutate(
    test = factor(test, levels = rev(c("Mathematics", "TMT", "Spelling - Non-verbs"))),
    # model A slightly above, model B slightly below each test
    y = as.numeric(test) + if_else(model == "A", 0.17, -0.17),
    label = str_c(sprintf("%+.0f", estimate), " points (", n_stayed, " pupils)")
  )

model_colours <- c(A = q1_orange, B = "#8A8A8A")

plot_estimate <- ggplot(estimate_data, aes(x = estimate_sd, y = y, colour = model)) +
  # the only reference point: no difference
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink, linewidth = 0.6) +
  geom_errorbar(aes(xmin = low_sd, xmax = high_sd), width = 0.12, linewidth = 1,
                orientation = "y") +
  geom_point(aes(shape = model), size = 3.6) +
  # difference in Cito points, in the same colour as the line
  geom_text(aes(x = 1.85, label = label), size = 4.3, hjust = 0, fontface = "bold") +
  annotate("text", x = -0.1, y = 0.42, label = "← lower than standard track",
           hjust = 1, size = 4.2, colour = muted) +
  annotate("text", x = 0.1, y = 0.42, label = "higher →",
           hjust = 0, size = 4.2, colour = muted) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5)) +
  scale_y_continuous(breaks = 1:3, labels = levels(estimate_data$test)) +
  scale_colour_manual(values = model_colours, guide = "none") +
  scale_shape_manual(values = c(A = 16, B = 17), guide = "none") +
  # zoom with coord_cartesian, so the labels right of the panel are kept
  coord_cartesian(xlim = c(-1.8, 1.7), clip = "off") +
  labs(subtitle = str_c(in_colour("All pupils who stayed longer", q1_orange), "<br>",
                        in_colour("Only those with a known starting level", "#8A8A8A"),
                        "<br>Line = 95% confidence interval"),
       x = "Difference from the standard track (in SD)", y = NULL) +
  theme_q1 +
  theme(panel.grid.major.y = element_blank(),
        # room on the right for the labels in Cito points
        plot.margin = margin(15, 165, 15, 15),
        axis.text.y = element_text(face = "bold", size = 15, colour = ink))

plot_estimate
ggsave(file.path(out_q1, "q1_adjusted_difference.png"),
       plot_estimate, width = 7.4, height = 4.8, dpi = 300)

# extra plot (appendix): months in kindergarten (number) vs Cito
plot_months <- ggplot(q1_plot_data, aes(x = months_kindergarten, y = score)) +
  geom_point(aes(colour = track), size = 2.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = ink,
              fill = "#DDDDDD", linewidth = 0.8) +
  facet_wrap(~ test, scales = "free_y") +
  scale_colour_manual(values = c("Standard track" = grey, "Stayed longer" = q1_orange),
                      guide = "none") +
  labs(subtitle = str_c("Each dot is one pupil (", in_colour("stayed longer", q1_orange),
                        " or standard track). Line = overall trend with 95% confidence band."),
       x = "Months in kindergarten", y = "Cito score (end of group 3)") +
  theme_q1

plot_months
ggsave(file.path(out_q1, "analysis_q1_months_vs_cito.jpg"),
       plot_months, width = 11, height = 4.8, dpi = 300)


# =============================================================================
# PART 5: QUESTION 2 - KINDERGARTEN SKILLS AND CITO
# =============================================================================
# Which kindergarten skills (transition goals) go together with better Cito
# results at the end of group 3?
# Controls: cohort and age at the start of group 3
# Uses final_dataset and goals_clean from Part 1.

# outcomes: Cito scores at the end of group 3
tests <- c(
  "Mathematics" = "mathematics_g3_end_score",
  "TMT" = "tmt_g3_end_score",
  "Spelling - Non-verbs" = "spelling_non_verbs_g3_end_score"
)

# all tables and graphs of this part go to output/question_2
output_dir <- out_q2

## 5.1 Data preparation ----

# goal scores in long format

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

# readiness per pupil

# mean last score over the basic goals and over the expected goals
readiness <- pupil_goals |>
  summarise(mean = mean(last_score), .by = c(pupil_name, goal_type)) |>
  pivot_wider(names_from = goal_type, values_from = mean,
              names_glue = "{goal_type}_mean")

# analysis dataset

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

# one dataset per test

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


## 5.2 Descriptive statistics ----

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


## 5.3 Basic and expected goals and Cito ----
# per test three nested models: controls -> + basic goals -> + expected goals
# control variables: cohort and age at the start of group 3

# mathematics: model_1 to model_3

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

# TMT: model_4 to model_6

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

# spelling - non-verbs: model_7 to model_9

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

## 5.4 Individual goals and Cito (exploratory) ----
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

## 5.5 Priority goals: mastery vs. relation with Cito (exploratory) ----

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


## 5.6 Results tables ----

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

# tables in APA format (for the report)
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


## 5.7 Plots ----

# relation between each goal and Cito

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

# mastery of the goals significantly related to Cito

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
