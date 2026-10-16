# Data cleaning


# 1 Library loading -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(writexl)

# paths relative to the repository root
raw_dir <- here("data", "raw")
processed_dir <- here("data", "processed")
dir.create(processed_dir, showWarnings = FALSE)

# Pupils Dataset --------------------------------------------------------------

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



# Cito Dataset ----------------------------------------------------------------

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

## Export ----

# save cleaned Cito data to data/processed
write_xlsx(cito_clean, file.path(processed_dir, "cito_clean.xlsx"))


# Final dataset: Pupils + Cito -----------------------------------------------

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


# Support Dataset -------------------------------------------------------------

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

## Number of support interventions per pupil ----

# started interventions per pupil
support_per_pupil <- support_clean |>
  count(`Student name`, name = "n_support")

# add to final dataset (pupils without support get 0)
final_dataset <- final_dataset |>
  left_join(support_per_pupil,
            by = join_by(pupil_name == `Student name`),
            relationship = "one-to-one") |>
  mutate(n_support = replace_na(n_support, 0L))


# Questionnaire Dataset -------------------------------------------------------

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

## Average questionnaire score per pupil ----

# mean of all answers (q1-q7) over all group 3 questionnaires
questionnaire_per_pupil <- questionnaire_clean |>
  pivot_longer(q1:q7, values_to = "answer") |>
  summarise(questionnaire_mean_g3 = mean(answer, na.rm = TRUE), .by = pupil_name)

# add to final dataset (pupils without group 3 questionnaire stay empty)
final_dataset <- final_dataset |>
  left_join(questionnaire_per_pupil,
            by = join_by(pupil_name),
            relationship = "one-to-one")


# Learning Goals Dataset ----------------------------------------------------

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


# Observations Dataset --------------------------------------------------------

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

## Last score per pupil and goal ----

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


# Codebook --------------------------------------------------------------------

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
    .default = NA
  )) |>
  mutate(description = as.character(description)) |>
  select(variable, description)


# Export final dataset --------------------------------------------------------

# save final dataset with the codebook on the second sheet
write_xlsx(
  list(final_dataset = final_dataset, codebook = codebook),
  file.path(processed_dir, "final_dataset.xlsx")
)
