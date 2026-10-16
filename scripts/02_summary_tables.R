# 
#
# Summary table 1: Time in kindergarten
#   - Months in kindergarten: mean, spread, outliers
#   - Age at the start of group 3
#   - Broken down by birth quarter, with winter-born pupils (Oct–Dec) in detail
#
#   Quick Look:
#     - table1:          time in kindergarten and age at the start of group 3 per birth quarter, with the number of pupils who spent more than 6 months longer or shorter than average
#     - late_born_table: the same figures for pupils born October–December only, split by whether they stayed longer, moved up early or were around average
#     - plot1:           histogram of months in kindergarten, with the average and a band of ±6 months
#     - plot2:           months in kindergarten per birth month for each pupil, highlighting late-year born pupils (Oct–Dec)
#
#
# Summary table 2: Transition goals
#   - All 34 goals (19 basic, 15 expected)
#   - Average % of the 34 goals the pupils have achieved at the start of group 3
#   - Lowest and highest % across pupils
#
#
library(tidyverse)
library(here)
library(readxl)
# ---- 0. Setup ----------------------------------------------------------------

analysis <- read_excel(here("data/processed/final_dataset.xlsx"), sheet = "final_dataset") |>
  mutate(across(c(date_of_birth, start_date, group3_start), as_date))

#view(analysis)
#names(analysis)

# ---- 1a. Summary table 1: Time in kindergarten: overall average --------------

table1_data <- analysis |>
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

# ---- 1b. Summary table 1: Time in kindergarten: deviation --------------------

# which pupils deviate a lot from the average pupil?
avg_months <- mean(table1_data$months_kindergarten)

table1_data <- table1_data |>
  mutate(deviation = case_when(
    months_kindergarten > avg_months + 6 ~ "Longer than average",
    months_kindergarten < avg_months - 6 ~ "Shorter than average",
    TRUE                                 ~ "Around average"
  ))

table1_data |>
  count(deviation)

# ---- 1c. Summary table 1: Time in kindergarten: birth quarters ---------------

table1_data <- table1_data |>
  mutate(birth_period = case_when(
    birth_quarter == 1 ~ "Jan–Mar",
    birth_quarter == 2 ~ "Apr–Jun",
    birth_quarter == 3 ~ "Jul–Sep",
    birth_quarter == 4 ~ "Oct–Dec"
  ))

# ---- 1d. Summary table 1: Time in kindergarten: birth period -----------------

table1_by_period <- table1_data |>
  group_by(birth_period) |>
  summarise(
    pupils         = n(),
    avg_months     = mean(months_kindergarten),
    min_months     = min(months_kindergarten),
    max_months     = max(months_kindergarten),
    n_longer       = sum(deviation == "Longer than average"),
    n_shorter      = sum(deviation == "Shorter than average"),
    avg_age_gr3    = mean(age_start_group3)
  )

# ---- 1e. Summary table 1: Time in kindergarten: add everything together ------

table1_total <- table1_data |>
  summarise(
    pupils         = n(),
    avg_months     = mean(months_kindergarten),
    min_months     = min(months_kindergarten),
    max_months     = max(months_kindergarten),
    n_longer       = sum(deviation == "Longer than average"),
    n_shorter      = sum(deviation == "Shorter than average"),
    avg_age_gr3    = mean(age_start_group3)
  ) |>
  mutate(birth_period = "All pupils")

table1 <- bind_rows(table1_by_period, table1_total)

table1 <- table1 |>
  mutate(across(c(avg_months, min_months, max_months, avg_age_gr3),
                \(x) round(x, 1)))

table1

write_csv(table1, here("output/tables/table1_time_in_kindergarten.csv"))

# ---- 1f. Summary table 1: Time in kindergarten: late-born pupils (Oct-Docs)---

late_born_table <- table1_data |>
  filter(birth_quarter == 4) |>
  group_by(deviation) |>
  summarise(
    pupils      = n(),
    avg_months  = round(mean(months_kindergarten), 1),
    avg_age_gr3 = round(mean(age_start_group3), 1)
  )

late_born_table

# ---- 2a. Summary table 1: visualization setup --------------------------------

# birth month as a number (1 = January ... 12 = December)
table1_data <- table1_data |>
  mutate(birth_month = month(date_of_birth))

# put the birth periods in calendar order (otherwise R sorts them alphabetically)
table1_data <- table1_data |>
  mutate(birth_period = factor(birth_period,
                               levels = c("Jan–Mar", "Apr–Jun", "Jul–Sep",
                                          "Oct–Dec (Late-year born)")))

table1_data <- table1_data |>
  mutate(late_born = month(date_of_birth) >= 10)

# ---- 2a. Summary table 1: visualization 1 ------------------------------------
# How long do pupils spend in kindergarten?

plot1 <- ggplot(table1_data, aes(x = months_kindergarten)) +
  annotate("rect",
           xmin = avg_months - 6, xmax = avg_months + 6,
           ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_histogram(binwidth = 2, fill = "#DF892E", colour = "white") +
  geom_vline(xintercept = avg_months, linetype = "dashed") +
  labs(title    = "Most pupils spend 20 to 32 months in kindergarten",
       subtitle = "Dashed line = average. Grey band = within 6 months of the average.",
       x = "Months in kindergarten",
       y = "Number of pupils") +
  theme_minimal()

plot1

# ---- 2a. Summary table 1: visualization 22 -----------------------------------
# Time in kindergarten by birth month

plot2 <- ggplot(table1_data,
                 aes(x = birth_month, y = months_kindergarten, colour = late_born)) +
  geom_hline(yintercept = avg_months, linetype = "dashed") +
  geom_jitter(width = 0.15, height = 0, size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "#DF892E"),
                      labels = c("Other pupils", "Late-born (Oct–Dec)"),
                      name   = NULL) +
  labs(title = "Late-born pupils usually spend less time in kindergarten",
       subtitle = "Each dot is one pupil. Dashed line = average.",
       x = "Birth month",
       y = "Months in kindergarten") +
  theme_minimal() +
  theme(legend.position = "top")

plot2



# ---- 3. Summary table 2: Transition goals ------------------------------------