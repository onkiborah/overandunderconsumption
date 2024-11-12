# Set working directory to access data files
setwd("D:/nsso")

# Load necessary libraries
library(abdiv)
library(dplyr)
library(haven)
library(ggplot2)
library(e1071)
library(car)
library(nortest)
library(tidyr)

# Load datasets
level01 <- read_dta("level_01.dta") # Basic household data
level02 <- read_dta("level_02.dta") # Demographic details
level03 <- read_dta("level_03.dta") # Consumption specifics
level05 <- read_dta("level_05.dta") # Food group quantity data
level06 <- read_dta("level_06.dta") # Food group value data
mpce <- read_dta("mpce_percentile.dta") # MPCE percentile data for expenditure levels
codes <- read.csv("codes.csv") # Item codes and descriptions
req <- read.csv("req.csv") # Requirements per food group

# Map expenditure percentiles to broader ranges in a new column
mpce <- mpce %>%
  mutate(new_percentile_NI = case_when(
    percentile_NI == 1 ~ 1,
    percentile_NI == 100 ~ 12,
    percentile_NI >= 2 & percentile_NI <= 10 ~ 2,
    percentile_NI >= 11 & percentile_NI <= 20 ~ 3,
    percentile_NI >= 21 & percentile_NI <= 30 ~ 4,
    percentile_NI >= 31 & percentile_NI <= 40 ~ 5,
    percentile_NI >= 41 & percentile_NI <= 50 ~ 6,
    percentile_NI >= 51 & percentile_NI <= 60 ~ 7,
    percentile_NI >= 61 & percentile_NI <= 70 ~ 8,
    percentile_NI >= 71 & percentile_NI <= 80 ~ 9,
    percentile_NI >= 81 & percentile_NI <= 90 ~ 10,
    percentile_NI >= 91 & percentile_NI <= 99 ~ 11
  ))    

# Merge datasets level05 and level06 on common columns
merged_data <- full_join(level05, level06, by = intersect(names(level05), names(level06)))

# Assign 'section' based on 'sector' and 'stratum' in level01
level01 <- level01 %>%
  mutate(section = case_when(
    sector %% 2 == 1 & stratum %% 2 == 1 ~ "R", # Rural
    sector %% 2 == 1 & stratum %% 2 == 0 ~ "D", # District
    sector %% 2 == 0 & stratum %% 2 == 1 ~ "U", # Urban
    TRUE ~ "P" # Peri-urban
  ))

# Merge datasets with additional details from codes, level01, and level03
merged_data <- merged_data %>%
  left_join(codes, by = "item_code") %>%
  left_join(select(level01, hhid, state, section), by = "hhid") %>%
  left_join(select(level03, hhid, hh_size), by = "hhid")

# Create a refined dataset with selected variables for further analysis
new_data <- merged_data %>%
  select(hhid, state, veg, nonveg, milk_conversion, dropdown, section, hh_size, cons_total_qty, tag, cons_total_value, food_group_code_ICMR, item_code, unit_factor, frequency, multiplier)

# Calculate daily consumption and value per household member and adjust for milk conversions and frequency
new_data <- new_data %>%
  mutate(
    daily_cons = (cons_total_qty * 1000 * milk_conversion) / (frequency * unit_factor * hh_size),
    weights = multiplier / 100,
    daily_value = cons_total_value / (frequency * hh_size)
  ) %>%
  filter(tag != "T" & tag != "D") # Filter out unnecessary tags

# Assign household diet type based on food group code
new_data <- new_data %>%
  group_by(hhid) %>%
  mutate(hh_eat_type = ifelse(any(food_group_code_ICMR == 4), "nonveg", "veg")) %>%
  ungroup()

# Separate data into two groups based on food group code: millets and non-millets
data_millets <- new_data %>%
  filter(food_group_code_ICMR == 8)

data_no_millets <- new_data %>%
  filter(food_group_code_ICMR != 8)

# Summarize daily consumption for millets by household and region-specific 'dropdown'
quantity_data_millets <- data_millets %>%
  group_by(hhid, dropdown, hh_eat_type, state, multiplier) %>%
  summarize(
    daily_cons = sum(daily_cons, na.rm = TRUE),
    .groups = 'drop'
  )

# Summarize daily consumption for non-millet food groups by household
quantity_data_no_millets <- data_no_millets %>%
  group_by(hhid, hh_eat_type, food_group_code_ICMR, state, multiplier) %>%
  summarize(
    daily_cons = sum(daily_cons, na.rm = TRUE),
    .groups = 'drop'
  )

# Filter millet consumption data based on state-specific 'dropdown' values
quantity_data_millets <- quantity_data_millets %>%
  filter(
    case_when(
      state %in% c(8, 3, 6, 24, 28, 32, 33, 36, 9) ~ dropdown == 2,
      TRUE ~ dropdown == 1
    ))

# Combine millet and non-millet data and remove rows with invalid food group codes
modified_table <- rbind(quantity_data_millets, quantity_data_no_millets) %>%
  filter(food_group_code_ICMR != "#N/A")

# Prepare a table to identify each household's diet type uniquely
hheat <- new_data %>%
  select(hhid, hh_eat_type) %>%
  distinct()

# Get the list of food groups for analysis, excluding non-vegetarian items (code 4)
all_food_group <- unique(modified_table$food_group_code_ICMR)
all_food_group <- setdiff(all_food_group, 4)

# Add missing food groups with zero consumption where necessary
add_missing_foodgroup_for_hhid <- function(df) {
  df %>%
    group_by(hhid) %>%
    complete(food_group_code_ICMR = all_food_group, fill = list(daily_cons = 0)) %>%
    ungroup()
}

# Apply function to add missing food groups to the dataset
result <- add_missing_foodgroup_for_hhid(modified_table)

# Re-add diet type column and prepare for joining with requirement data
modified_table2 <- result %>%
  left_join(hheat, by = "hhid") %>%
  mutate(food_group_code_ICMR = as.integer(food_group_code_ICMR))

# Join requirement data with modified table
modified_table2 <- modified_table2 %>%
  left_join(req, by = "food_group_code_ICMR")

# Calculate the deficit or surplus in consumption for each household based on diet type
quantity_data <- modified_table2 %>%
  mutate(
    deficit_surplus_household = case_when(
      hh_eat_type == "nonveg" ~ (daily_cons - nonveg),
      hh_eat_type == "veg" ~ (daily_cons - veg)
    ))

# Load and prepare household weights
weights <- level01 %>%
  select(hhid, multiplier) %>%
  mutate(weights = multiplier / 100)

# Apply weights to deficit/surplus and daily consumption values
quantity_data <- quantity_data %>%
  left_join(weights, by = "hhid") %>%
  mutate(
    deficit_surplus = weights * deficit_surplus_household,
    daily_cons_weighted = weights * daily_cons
  )

# Select relevant columns for final data preparation
quantity_data <- quantity_data %>%
  select(hhid, food_group, food_group_code_ICMR, hh_eat_type, deficit_surplus, daily_cons_weighted)

# Load additional data for the final dataset
level001 <- level01 %>%
  select(hhid, sector, section, state, multiplier)

# Merge final data with percentile and section data
final_data <- quantity_data %>%
  left_join(mpce, by = "hhid") %>%
  left_join(level01, by = "hhid") %>%
  mutate(
    weights = multiplier / 100
  )

# Keep only distinct rows with relevant columns
final_data2 <- final_data %>%
  select(hhid, state, hh_eat_type, food_group, food_group_code_ICMR, new_percentile_NI, weights, deficit_surplus, daily_cons_weighted)

# Summarize final data by food group, diet type, and consumption characteristics
final_data2 <- final_data2 %>%
  group_by(food_group_code_ICMR, hh_eat_type, food_group) %>%
  summarize(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()

# Calculate adjusted values for deficit/surplus and daily consumption by weight
final_data2 <- final_data2 %>%
  mutate(
    new_defsur = deficit_surplus / weights,
    new_dailycons =
      