## Import the tidyverse library
library(tidyverse)

## Load the calcium.csv file from the local filepath
calcium_dataframe <- read.csv("calcium.csv")

## Create a dataframe with only the null-treatment group
control_dataframe <- calcium_dataframe |>
  # Select the first 4 columns (those are the null treatment group columns)
  select(1:4) |>
  # Rename the columns to make them reproducible
  rename(
    'Initial' = y0,
    'Year1' = y1,
    'Year2' = y2,
    'Year3' = y3
  ) |>
  # Drop all rows with NA values
  drop_na() |>
  # Add a new column called 'treatment_group' and set all values to 'null treatment'
  mutate(
    treatment_group = "null treatment"
  )

## Create a dataframe with only the diet/exercise treatment group
treatment_dataframe <- calcium_dataframe |>
  # Select the last 4 columns (those are the diet/exercise treatment group columns)
  select(5:8) |>
  # Rename the columns to make them reproducible
  rename(
    'Initial' = y0.1,
    'Year1' = y1.1,
    'Year2' = y2.1,
    'Year3' = y3.1
  ) |>
  # Drop all NA values
  drop_na() |>
  # Add a new column called 'treatment_group'
  # and set all values to 'diet/exercise treatment'
  mutate(
    treatment_group = "diet/exercise treatment"
  )

## Combine the null and diet\exercise treatment dataframes and compute
## the average calcium level by treatment group per year
avg_calcium_tbl <- bind_rows(control_dataframe, treatment_dataframe) |>
  # Combine all the years into one column called 'year' and
  # store the values in a new column called 'calciumLevel'
  pivot_longer(
    cols = c(Initial, Year1, Year2, Year3),
    names_to = "Year",
    values_to = "calciumLevel"
  ) |>
  # group by 'Year' and 'treatment_group' to calculate the
  # average calcium level per year by treatment group
  group_by(Year, treatment_group) |>
  summarise(
    average_calcium = mean(calciumLevel),
    .groups = "drop"
  )

## Create a bar graph comparing the average
## calcium level per year by treatment group
# using the 'avg_calcium_tbl' dataframe, map 'Year' to the x-axis
# and 'average_calcium' to the y-axis; also, map 'treatment_group' to the
# color inside the bars so that we can distinguish which bar belongs to which group
ggplot(
  data = avg_calcium_tbl, 
  mapping = aes(x = Year, y = average_calcium, fill = treatment_group)
       ) +
  geom_col(position = "dodge") +
  # make proper labels representing the data
  labs(
    title = "Average Calcium Level per Year by Treatment Type",
    x = "Year",
    y = "Average Calcium Level",
    fill = "Treatment Group"
  ) +
  theme_bw()
