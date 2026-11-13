library(tidyverse)
library(rvest)

webpage <- read_html("https://uofi.app.box.com/s/vhbl9evt5aoqyiduq4pkgehm5on9cuaq/file/2204276315375")
tables <- webpage |>
  html_table()

games <- tables[[2]]


pennStateGames <- games |>
  filter(
    season == 2026,
    status_type_completed == TRUE,
    neutral_site == FALSE,
    home_location == "Penn State" | away_location == "Penn State"
  ) |>
  mutate(
    location = if_else(
      home_location == "Penn State", "Home", "Away"
    ),
    points = if_else(
      home_location == "Penn State", home_score, away_score
    )
  )

scoringAverages <- pennStateGames |>
  group_by(location) |>
  summarise(
  average_points = mean(points, na.rm = TRUE),
  .groups = "drop"
  ) |>
  ggpot(aes(x = location, y = average_points)) +
  geom_col(fill = "blue") +
  labs(
    title = "Penn State Women's Basketball: Home vs. Away",
    subtitle = "2026 season. Neutral-site games excluded",
    x = "Game Location",
    y = "Average Points per Game"
  ) +
  theme_bw()
