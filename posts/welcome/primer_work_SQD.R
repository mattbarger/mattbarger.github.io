library(tidyverse)
library(worldfootballR)

### Every shot from every match of the top 5 European leagues
shots_top5 <- load_fb_match_shooting(
  country = c('ENG','GER','ESP','ITA','FRA'),
  gender = 'M',
  tier = '1st',
  season_end_year = 2021:2024
)

st5 <- shots_top5 |> 
  clean_names() |>
  filter(season_end_year == 2024) |>
  mutate(xg = as.numeric(x_g),
         psxg = ifelse(
           is.na(as.numeric(p_sx_g)),
           0, as.numeric(p_sx_g)
           )
         ) |>
  select(2:6, xg, psxg, outcome, distance, body_part, country, season_end_year, match_url)

st5 |> 
  group_by(country, squad) |>
  summarize(shots = n(),
            goals = sum(outcome == "Goal"),
            xg = sum(xg),
            psxg = sum(psxg)) |>
  mutate(xgdiff = goals - xg,
         psxgdiff = goals - psxg) |>
  arrange(psxgdiff)
