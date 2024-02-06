library(tidyverse)
library(worldfootballR)
library(janitor)

### Every shot from every match of the top 5 European leagues
shots_top5 <- load_fb_match_shooting(
  country = c('ENG','GER','ESP','ITA','FRA'),
  gender = 'M',
  tier = '1st',
  season_end_year = 2021:2024
)

st5 <- shots_top5 |> 
  clean_names() |>
  separate(player, into = c('player','penalty'),
           sep = " \\(") |> 
  mutate(xg = as.numeric(x_g),
         psxg = ifelse(
           is.na(as.numeric(p_sx_g)),
           0, as.numeric(p_sx_g)
           ),
         on_target = ifelse(
           is.na(as.numeric(p_sx_g)),
           "Off-Target", "On-Target"
         ),
         penalty = ifelse(is.na(penalty), 0, 1),
         shot_class = case_when(
           penalty == 1 ~ "Penalty",
           xg >= 0.16 ~ "Above-Average Chance",
           xg >= 0.08 ~ "Average Chance",
           xg >= 0.03 ~ "Below-Average Chance",
           .default = "Poor Chance"
         )) |>
  select(2:7, penalty, xg, psxg, outcome, shot_class, distance, body_part, on_target, country, season_end_year, match_url)

st5 |>
  group_by(shot_class) |>
  summarize(
    n = n(),
    xg_min = min(xg),
    xg_max = max(xg),
    miss_target = sum(psxg == 0),
    subtr_value = sum(psxg != 0 & psxg < xg),
    added_value = sum(psxg != 0 & psxg >= xg),
  ) |>
  mutate(across(c(miss_target:added_value),
                ~.x/n),
         pct = n/sum(n)) |>
  arrange(-xg_min, shot_class) |>
  select(1:2, pct, everything())

st5 |>
  group_by(xg) |>
  summarize(n = n(),
            miss_target = sum(psxg == 0),
            subtr_value = sum(psxg != 0 & psxg < xg),
            equal_value = sum(psxg != 0 & psxg == xg),
            added_value = sum(psxg != 0 & psxg > xg),
            goal = sum(outcome == "Goal")) |>
  arrange(xg) |>
  mutate(across(c(miss_target:added_value),
        ~ .x / n),
        cdf = cumsum(n)/sum(n),
        shot_class = case_when(
          xg >= 0.15 ~ "Above-Average Chance",
          xg >= 0.08 ~ "Average Chance",
          xg >= 0.03 ~ "Below-Average Chance",
          .default = "Poor Chance"
        )
  ) 


st5 |>
  mutate(on_target = ifelse(outcome == "Goal" |outcome == "Saved",
                            "On-Target",
                            "Off-Target"
                            )) |>
  group_by(on_target) |>
  summarize(n = n())


### compute sample-wide averages
season_wide_averages <- st5 |> group_by(country, season_end_year) |>
  summarize(
    shots = n(),
    goals_avg = sum(outcome == "Goal"),
    miss_target_avg = sum(psxg == 0),
    subtr_value_avg = sum(psxg != 0 & psxg <= xg),
    added_value_avg = sum(psxg != 0 & psxg > xg)
  ) |>
  mutate(
    across(goals_avg:added_value_avg, ~ .x / shots)
  )  |> select(-shots)


st5 |> 
  group_by(player, squad, country, season_end_year) |>
  summarize(
    shots = n(),
    goals = sum(outcome == "Goal"),
    xG = sum(xg),
    miss_target = sum(psxg == 0),
    subtr_value = sum(psxg != 0 & psxg <= xg),
    added_value = sum(psxg != 0 & psxg > xg)
            ) |>
  filter(shots >= 25) |>
  left_join(season_wide_averages) |>
  mutate(across(goals_avg:added_value_avg, ~round(.x * shots *100)/100),
         goals_diff = goals - xG,
         added_value_diff = added_value - added_value_avg,
         added_value_pct = added_value/shots,
         added_value_ppdiff = added_value_pct-(added_value_avg/shots)) |>
  arrange(-added_value_ppdiff)
  

cooked <- st5 |> 
  group_by(player, squad, country, season_end_year) |>
  summarize(
    shots = n(),
    goals = sum(outcome == "Goal"),
    xG = sum(xg),
    miss_target = sum(psxg == 0),
    subtr_value = sum(psxg != 0 & psxg <= xg),
    added_value = sum(psxg != 0 & psxg > xg)
  ) |>
  filter(shots >= 15) |>
  left_join(season_wide_averages) |>
  mutate(across(goals_avg:added_value_avg, ~round(.x * shots *100)/100),
         goals_diff = goals - xG,
         added_value_diff = added_value - added_value_avg,
         off_target_diff = miss_target - miss_target_avg,
         off_target_pct = miss_target/shots,
         added_value_pct = added_value/shots,
         ot_ppdiff = 100 * (off_target_pct - (miss_target_avg/shots)),
         added_value_ppdiff = 100 * (added_value_pct-(added_value_avg/shots))) |>
  arrange(added_value_ppdiff)

lm_model <- lm(goals_diff ~ added_value_diff, data = cooked)

ggplot(cooked, aes(y = goals_diff, x = added_value_diff)) +
  geom_point(alpha = 0.05) +
  geom_point(data = cooked |> dplyr::filter(stringr::str_detect(player, "Dejan Kulusevski")), size = 5) +
  stat_smooth(method = "lm", color = "blue", formula = y ~ x, method.args = list()) +
  ggrepel::geom_text_repel(data = cooked |> dplyr::filter(stringr::str_detect(player, "Dejan Kulusevski")),
                           aes(label = paste(squad, season_end_year, sep = " - "))) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  # Add any other customization or labels as needed
  labs(
    title = paste(cooked |> filter(str_detect(player, "Dejan Kulusevski")) |> ungroup() |> select(player) |> distinct() |> unlist() |> as.vector(), "Striking with Expectation", sep = " - "),
    subtitle = "sample limited to those with > 15 shots, seasons 2021-2024",
    y = "How many more goals (Goals - xG) than expected",
    x = "How many more added-value shots (PSxG > xG) than expected"
  ) +
  # Display equation coefficients
  annotate(
    "text",
    x = 20,
    y = 10,
    hjust = 1,
    vjust = 0,
    label = paste("Equation: [goals above expected] =", round(coef(lm_model)[1], 3), "+", round(coef(lm_model)[2], 3), "* [good shots above expected]")
  ) +
  # Display R-squared
  annotate(
    "text",
    x = 20,
    y = 9.75,
    hjust = 1,
    vjust = 1,
    label = paste("R-squared =", round(summary(lm_model)$r.squared, 3))
  )


st5 |> 
  group_by(country, squad) |>
  summarize(shots = n(),
            goals = sum(outcome == "Goal"),
            xg = sum(xg),
            psxg = sum(psxg)) |>
  mutate(xgdiff = goals - xg,
         psxgdiff = goals - psxg) |>
  arrange(psxgdiff)
