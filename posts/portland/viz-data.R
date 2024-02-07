library(worldfootballR)

mls_shots <- load_fb_match_shooting(
  country = "USA", tier = "1st", gender = "M",
  season_end_year = 2021:2023
) |>
  mutate(xG = as.numeric(xG),
         PSxG = ifelse(PSxG == "", 0, as.numeric(PSxG)),
         on_target = ifelse(PSxG == 0, 0, 1))


mls_shots |> distinct(Season_End_Year)

mls_results <- load_match_results(
  country = "USA", tier = "1st", gender = "M",
  season_end_year = 2021:2023
)

results_con <- mls_results |>
  select(
    Date,
    Team = Home,
    Opponent = Away,
    GF = HomeGoals,
    GA = AwayGoals
  ) |>
  mutate(Date = as.character(Date),
         Home_Away = "Home") |>
  bind_rows(
    mls_results |>
      select(
        Date,
        Team = Away,
        Opponent = Home,
        GF = AwayGoals,
        GA = HomeGoals
      ) |>
      mutate(Date = as.character(Date),
             Home_Away = "Away")
  ) 


full_shots <- mls_shots |>
  left_join(results_con, by = c('Date',c('Squad' = 'Team')))



full_shots |>
  group_by(Season_End_Year, Opponent) |>
  summarize(shots_allowed = n(),
            SoT_allowed   = sum(on_target),
            goals_allowed = sum(Outcome == "Goal"),
            xg_allowed = sum(xG)
            ) |>
  mutate(
    Shot_pct = goals_allowed/shots_allowed,
    SoT_pct = SoT_allowed/shots_allowed,
    GlSoT_pct = goals_allowed/SoT_allowed,
    SoT_exp = shots_allowed * 5505/16002,
    SoT_res = SoT_allowed - SoT_exp) |>
  filter(Season_End_Year == 2023) |>
  janitor::adorn_totals('row')


mls_salaries <- asa_client$get_player_salaries(leagues ='mls')

salaries_2023 <- mls_salaries |> 
  filter(season_name == 2023) |>
  left_join(
    all_players|>
      select(player_id, player_name, nationality)
    ) |>
  left_join(
    all_teams |>
      select(1:4)
  )


salaries_2023 |> 
  filter(team_abbreviation == "POR") |>
  group_by(player_id) |>
  filter(mlspa_release == max(mlspa_release)) |>
  arrange(-guaranteed_compensation) |>
  print(n = 31)

ga_clean_salaries <- asa_goals_added |>
  unnest(data) |>
  filter(season_name == 2023) |>
  select(-count_actions, -goals_added_above_avg) |>
  pivot_wider(names_from = action_type, values_from = goals_added_raw) |>
  mutate(gadd_player = Dribbling + Fouling + Interrupting + Passing + Receiving + Shooting,
         gaddplayer96 = 96 * gadd_player/minutes_played,
         gadd_team   = gadd_player - Interrupting,
         season_name = as.integer(season_name)) |>
  right_join(salaries_2023) |>
  #filter(team_abbreviation == "ORL") |>
  group_by(player_id) |>
  filter(mlspa_release == max(mlspa_release)) |>
  arrange(-base_salary) |>
  mutate(salary = base_salary/1000,
         m90s = minutes_played/90)

asa_goals_added |>
  unnest(data) |>
  filter(season_name == 2023) |>
  select(-count_actions, -goals_added_above_avg) |>
  pivot_wider(names_from = action_type, values_from = goals_added_raw) |>
  mutate(gadd_player = Dribbling + Fouling + Interrupting + Passing + Receiving + Shooting,
         gadd_team   = gadd_player - Interrupting,
         season_name = as.integer(season_name)) |>
  summarize(mean(gadd_player), sd(gadd_player))

ga_clean_salaries |>
  ggplot(aes(x = salary, y = gadd_player)) +
  annotate('rect', xmin = 1000, xmax = 2000, ymin = 0, ymax = 7.5, fill = "#00472b", alpha = 0.6) +
  annotate('rect', xmin = 300, xmax = 1000, ymin = 0, ymax = 7.5, fill = "#5b781a", alpha = 0.6) +
  annotate('rect', xmin = 65, xmax = 300, ymin = 0, ymax = 7.5, fill = "#d69a00", alpha = 0.6) +
  geom_hline(yintercept = c(1.82, 3.6, 6.78), color = 'white', size = 2) +
  geom_point(aes(size = m90s), alpha = 0.1, color = "grey70") +
  geom_point(data = ga_clean_salaries |> filter(team_abbreviation == "POR"), aes(size = m90s)) +
  coord_trans(x = "log10") +
  ggrepel::geom_text_repel(data = ga_clean_salaries |> filter(team_abbreviation == "POR"), aes(label = paste(player_name, " (", general_position, ")", sep = ""))) +
  labs(
    title = 'Money for Nothing: Portland Timbers 2023 Wages x Output',
    subtitle = 'points sized by minutes played',
    y = 'Raw Goals Added',
    x = 'Base Salary ($ 000)'
  )
  )

ga_clean_salaries |>
  ggplot(aes(x = salary, y = gaddplayer96)) +
 geom_point(aes(size = m90s)) +
  coord_trans(x = "log10") +
  ggrepel::geom_text_repel(aes(label = paste(player_name, " (", general_position, ")", sep = ""))) +
  labs(
    title = 'Money for Nothing: Portland Timbers 2023 Wages x Output',
    subtitle = 'points sized by minutes played',
    y = 'Raw Goals Added',
    x = 'Base Salary ($ 000)'
  )
)

  
gk_ga <- asa_client$get_goalkeeper_goals_added(leagues = 'mls',split_by_season = T, split_by_team = T) |>
  filter(season_name %in% c(2021,2022,2023)) |>
  unnest(data) |>
  unnest(team_id) |>
  left_join(
    all_players|>
      select(player_id, player_name, nationality)
  ) |>
  left_join(
    all_teams |>
      select(1:4)
  )

gk_ga |>
  filter(team_abbreviation == "" |
           player_id == 'KPqjonZyQ6') |>
  group_by(season_name,player_name) |>
  summarize(sum(goals_added_raw))

ga_clean_salaries |>
  filter(general_position == "CB") 
