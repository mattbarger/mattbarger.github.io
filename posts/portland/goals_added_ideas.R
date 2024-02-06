library(tidyverse)

mls_summary <- read_csv('https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_advanced_match_stats/USA_M_1st_passing_team_advanced_match_stats.csv')


mls_summary |>
  filter(Season_End_Year == 2023) |>
  mutate(Opponent = ifelse(Team == Home_Team, Home_Team, Away_Team))


asa_client <- AmericanSoccerAnalysis$new()
# Access dataframes of games, players, teams, and more from the ASA client object created above
all_teams <- asa_client$teams
all_players <- asa_client$players

asa_goals_added <- asa_client$get_player_goals_added(
  leagues = "mls",
  split_by_season = T,
  split_by_team = T
)



asa_goals_added |> 
  left_join(all_players) |> 
  left_join(all_teams, by = 'team_id') |> 
  filter(season_name == 2023)  |> 
  unnest(data) |> 
  filter(team_abbreviation == "POR" | player_name == "Kamal Miller" & team_abbreviation != "MTL") |> 
  select(player_name, general_position, minutes_played, action_type, goals_added_raw) |> 
  pivot_wider(names_from = action_type, values_from = goals_added_raw) |>
  mutate(Goals_Added = Dribbling + Receiving + Fouling + Passing + Shooting + Interrupting,
         GA_team = Goals_Added - Interrupting) |>
  mutate(across(Dribbling:GA_team,
                list(p96 = ~(96 * .x) /minutes_played))) |> arrange(-Goals_Added_p96)


team_goals_added_zone <- tibble()

for(i in 1:30) {
  tmp <- asa_client$get_team_goals_added(
    leagues = "mls", split_by_seasons = T, zone = i) |>
    unnest(data) |>
    mutate(zone = i)
  
  team_goals_added_zone<- bind_rows(team_goals_added_zone, tmp)
  rm(tmp)
}



gaa_zone_lg <- team_goals_added_zone |>
  filter(season_name == '2023') |>
  left_join(all_teams |> select(-competitions), by = "team_id") |> 
  select(-num_actions_for, -goals_added_for, -num_actions_against) |>
  pivot_wider(values_from = goals_added_against, names_from = action_type, values_fill = 0) |>
  mutate(team_goals_added_against = Fouling + Receiving + Passing + Dribbling + Shooting,
         tgaa_96 = 96 * team_goals_added_against/minutes) |>
  group_by(zone) |>
  summarize(lg_avg_gaa_96 = mean(tgaa_96),
            lg_avg_gaa = mean(team_goals_added_against))


stats_per_zone <- team_goals_added_zone |>
  filter(season_name == '2023') |>
  left_join(all_teams |> select(-competitions), by = "team_id") |>
  select(-num_actions_for, -goals_added_for, -num_actions_against) |>
  pivot_wider(values_from = goals_added_against, names_from = action_type, values_fill = 0) |>
  mutate(team_goals_added_against = Fouling + Receiving + Passing + Dribbling + Shooting,
         tgaa_96 = 96 * team_goals_added_against/minutes) |>
  group_by(team_abbreviation, zone) |>
  summarize(avg_gaa_96 = mean(tgaa_96),
            avg_gaa = mean(team_goals_added_against)) |>
  left_join(gaa_zone_lg) |>
  mutate(gaa_vs_lg_avg = lg_avg_gaa - avg_gaa,
         gvla_96 = lg_avg_gaa_96 - avg_gaa_96) |>
  ungroup() |>
  group_by(zone) |>
  summarize(mean_gaa = mean(gaa_vs_lg_avg), sd_gaa = sd(gaa_vs_lg_avg))

  
team_goals_added_zone |>
  filter(season_name == '2023') |>
  left_join(all_teams |> select(-competitions), by = "team_id") |>
  select(-num_actions_for, -goals_added_for, -num_actions_against) |>
  pivot_wider(values_from = goals_added_against, names_from = action_type, values_fill = 0) |>
  mutate(team_goals_added_against = Fouling + Receiving + Passing + Dribbling + Shooting,
         tgaa_96 = 96 * team_goals_added_against/minutes) |>
  group_by(team_abbreviation, zone) |>
  summarize(avg_gaa_96 = mean(tgaa_96),
            avg_gaa = mean(team_goals_added_against)) |>
  left_join(gaa_zone_lg) |>
  mutate(gaa_vs_lg_avg = lg_avg_gaa - avg_gaa,
         gvla_96 = lg_avg_gaa_96 - avg_gaa_96) |>
  left_join(stats_per_zone) |>
  mutate(sd_above_mean = (gaa_vs_lg_avg - mean_gaa)/sd_gaa) |>
  filter(team_abbreviation == "POR")
  
  
  