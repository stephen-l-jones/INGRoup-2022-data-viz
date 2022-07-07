load("data/nba_player_game.rda")
nba_player_game <- nba_player_game %>% as_tibble()

nba_team_game <- nba_player_game %>%
  group_by(team_code, year) %>%
  mutate(game_n      = max(game_number),
         salary_team = sum(unique(data.frame(player_name, inflation_adj_salary), 
                                  by = "player_name")[, "inflation_adj_salary"] * 10^-3, 
                           na.rm = TRUE)) %>%
  group_by(team_code, year, game_number) %>%
  summarise(
    season        = as.factor(season[1]),
    game_date     = game_date[1],
    team_name     = team_name[1],
    game_location = game_location[1],
    matchup       = matchup[1],
    opponent_code = as.factor(opponent_code[1]),
    is_win        = is_win[1],
    field_goals_made          = sum(field_goals_made),
    field_goals_attempted     = sum(field_goals_attempted),
    field_goals_2pt_made      = sum(field_goals_2pt_made),
    field_goals_2pt_attempted = sum(field_goals_2pt_attempted),
    field_goals_3pt_made      = sum(field_goals_3pt_made),
    field_goals_3pt_attempted = sum(field_goals_3pt_attempted),
    free_throws_made          = sum(free_throws_made),
    free_throws_attempted     = sum(free_throws_attempted),
    offensive_rebounds = sum(offensive_rebounds),
    defensive_rebounds = sum(defensive_rebounds),
    assists            = sum(assists),
    steals             = sum(steals),
    blocks             = sum(blocks),
    turnovers          = sum(turnovers),
    personal_fouls     = sum(personal_fouls),
    points             = sum(points),
    salary_team     = salary_team[1],
    salary_gini     = dineq::gini.wtd(inflation_adj_salary[!is.na(salary)], minutes[!is.na(salary)]),
    salary_on_court = 
      sum(inflation_adj_salary[!is.na(salary)] / game_n[1] * minutes[!is.na(salary)]) / 
      (sum(minutes[!is.na(salary)]) / 5) * 10^-3
  )

save(nba_team_game, file = "data/nba_team_game.rda")

