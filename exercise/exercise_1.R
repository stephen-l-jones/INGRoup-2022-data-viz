#--------------------------------------------------------------------------------------------------
# Exercise 1. Effect of salary on regular-season wins
#
# Directions:
# 1. Read the CSV file 'data/nba_salary_wins.csv' and convert salary from thousands to millions.
# 2. Run a linear regression that regresses pct_win on the interaction of salary_team and season_3.
# 3. Use the the predictions() function to create a 2x2 of the predicted interaction when
#    season_3 is "2013-16" or "2019-22". Make sure team_salary is within the relevant range 
#    for season_3.
# 4. Create a line plot using geom_line that shows the interaction of salary_team and season_3.
#    Use plot exercise_1.png in the plot folder as you guide for creating the plot.
#    Have the linetype and color differ based on season_3. Include a 95% confidence interval
#    using geom_ribbon.
#--------------------------------------------------------------------------------------------------

# 1.
wins <- read.csv("data/nba_salary_wins.csv") %>% 
  mutate(salary_team = salary_team * 10^-3) %>%
  as_tibble()

# 2.
wins_lm <- lm(pct_win ~ salary_team * season_3, wins)
summary(wins_lm)

# 3.
wins_prediction <- wins %>% 
  dplyr::filter(season_3 %in% c("2013-16","2019-22")) %>%
  group_by(season_3) %>% 
  summarise(salary_team = seq(min(salary_team), max(salary_team)), length.out = 30) %>%
  predictions(model = wins_lm, newdata = .)

# 4.
# <Create your ggplot() here>
