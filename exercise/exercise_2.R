#--------------------------------------------------------------------------------------------------
# Exercise 2. Salary spend and winning over time for select teams
#
# Directions:
# 1. Read the CSV file 'data/nba_salary_wins.csv' and convert salary from thousands to millions,
#    and highlight the Lakers (LAL), Cavaliers (CLE), and Phoenix Suns (PHX).
# 2. Create a connected scatterplot. Place pct_win on the y-axis and salary_team on the x-axis 
#    and create a path using geom_path that connects year 2014 through 2022. Give each focus team 
#    a different color. Use gray and a smaller size for the other teams. Use plot exercise_2.png 
#    as a guide.
#--------------------------------------------------------------------------------------------------

# 1.
wins_path <- read.csv("data/nba_salary_wins.csv") %>% 
  mutate(salary_team = salary_team * 10^-3,
         team_label = factor(
           ifelse(team_code %in% c("LAL","CLE","PHX"), team_name, "Other"),
           c("Cleveland Cavaliers","Los Angeles Lakers","Phoenix Suns","Other")
         )) %>%
  arrange(team_label, year) %>%
  as_tibble()

# 2.
# <Create your ggplot() here>




