#--------------------------------------------------------------------------------------------------
# Exercise 3. Histogram of team salary spending
#
# Directions:
# 1. Read the CSV file 'data/nba_salary_wins.csv' and convert salary from thousands to millions
#    and filter the data to season_3 equal to "2013-16" or "2019-22".
# 2. Create a histogram of salary_team. Facet the data by season_3. Use plot exercise_3.png 
#    as a guide.
#--------------------------------------------------------------------------------------------------

# 1.
salary <- read.csv("data/nba_salary_wins.csv") %>% 
  mutate(salary_team = salary_team * 10^-3) %>%
  dplyr::filter(season_3 %in% c("2013-16", "2019-22")) %>%
  as_tibble()

# 2.
# <Create your ggplot() here>
