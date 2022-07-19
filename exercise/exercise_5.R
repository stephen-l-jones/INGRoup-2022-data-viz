#--------------------------------------------------------------------------------------------------
# Exercise 5. Three-pointers and game wins
#
# Directions:
# 1. Load nba_team_game.
# 2. Create bins for every five 3-point attempts and for every 5% of 3-point percentage. For each
#    bin calculate the percentage wins.
# 3. Create a raster plot using geom_raster. Use bin_3pt_attempted for the x-axis and bin_3pt_pct
#    for the y-axis. Set the fill to pct_win.
#--------------------------------------------------------------------------------------------------

# 1.
load("data/nba_team_game.rda") 

# 2.
field_goals_3pt <- nba_team_game %>%
  mutate(bin_3pt_attempted = floor(field_goals_3pt_attempted / 5) * 5,
         bin_3pt_pct = floor((field_goals_3pt_made / field_goals_3pt_attempted) / .05) * .05) %>%
  group_by(bin_3pt_attempted, bin_3pt_pct) %>%
  summarise(pct_win = sum(is_win) / n())

# 3.

