load("data/nba_team_game.rda")

salary_wins <- nba_team_game %>%
  dplyr::filter(year > 2013) %>%
  group_by(team_code, year) %>%
  summarise(pct_win     = sum(is_win) / max(game_number),
            salary_team = salary_team[1]) %>%
  mutate(season_3 = ifelse(year <= 2016, "2013-16", ifelse(year <= 2019, "2016-19", "2019-22")))

p01 <- ggplot(data    = salary_wins, 
              mapping = aes(x = salary_team * 10^-3, 
                            y = pct_win, 
                            color = season_3, 
                            shape = season_3))
print(p01)
ggsave("plot/nba_wins_01.png", width = 4.49, height = 4)

p02 <- p01 + geom_point()
print(p02)
ggsave("plot/nba_wins_02.png", width = 4.49, height = 4)

p03 <- p02 + geom_smooth(method = "lm", formula = y ~ x)
print(p03)
ggsave("plot/nba_wins_03.png", width = 4.49, height = 4)

p04 <- p03 + scale_x_continuous(name = "Team salaries (inflation-adjusted, in millions)", 
                                expand = c(.02,0),
                                breaks = seq(0, 300, 50),
                                labels = label_dollar()) + 
  scale_y_continuous(name = NULL, 
                     breaks = seq(0, 1, .2), 
                     labels = label_percent())
print(p04)
ggsave("plot/nba_wins_04.png", width = 4.49, height = 4)

p05 <- p04 + scale_color_manual(name = "Seasons", 
  values = c("#F88C93","#B74F57","#7B071A"))
print(p05)
ggsave("plot/nba_wins_05.png", width = 4.49, height = 4)

p06 <- p05 + scale_shape("Seasons")
print(p06)
ggsave("plot/nba_wins_06.png", width = 4.49, height = 4)

p07 <- p06 + facet_wrap(~ season_3, scales = "free_x")
print(p07)
ggsave("plot/nba_wins_07.png", width = 4.49, height = 4)

p08 <- p07 + coord_cartesian(ylim = c(.20, .80))
print(p08)
ggsave("plot/nba_wins_08.png", width = 4.49, height = 4)

p09 <- p08 + labs(title = "Does money buy wins?",
                  subtitle = "Percent regular season wins",
                  caption  = "Source: NBA Stats, HoopsHype")
print(p09)
ggsave("plot/nba_wins_09.png", width = 4.49, height = 4)

p10 <- p09 + 
  theme_basic_no_grid() + 
  theme(legend.position = "none")
print(p10)
ggsave("plot/nba_wins_10.png", width = 4.49, height = 4)

p11 <- p10 + 
  theme_basic_no_grid(base_family = google_font("Crimson Pro")) + 
  theme(legend.position = "none")
showtext_auto()
print(p11)
ggsave_showtext("plot/nba_wins_11.png", width = 4.49, height = 4)
showtext_auto(enable = FALSE)

p12 <- ggplot(data    = salary_wins, 
              mapping = aes(x = salary_team * 10^-3, 
                            y = pct_win, 
                            color = season_3)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x) + 
  scale_x_continuous(name = "Team salaries (inflation-adjusted, in millions)", 
                     expand = c(.02,0),
                     breaks = seq(60, 320, 20),
                     labels = label_skip_dollar(4)) + 
  scale_y_continuous(name = NULL, 
                     breaks = seq(0, 1, .05),
                     labels = label_skip_percent(4)) + 
  scale_color_manual(name = "Seasons", 
                     values = c("#F88C93","#B74F57","#7B071A")) + 
  scale_shape("Seasons") + 
  facet_wrap(~ season_3, scales = "free_x") + 
  labs(title = "Does money buy wins?",
       subtitle = "Percent regular season wins",
       caption  = "Source: NBA Stats, HoopsHype") +
  theme_basic_no_grid(base_family = google_font("Crimson Pro"), 
                      legend.position = "none")

showtext_auto()
print(p12)
ggsave_showtext("plot/nba_wins_12.png", width = 4.49, height = 4)
showtext_auto(enable = FALSE)
