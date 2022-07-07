load("data/nba_team_game.rda")
nba_team_game <- nba_team_game %>% as_tibble()

salary_wins <- nba_team_game %>%
  dplyr::filter(year > 2013) %>%
  group_by(team_code, year) %>%
  summarise(pct_win     = sum(is_win) / max(game_number),
            salary_team = salary_team[1]) %>%
  mutate(season_3 = ifelse(year <= 2016, "2013-16", ifelse(year <= 2019, "2016-19", "2019-22"))) %>%
  as.data.frame()

p <- ggplot(data    = salary_wins, 
            mapping = aes(x = salary_team * 10^-3, 
                          y = pct_win, 
                          color = season_3, 
                          shape = season_3))
print(p)
ggsave("plot/salary_wins (a).png", width = 4.49, height = 4)

p <- p + geom_point()
print(p)
ggsave("plot/salary_wins (b).png", width = 4.49, height = 4)

p <- p + geom_smooth(method = "lm", formula = y ~ x)
print(p)
ggsave("plot/salary_wins (c).png", width = 4.49, height = 4)

p <- p + scale_x_continuous(
  name = "Team salaries (inflation-adjusted, in millions)", 
  expand = c(.02,0),
  breaks = seq(0, 300, 50),
  labels = scales::label_dollar()
) + scale_y_continuous(
  name = NULL, 
  breaks = seq(0, 1, .2), 
  labels = scales::label_percent()
)
print(p)
ggsave("plot/salary_wins (d).png", width = 4.49, height = 4)

p <- p + scale_color_manual(
  name = "Seasons", 
  values = c("#F88C93","#B74F57","#7B071A")
)
print(p)
ggsave("plot/salary_wins (e).png", width = 4.49, height = 4)

p <- p + scale_shape("Seasons")
print(p)
ggsave("plot/salary_wins (f).png", width = 4.49, height = 4)

p <- p + facet_wrap(~ season_3, scales = "free_x")
print(p)
ggsave("plot/salary_wins (g).png", width = 4.49, height = 4)

p <- p + coord_cartesian(ylim = c(.20, .80))
print(p)
ggsave("plot/salary_wins (h).png", width = 4.49, height = 4)

p <- p + labs(title = "Does money buy wins?",
              subtitle = "Percent regular season wins",
              caption  = "Source: NBA Stats, HoopsHype")
print(p)
ggsave("plot/salary_wins (i).png", width = 4.49, height = 4)

p <- p + 
  theme_basic_no_grid() + 
  theme(legend.position = "none")
print(p)
ggsave("plot/salary_wins (j).png", width = 4.49, height = 4)

google_font <- "Crimson Pro"
if (!(google_font %in% sysfonts::font_families())) {
  sysfonts::font_add_google(google_font)
}
p <- p + 
  theme_basic_no_grid(base_family = google_font("Crimson Pro")) + 
  theme(legend.position = "none")
showtext::showtext_auto()
print(p)
ggsave_showtext("plot/salary_wins (k).png", width = 4.49, height = 4)
showtext::showtext_auto(enable = FALSE)

p <- ggplot(data    = salary_wins, 
       mapping = aes(x = salary_team * 10^-3, 
                     y = pct_win, 
                     color = season_3)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x) + 
  scale_x_continuous(
    name = "Team salaries (inflation-adjusted, in millions)", 
    expand = c(.02,0),
    breaks = seq(60, 320, 20),
    labels = label_skip(scales::dollar(seq(60, 320, 20), ), 4)
  ) + 
  scale_y_continuous(
    name = NULL, 
    breaks = seq(0, 1, .05),
    labels = label_skip(scales::percent(seq(0, 1, .05)), 4) 
  ) + 
  scale_color_manual(
    name = "Seasons", 
    values = c("#F88C93","#B74F57","#7B071A")
  ) + 
  scale_shape("Seasons") + 
  facet_wrap(~ season_3, scales = "free_x") + 
  labs(title = "Does money buy wins?",
       subtitle = "Percent regular season wins",
       caption  = "Source: NBA Stats, HoopsHype") +
  theme_basic_no_grid(base_size = 9, 
                      base_family = google_font("Crimson Pro"), 
                      legend.position = "none")
showtext::showtext_auto()
print(p)
ggsave_showtext("plot/salary_wins (l).png", width = 4.49, height = 4)
showtext::showtext_auto(enable = FALSE)
