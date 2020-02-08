library(tidyverse)
# Get the Data
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

attend <- attendance %>% mutate(team_name_long = paste(team, team_name))

games <- games %>% mutate(week = as.integer(week))
attend %>% left_join(games, by = c("year", "team_name_long" = "home_team", "week")) %>%
  mutate(team_name_long = fct_reorder(team_name_long, weekly_attendance, .fun='median')) %>%
  ggplot(aes(team_name_long, weekly_attendance)) + geom_boxplot() + coord_flip()

games_attendance <- attend %>% left_join(games, by = c("year", "team_name_long" = "home_team", "week")) %>% 
  filter(!is.na(weekly_attendance)) %>%
  mutate(team_name_long = fct_reorder(team_name_long, weekly_attendance))
games_attendance %>% ggplot(aes(team_name_long, weekly_attendance)) + geom_boxplot() + coord_flip()
attend %>% left_join(games, by = c("year", "team_name_long" = "home_team", "week")) %>% 
  filter(is.na(weekly_attendance))


