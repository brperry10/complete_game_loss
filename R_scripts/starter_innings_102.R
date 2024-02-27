####### How many starters do teams use in a season? ########
####### 5.19.23


# Read in Data & Load Packages --------------------------------------------


## Packages
library(tidyverse)
library(Lahman)

## read in data
data <- read_csv("/Users/benperry/Desktop/CGLoss/CSVs/102_starts_2015_2022.csv")
View(data)

## Check out data
## Concatenate Team and Season then count distinct starters
data %>% 
  unite(team_season, c("Team", "Season")) %>% 
  count(n_distinct(team_season))

## Team Seasons with the most distinct SP
data %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  summarize(count_pitchers = n()) %>% 
  arrange(desc(count_pitchers))




# Visualizations ----------------------------------------------------------



## Viz 1: Starting Pitchers per Team Season
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  summarize(count_pitchers = n()) %>% 
  ggplot(aes(count_pitchers)) +
  geom_histogram(binwidth = 1, color = "orange", fill = "maroon", alpha = I(0.7)) +
  labs(title = "Number of Starting Pitchers per Team Season", 
       subtitle = "2015 - 2022 Seasons, Excluding 2020", 
       x = "",
       y = "", 
       caption = "Source: FanGraphs 
       
                  CompleteGameLoss ") +
  theme_light()


## List of playoff teams to join to team-seasons data
playoff_teams <- Lahman::BattingPost %>% 
  filter(yearID > 2014,
         yearID != 2020) %>% 
  mutate(teamID_adj = 
           case_when(
             teamID == "NYA" ~ 'NYY',
             teamID == "CHA" ~ "CHW",
             teamID == "CHN" ~ "CHC",
             teamID == "NYN" ~ "NYM",
             teamID == "LAN" ~ "LAD",
             teamID == "KCA" ~ "KCR",
             teamID == "TBA" ~ "TBR",
             teamID == "SFN" ~ "SFG",
             teamID == "SDN" ~ "SDP",
             teamID == "WAS" ~ "WSN",
             teamID == "SLN" ~ "STL",
             .default = teamID
           )) %>% 
  unite(team_season, c("teamID_adj", "yearID")) %>% 
  distinct(team_season) %>% 
  mutate(playoff_team = 1) 


Lahman::Teams %>% 
  filter(yearID > 2014) %>% 
  distinct(teamID)


## Viz 2: Number starting pitchers by team-season, grouped by playoffs status
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  left_join(playoff_teams, by = "team_season") %>% 
  mutate(made_playoffs = !is.na(playoff_team)) %>% 
  group_by(team_season, made_playoffs) %>% 
  summarise(count_pitchers = n()) %>% 
  ggplot(aes(count_pitchers, fill = made_playoffs)) +
  geom_histogram(binwidth = 1, color = "turquoise") +
  scale_fill_manual(values = alpha(c("maroon", "orange"), 0.8)) +
  labs(title = "Number of Starting Pitchers per Team Season", 
       subtitle = "Grouped by Playoff Status; 2015 - 2022 Seasons, Excluding 2020", 
       x = "",
       y = "",
       fill = "Made Playoffs",
       caption = "Source: FanGraphs 
       
                  CompleteGameLoss ") +
  theme_light()





#### Top 5 Pitchers breakdown
# IP by Top 5 Pitchers for each team-season - not running
# data %>% 
#   filter(Season != 2020) %>% 
#   unite(team_season, c("Team", "Season")) %>% 
# #  mutate(ip_adj = case_when(colnames(IP), ends_with('.1') ~ ip - .1 + .33,
# #                            colnames(IP), ends_with('.2') ~ ip - .2 + .66) %>%
#   group_by(team_season) %>% 
#   arrange(ip) %>% 
#   mutate(ip_rank = row_number()) %>% 
#   View()


## top 5 IP, not accounting for .1/.2 IP
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  arrange(desc(IP)) %>% 
  mutate(ip_rank = row_number()) %>% 
  filter(ip_rank < 6) %>% 
  select(team_season, IP) %>% 
  summarise(top_5_ip = sum(IP)) %>% 
  View()



## Viz 3: top 5 IP, not accounting for .1/.2 IP
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  arrange(desc(IP)) %>% 
  mutate(ip_rank = row_number()) %>% 
  filter(ip_rank < 6) %>% 
  select(team_season, IP) %>% 
  summarise(top_5_ip = sum(IP)) %>% 
  ggplot(aes(top_5_ip)) +
  geom_histogram(binwidth = 20, color = "orange", fill = "maroon", alpha = I(0.7)) +
  labs(title = "Number of Innings Pitched by Top 5 IP Pitchers per Team Season", 
       subtitle = "2015 - 2022 Seasons, Excluding 2020", 
       x = "",
       y = "", 
       caption = "Source: FanGraphs 
       
                  CompleteGameLoss ") +
  theme_light()
  





## Viz 4: Top 5 IP pitchers, grouped by playoffs
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  left_join(playoff_teams, by = "team_season") %>% 
  mutate(made_playoffs = !is.na(playoff_team)) %>% 
  arrange(desc(IP)) %>% 
  mutate(ip_rank = row_number()) %>% 
  filter(ip_rank < 6) %>% 
  select(team_season, IP, made_playoffs) %>% 
  group_by(team_season, made_playoffs) %>% 
  summarise(top_5_ip = sum(IP)) %>% 
  ggplot(aes(top_5_ip, fill = made_playoffs)) +
  geom_histogram(binwidth = 20, color = "turquoise") +
  scale_fill_manual(values = alpha(c("maroon", "orange"), 0.7)) +
  labs(title = "Number of Innings Pitched by Top 5 IP Pitchers per Team Season", 
       subtitle = "Grouped by Playoff Status; 2015 - 2022 Seasons, Excluding 2020", 
       x = "",
       y = "", 
       fill = "Made Playoffs",
       caption = "Source: FanGraphs 
       
                  CompleteGameLoss ") +
  theme_light()




# Other Misc. Calculations ------------------------------------------------


## teams w/ fewest 1-5 starter IP to make playoffs
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  left_join(playoff_teams, by = "team_season") %>% 
  mutate(made_playoffs = !is.na(playoff_team)) %>% 
  arrange(desc(IP)) %>% 
  mutate(ip_rank = row_number()) %>% 
  filter(ip_rank < 6) %>% 
  select(team_season, IP, made_playoffs) %>% 
  group_by(team_season, made_playoffs) %>% 
  summarise(top_5_ip = sum(IP)) %>% 
  filter(made_playoffs) %>% 
  arrange(top_5_ip)



## teams w/ fewest 1-5 starter IP to make playoffs
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  left_join(playoff_teams, by = "team_season") %>% 
  mutate(made_playoffs = !is.na(playoff_team)) %>% 
  arrange(desc(IP)) %>% 
  mutate(ip_rank = row_number()) %>% 
  filter(ip_rank < 6) %>% 
  select(team_season, IP, made_playoffs) %>% 
  group_by(team_season, made_playoffs) %>% 
  summarise(top_5_ip = sum(IP)) %>% 
  group_by(made_playoffs) %>% 
  summarise(count = n(),
            avg = mean(top_5_ip))


data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  summarise(count_number = n()) %>% 
  left_join(playoff_teams, by = "team_season") %>% 
  mutate(made_playoffs = !is.na(playoff_team)) %>% 
  select(team_season, made_playoffs, count_number) %>% 
  group_by(made_playoffs) %>% 
  summarise(avg_starters = mean(count_number),
            n = n())






#### Working on getting Percentages
data %>% 
  filter(Season != 2020) %>% 
  unite(team_season, c("Team", "Season")) %>% 
  group_by(team_season) %>% 
  left_join(playoff_teams, by = "team_season") %>% 
  mutate(made_playoffs = !is.na(playoff_team)) %>% 
  group_by(team_season, made_playoffs) %>% 
  summarise(count_pitchers = n()) %>% 
  group_by(count_pitchers) %>% 
  summarise(pct_playoffs = if_else(made_playoffs, 1, 0)/n())
