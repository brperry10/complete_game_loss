###### Investigation of triples; who is going for 3B?
###### 12.12.23 ######

# Load packages and data --------------------------------------------------
library(tidyverse)
library(scales)

# data from multiple sources, compiled in gsheets
# statcast for sprint speed, baseball ref for player age by season, fangraphs for park effects
data <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_age_and_triples.csv")


## add field for 3B park factor relative to 2B park factor
data <- data %>% 
  mutate(triples_by_doubles = `3B...22`/`2B...23`)

View(data)



data %>% 
  group_by(Age) %>% 
  summarize(
    mean = mean(triples_over_xbh),
    median = median(triples_over_xbh),
    n = n()
  ) %>% 
  ggplot(aes(Age, median)) +
  geom_point() +
  theme_light()

lm(triples_over_xbh ~ Age + triples_by_doubles + `Sprint Speed`, data = data) %>% 
  summary()

lm(triples_over_xbh ~ Age, data = data) %>% 
  summary()



#### Visualize
data %>% 
  ggplot(aes(Age, triples_over_xbh)) +
  geom_jitter(size = 2.5, width = 0.25, color = "Maroon", alpha = 0.5) +
  geom_smooth(method = "lm", color = "Turquoise", se = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Batter Age versus Triples as a Percentage of Non-Home Run Extra Base Hits", 
       subtitle = "Qualifying Batter Seasons, 2021 - 2023", 
       x = "Batter Age",
       y = "3B / (2b + 3B)", 
       caption = "Sources: FanGraphs (park factors), baseballreference.com (player ages & XBHs), Statcast (sprint speed) 
       
                  CompleteGameLoss ") +
  theme_light()
