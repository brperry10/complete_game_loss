####### Sprint Speed Investigation
####### 2.21.24

## Load necessary packages
library(tidyverse)
library(janitor)
library(RColorBrewer)
library(gt)
library(curl)
library(readr)


### Read in data
sprint_speed_15 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2015.csv")
sprint_speed_16 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2016.csv")
sprint_speed_17 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2017.csv")
sprint_speed_18 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2018.csv")
sprint_speed_19 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2019.csv")
sprint_speed_20 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2020.csv")
sprint_speed_21 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2021.csv")
sprint_speed_22 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2022.csv")
sprint_speed_23 <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/sprint_speed_2023.csv")



### Combine data to one dataset
sprint_speeds <- rbind(sprint_speed_15, 
                       sprint_speed_16,
                       sprint_speed_17,
                       sprint_speed_18,
                       sprint_speed_19,
                       sprint_speed_20,
                       sprint_speed_21,
                       sprint_speed_22,
                       sprint_speed_23)


View(sprint_speeds)

### Janitor package to clean up column names
sprint_speeds <- sprint_speeds %>% 
  clean_names() 



########## General Investigation

## average speeds by position
sprint_speeds %>% 
  group_by(position) %>% 
  summarize(mean_ss = mean(sprint_speed),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
  View()


## Aaron Judge speed by year
sprint_speeds %>% 
  filter(last_name_first_name == 'Judge, Aaron') %>% 
  View()


## average speeds by age
sprint_speeds %>% 
  group_by(age) %>% 
  summarize(mean_ss = mean(sprint_speed),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
  View()



### Group players by their max sprint speeds: do faster players see attrition differently than slower?
max_player_speeds <- sprint_speeds %>% 
  group_by(player_id, last_name_first_name) %>% 
  summarize(max_sprint_speed = max(sprint_speed),
            ## rounded_max_sprint_speed = round(max(sprint_speed), 0),
            rounded_max_sprint_speed = round(max(sprint_speed) + .0001, 0)) ##R rounds .5 down oddly, so adding small value to adjust for that

  
  
### Join the data
sprint_speeds %>% 
  left_join(max_player_speeds) %>% 
  group_by(age, rounded_max_sprint_speed) %>% 
  summarize(mean_ss = mean(sprint_speed),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
  View()



### Quick linear model: players lose ~.16 ft/sec w/ each year of age on average
model <- lm(sprint_speed ~ age, data = sprint_speeds)
summary(model)



####### Visualizations

### Visualization 1: Sprint speeds overtime grouped by player max speed
sprint_speeds %>% 
  left_join(max_player_speeds) %>% 
  group_by(age, rounded_max_sprint_speed) %>% 
  summarize(mean_ss = mean(sprint_speed),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
  ggplot(aes(age, median_ss, group = rounded_max_sprint_speed, color = as.factor(rounded_max_sprint_speed))) +
    geom_line(size = 1.3) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(title = "Sprint Speed Attrition by Peak Player Speed",
         subtitle = "Qualifying Statcast player seasons (10+ observations) from 2015 - 2023",
         x = "Age",
         y = "Median Sprint Speed (rounded to nearest ft/sec)",
         color = "Rounded Max Sprint Speed",
         caption = "Source: Statcast 
       
                  CompleteGameLoss ") +
    theme_light()



#### Visualization 2: Player sprint speed year to year
sprint_speeds %>% 
  mutate(age = if_else(age > 38, 39, age)) %>% 
  group_by(age) %>% 
  summarize(mean_ss = mean(sprint_speed),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
  ggplot(aes(age, median_ss, size = count_player_seasons)) +
  geom_point(color = "Maroon") +
  geom_line(size = 0.5, color = "maroon") +
  labs(title = "Player-Season Sprint Speed & Age",
       subtitle = "Qualifying Statcast player seasons (10+ observations) from 2015 - 2023 \n \nplayer seasons w/ age >38 all assigned value of 39 given small samples & noise",
       x = "Age",
       y = "Median Sprint Speed",
       color = "Total Player Seasons",
       caption = "Source: Statcast 
       
                  CompleteGameLoss ") +
  theme_light()



#### Adjusting Data
sprint_speeds %>% 
  filter(position == "CF") %>% 
  mutate(age = if_else(age > 38, 39, age)) %>% 
  group_by(age) %>% 
  summarize(mean_ss = mean(sprint_speed),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
  ggplot(aes(age, median_ss, size = count_player_seasons)) +
  geom_point(color = "Maroon") +
  geom_line(size = 0.5, color = "Maroon") +
  labs(title = "Player-Season Sprint Speed & Age",
       subtitle = "Player-season data: 2015-2023",
       color = "Total Player Seasons",
       x = "Age",
       y = "Median Sprint Speed") +
  theme_light()



### Visualization 3: Biggest dropoffs in speed
sprint_speeds %>% 
  group_by(last_name_first_name) %>% 
  summarize(
    min_speed = min(sprint_speed),
    max_speed = max(sprint_speed),
    speed_difference = max_speed - min_speed,
    number_qualifying_seasons = n(),
    min_age = min(age),
    max_age = max(age)
  ) %>% 
  arrange(desc(speed_difference)) %>% 
  filter(number_qualifying_seasons > 1) %>% 
  head(15) %>% 
  gt() %>% 
  tab_header(
    title = "Biggest Cases of Sprint Speed Attrition",
    subtitle = "Statcast qualifying sprint speeds 2015-2023"
  ) %>% 
  tab_source_note(
    source_note = "Source: Statcast"
  ) %>% 
  tab_source_note(
    source_note = "CompleteGameLoss"
  ) %>% 
  cols_label(
    last_name_first_name = "Name",
    min_speed = "Min Season Speed",
    max_speed = "Max Season Speed",
    speed_difference = "Max - Min Diff.",
    number_qualifying_seasons = "Number Qualifying Seasons",
    min_age = "Min Age",
    max_age = "Max Age"
  ) %>% 
  data_color(
    columns = vars(speed_difference),
    colors = scales::col_numeric(
      palette = c('white', 'maroon'),
      domain = c(2.5, 4)
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = c(-last_name_first_name)
  )



### Visualization 4: speed by position
## average speeds by position
sprint_speeds %>% 
  group_by(position) %>% 
  summarize(mean_ss = round(mean(sprint_speed), 2),
            median_ss = median(sprint_speed),
            count_player_seasons = n()) %>% 
gt() %>% 
  tab_header(
    title = "Measures of Center of Sprint Speed by Player Position",
    subtitle = "Statcast qualifying sprint speeds 2015-2023"
  ) %>% 
  cols_label(
    position = "Position",
    mean_ss = "Mean",
    median_ss = "Median",
    count_player_seasons = "Total Player Seasons"
  ) %>% 
  tab_source_note(
    source_note = "Source: Statcast"
  ) %>% 
  tab_source_note(
    source_note = "CompleteGameLoss"
  ) %>% 
  data_color(
    columns = vars(median_ss),
    colors = scales::col_numeric(
      palette = c('white', 'maroon'),
      domain = c(25, 29)
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = c(position, mean_ss, median_ss, count_player_seasons)
  )
