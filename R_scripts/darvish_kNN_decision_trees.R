######### Yu Darvish Pitch Classifications ############
## 6.21


# Load libraries & Data ---------------------------------------------------

library(tidyverse)
library(broom)
library(ggrepel)
# Crosstable
library(psych)
# rolling average
library(zoo)
# as a tibble
#darvish <- read_csv("/Users/benperry/Desktop/CGLoss/CSVs/Darvish2019.csv")
darvish <- read_csv("https://raw.githubusercontent.com/brperry10/complete_game_loss/main/CSVs/darvish_statcast_2019.csv")
summary(darvish)



# Clean Data --------------------------------------------------------------

darvish %>% 
  select(pitch_name, # pitch type, variable to be predicted
         release_speed, # Out of hand velocity
         release_pos_x, # Horizontal Release Position of the ball measured in feet from the catcher's perspective
         release_pos_z, # Vertical Release Position of the ball measured in feet from the catcher's perspective
         plate_x, # Horizontal position of the ball when it crosses home plate from the catcher's perspective.
         plate_z, # Vertical position of the ball when it crosses home plate from the catcher's perspective.
         pfx_x, # Horizontal movement in feet from the catcher's perspective.
         pfx_z, # Vertical movement in feet from the catcher's perspective.
         release_spin_rate, # Spin rate of pitch tracked by Statcast.
         release_extension, # Release extension of pitch in feet as tracked by Statcast.
         spin_axis, # The Spin Axis in the 2D X-Z plane in degrees from 0 to 360, such that 180 represents 
                    # a pure backspin fastball and 0 degrees represents a pure topspin (12-6) curveball
         stand # Side of the plate batter is standing.
         ) %>% 
  filter(pitch_name != 'Changeup') %>% 
  filter(pitch_name != 'Eephus') %>% 
  filter(!is.na(release_spin_rate)) -> darvish # 14 pitches dropped overall, full set is 2834 observations

# might need to add row IDs, will need to randomize row order
View(darvish)


# kNN  --------------------------------------------------------------------

# Rearrange rows of dataset in R so "hold out" method takes a random smaple versus chronological
darvish <- as.data.frame(darvish)
set.seed(42)
rows <- sample(nrow(darvish))
darvish <- darvish[rows, ]



## Normalize numeric fields
darvish2 <- darvish %>% 
  mutate(release_speed_normal = (release_speed - min(release_speed)) / (max(release_speed) - min(release_speed)),
         release_pos_x_normal = (release_pos_x - min(release_pos_x)) / (max(release_pos_x) - min(release_pos_x)),
         release_pos_z_normal = (release_pos_z - min(release_pos_z)) / (max(release_pos_z) - min(release_pos_z)),
         plate_x_normal = (plate_x - min(plate_x)) / (max(plate_x) - min(plate_x)),
         plate_z_normal = (plate_z - min(plate_z)) / (max(plate_z) - min(plate_z)),
         pfx_x_normal = (pfx_x - min(pfx_x)) / (max(pfx_x) - min(pfx_x)),
         pfx_z_normal = (pfx_z - min(pfx_z)) / (max(pfx_z) - min(pfx_z)),
         release_spin_rate_normal = (release_spin_rate - min(release_spin_rate)) / (max(release_spin_rate) - min(release_spin_rate)),
         release_extension_normal = (release_extension - min(release_extension)) / (max(release_extension) - min(release_extension)),
         spin_axis_normal = (spin_axis - min(spin_axis)) / (max(spin_axis) - min(spin_axis)) 
)

View(darvish2)

# table to check pitch counts
table(darvish2$pitch_name) 


# 2512 observations after removing NAs
# Split the dataset into test and training subsets, only use fields that have been normalized (and drop outcome variable)
game_data_train <- darvish2[1:2261, ] 
game_data_train <- game_data_train[, 13:22]
nrow(game_data_train)
game_data_test <- darvish2[2262:2512, ]
game_data_test <- game_data_test[, 13:22]
nrow(game_data_test)


# Create vectors to test against, which only include the 
game_data_train_labels <- darvish2[1:2261, ] 
# game_data_train_labels <- game_data_train_labels[ , 1]
nrow(game_data_train_labels)

game_data_test_labels <- darvish2[2262:2512, ]
# game_data_test_labels[, 1] 
nrow(game_data_test_labels)

library(class)


dim(game_data_train)
dim(game_data_test)
dim(game_data_train_labels)
dim(game_data_test_labels)
length(game_data_train)
length(game_data_test)

# use knn function from class package
# needed to ensure the cl was in fact a vector: $AutoPitchType fixed the issue
# Originally used K as 15, w/ 92% accuracy, k as 13 led to 95% accuracy
pitch_pred <- knn(train = game_data_train, test = game_data_test, cl = game_data_train_labels$pitch_name, k = 50)
length(pitch_pred)
# install gmodels package in order to create cross tabs
library(gmodels)

CrossTable(x = game_data_test_labels$pitch_name, y = pitch_pred,
           prop.chisq = FALSE)



### 92% accuracy



# C5.0 Decision Tree Model ------------------------------------------------
library(C50)

## Randomize dataset
darvish <- as.data.frame(darvish)
set.seed(42)
rows <- sample(nrow(darvish))
darvish <- darvish[rows, ]



## Create the hold-out data sets
decision_tree_train <- darvish[1:2261, 1:11] 
View(decision_tree_train)

decision_tree_test <- darvish[2262:2512, 1:11]
View(decision_tree_test)


## change randomized first column to a factor
class(decision_tree_train$pitch_name)
decision_tree_factor <- factor(decision_tree_train$pitch_name)
class(decision_tree_factor)


View(decision_tree_train)

# Train Model
decision_tree_model <- C5.0(decision_tree_train[-1], decision_tree_factor)
decision_tree_model
summary(decision_tree_model) ## correctly (finally) number of estimates

##### Test model performance

## change randomized first column to a factor
class(decision_tree_test$pitch_name)
decision_tree_factor_test <- factor(decision_tree_test$pitch_name)
class(decision_tree_factor_test)


decision_tree_pred <- predict(decision_tree_model, decision_tree_test)

CrossTable(x = decision_tree_test$pitch_name, y = decision_tree_pred,
           prop.chisq = FALSE)

## 97% Accuracy


## Create a simple tuned model
install.packages("caret")
library(caret)

RNGversion("3.5.2")
set.seed(42)
install.packages("e1071")
library(e1071)

m <- train(pitch_name ~ ., data = decision_tree_train, method = "C5.0")
m

p <- predict(m, decision_tree_train)

table(p, decision_tree_train$pitch_name)
