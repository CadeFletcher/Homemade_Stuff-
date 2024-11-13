## End of Year Project
library(dplyr)
library(ggplot2)
library(reshape2)

install.packages("rpart")
library(rpart)

install.packages("randomForest")
library(randomForest)

install.packages('rlang')
library(rlang)

install.packages('vctrs')
library(vctrs)
install.packages('tibble')
library(tibble)
install.packages('caret')
library(caret) 

install.packages('gbm')
library(gbm)



pitch_data <- data.frame(read.csv("C:/Users/fletc/Downloads/pitch_movement (1).csv"))
run_value_data <- data.frame(read.csv("C:/Users/fletc/Downloads/pitch-arsenal-stats (1).csv"))

colnames(pitch_data)[4] <- 'player_id' 

merged_df <- merge(pitch_data, run_value_data, by = c('player_id', 'pitch_type'))


merged_df <- merged_df %>% select(-one_of('last_name.y',
                             'first_name.y',
                             'team_name_alt',
                             'pitch_name',
                             'tail'))

head(merged_df)
summary(merged_df)


## The harder the pitch is thrown the less it will drop as it approaches the plate.
ggplot(data = merged_df, aes(x = avg_speed, y = pitcher_break_z)) +
  geom_point()

ggplot(data = merged_df, aes(x = pitcher_break_z, y = run_value_per_100)) +
  geom_point() +
  geom_smooth()

ggplot(data = merged_df, aes(x = pitcher_break_x, y = run_value_per_100)) +
  geom_point() +
  geom_smooth()

ggplot(data = merged_df, aes(x = avg_speed, y = run_value_per_100)) +
  geom_point() +
  geom_smooth()

## What are run values across league for all pitch types
ggplot(data = merged_df, aes(x = pitch_type_name, y = run_value_per_100)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))


numeric_columns <- c("avg_speed", "pitcher_break_z", "pitcher_break_x", "run_value_per_100", "whiff_percent", 'est_slg', 'hard_hit_percent')
cor_matrix <- cor(merged_df[, numeric_columns])

cor_data <- reshape2::melt(cor_matrix)

ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Plot", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90))


cor_matrix
## Noteworthy findings, whiff percentage and speed of pitch are inversely correlated
# Hard hit percentage goes down as pitcher break z goes up


unique(merged_df$pitch_type_name)



Fastballs <- merged_df[merged_df$pitch_type_name == '4-Seamer' | merged_df$pitch_type_name == 'Sinker' | 
                         merged_df$pitch_type_name == 'Cutter',]

#Removing Tyler Rogers weird submarine fastball
Fastballs <- Fastballs[Fastballs$pitcher_break_z < 50,]


ggplot(data = Fastballs, aes(x = pitcher_break_z, y = run_value_per_100, color = pitcher_break_x)) +
  geom_point()

OffSpeed <- merged_df[merged_df$pitch_type_name == 'Changeup' |
                        merged_df$pitch_type_name == 'Splitter',]

ggplot(data = OffSpeed, aes(pitcher_break_x, y = run_value_per_100, color = avg_speed)) +
  geom_point()

BreakingBall <- merged_df[merged_df$pitch_type_name == 'Curveball' |
                            merged_df$pitch_type_name == 'Slider' |
                            merged_df$pitch_type_name == 'Sweeper' |
                            merged_df$pitch_type_name == 'Slurve',]

ggplot(data = BreakingBall, aes(x = avg_speed, y = run_value_per_100, color = pitcher_break_z)) +
  geom_point()







selected_cols <- c("pitcher_break_x", "pitcher_break_z", "avg_speed", "run_value_per_100")
df_selected <- Fastballs[, selected_cols]

### Train and Testing Sets
set.seed(95)  
train_indices <- createDataPartition(df_selected$run_value_per_100, p = 0.7, list = FALSE)
train_data <- df_selected[train_indices, ]
test_data <- df_selected[-train_indices, ]

train_X <- train_data[, c("pitcher_break_x", "pitcher_break_z", "avg_speed")]
train_Y <- train_data$run_value_per_100

test_X <- test_data[, c("pitcher_break_x", "pitcher_break_z", "avg_speed")]
test_Y <- test_data$run_value_per_100



## Models
rf_model <- randomForest(train_X, train_Y)
rf_predict <- predict(rf_model, newdata = test_X)
summary(rf_predict)

RMSE(test_Y, rf_predict)

## Each prediction of the random forest model was off by about 2 run values per 100. Not that great let's see if we can find a different model

gbm_model <- gbm(train_Y ~ ., data = train_X, n.trees = 100)

gbm_predictions <- predict(gbm_model, newdata = test_X)

gbm_predictions

RMSE(test_Y, gbm_predictions)

## A little improvement upon the random forest one


## Early stopping to determine optimal number of trees for gbm
train_data <- data.frame(train_X, train_Y)

set.seed(95)  
trainIndex <- createDataPartition(train_data$train_Y, p = 0.7, list = FALSE)
train_set <- train_data[trainIndex, ]
validation_set <- train_data[-trainIndex, ]

FastballGBM <- gbm(
  train_Y ~ .,  
  data = train_set,  
  n.trees = 1000,  
  verbose = FALSE,  
  distribution = "gaussian",  
  shrinkage = 0.01,  
  cv.folds = 10,  
  keep.data = TRUE  
)

validation_predictions <- predict(FastballGBM, newdata = validation_set, n.trees = gbm_model$best.iter)


FastballRMSE <- RMSE(validation_predictions, test_Y)

## Gradient boosted model seems to be our best bet, lets use it on breaking balls and offspeed now

df_selected2 <- BreakingBall[, selected_cols]
set.seed(96)  
train_indices2 <- createDataPartition(df_selected2$run_value_per_100, p = 0.7, list = FALSE)
BBtrain_data <- df_selected2[train_indices2, ]
BBtest_data <- df_selected2[-train_indices2, ]

BBtrain_X <- BBtrain_data[, c("pitcher_break_x", "pitcher_break_z", "avg_speed")]
BBtrain_Y <- BBtrain_data$run_value_per_100

BBtest_X <- BBtest_data[, c("pitcher_break_x", "pitcher_break_z", "avg_speed")]
BBtest_Y <- BBtest_data$run_value_per_100


set.seed(85)
BBtrainIndex <- createDataPartition(BBtrain_data$run_value_per_100, p = 0.7, list = FALSE)
BBtrain_set <- BBtrain_data[BBtrainIndex, ]
BBvalidation_set <- BBtrain_data[-BBtrainIndex, ]

BBtrain_data <- data.frame(BBtrain_X, BBtrain_Y)


BBgbm_model <- gbm(
  BBtrain_Y ~ .,  
  data = BBtrain_data,  
  n.trees = 1000,  
  verbose = FALSE,  
  distribution = "gaussian",  
  shrinkage = 0.01,  
  cv.folds = 10,  
  keep.data = TRUE  
)

BBvalidation_predictions <- predict(BBgbm_model, newdata = BBvalidation_set, n.trees = BBgbm_model$best.iter)

summary(BBvalidation_predictions)

BB_RMSE <- RMSE(BBvalidation_predictions, BBtest_Y)

## Again about 1.7 on our RMSE, last is all offspeed pitches





df_selected3 <- OffSpeed[, selected_cols]
set.seed(97)  
train_indices3 <- createDataPartition(df_selected3$run_value_per_100, p = 0.7, list = FALSE)
OS_train_data <- df_selected3[train_indices3, ]
OS_test_data <- df_selected3[-train_indices3, ]




OS_train_X <- OS_train_data[, c("pitcher_break_x", "pitcher_break_z", "avg_speed")]
OS_train_Y <- OS_train_data$run_value_per_100

OS_test_X <- OS_test_data[, c("pitcher_break_x", "pitcher_break_z", "avg_speed")]
OS_test_Y <- OS_test_data$run_value_per_100



OS_trainIndex <- createDataPartition(OS_train_data$run_value_per_100, p = 0.7, list = FALSE)
OS_train_set <- OS_train_data[OS_trainIndex, ]
OS_validation_set <- OS_train_data[-OS_trainIndex, ]
OS_validation_set

OS_train_data <- data.frame(OS_train_X, OS_train_Y)

OS_gbm_model <- gbm(
  OS_train_Y ~ .,  
  data = OS_train_data,  
  n.trees = 1000,  
  verbose = FALSE,  
  distribution = "gaussian",  
  shrinkage = 0.01,  
  cv.folds = 10,  
  keep.data = TRUE  
)

OS_validation_predictions <- predict(OS_gbm_model, newdata = OS_validation_set, n.trees = OS_gbm_model$best.iter)


OS_RMSE <- RMSE(OS_validation_predictions, OS_test_Y)

## Offspeed performed the worst by far with 2.2 RMSE



## Lets visualize our predictions on a heatmap


## Fastballs
length(validation_predictions)

heatmap_data <- data.frame(Hor_Break = validation_set$pitcher_break_x, Vert_Break = validation_set$pitcher_break_z, mph = validation_set$avg_speed,
                           run_value_per_100 = validation_predictions)
aggregated_data <- aggregate(run_value_per_100 ~ Hor_Break + Vert_Break + mph, data = heatmap_data, FUN = mean)


# Helpful
movement_plot <- ggplot(aggregated_data, aes(x = Hor_Break, y = Vert_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.6, height = 0.6) +
  labs(x = "Hor_Break", y = "Vert_Drop", title = "Fastball Stuff+ Model") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(0,19), ylim = c(7, 40))
movement_plot

#Decent
speed_horizontal <- ggplot(aggregated_data, aes(x = mph, y = Hor_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.5, height = 0.5) +
  labs(x = "mph", y = "Hor_Break", title = "Heatmap of Run Value") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(82,103), ylim = c(0, 20))
speed_horizontal

#Helpful
speed_vertical <- ggplot(aggregated_data, aes(x = mph, y = Vert_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.7, height = 0.7) +
  labs(x = "mph", y = "Vert_Break", title = "Heatmap of Run Value") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(82,103), ylim = c(7, 40))
speed_vertical



## Breaking Balls



BBheatmap_data <- data.frame(Hor_Break = BBvalidation_set$pitcher_break_x, Vert_Break = BBvalidation_set$pitcher_break_z, mph = BBvalidation_set$avg_speed,
                           run_value_per_100 = BBvalidation_predictions)
BBaggregatedata <- aggregate(run_value_per_100 ~ Hor_Break + Vert_Break + mph, data = BBheatmap_data, FUN = mean)


##Helpful
BBmovement_plot <- ggplot(BBaggregatedata, aes(x = Hor_Break, y = Vert_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.7, height = 0.7) +
  labs(x = "Hor_Break", y = "Vert_Drop", title = "Breaking Ball Stuff+ Model") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(), ylim = c())
BBmovement_plot

## Helpful
BBSpeed_Horizontal <- ggplot(BBaggregatedata, aes(x = mph, y = Hor_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.5, height = 0.5) +
  labs(x = "mph", y = "Hor_Break", title = "Heatmap of Breaking Ball Run Value", subtitle = 'By Speed and Horizontal Movement') +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()
BBSpeed_Horizontal

## Eh not terrible
BBSpeed_Vert <- ggplot(BBaggregatedata, aes(x = mph, y = Vert_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.5, height = 1) +
  labs(x = "mph", y = "Vert_Drop", title = "Heatmap of Run Value") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(), ylim = c())
BBSpeed_Vert

## Off Speed


OS_heatmap_data <- data.frame(Hor_Break = OS_validation_set$pitcher_break_x, Vert_Break = OS_validation_set$pitcher_break_z, mph = OS_validation_set$avg_speed,
                             run_value_per_100 = OS_validation_predictions)
OS_aggregatedata <- aggregate(run_value_per_100 ~ Hor_Break + Vert_Break + mph, data = OS_heatmap_data, FUN = mean)

## Helpful
OS_movement_plot <- ggplot(OS_aggregatedata, aes(x = Hor_Break, y = Vert_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.6, height = 0.6) +
  labs(x = "Hor_Break", y = "Vert_Drop", title = "OffSpeed Stuff+ Model") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(), ylim = c())
OS_movement_plot

# Not that good
OS_Speed_Horizontal <- ggplot(OS_aggregatedata, aes(x = mph, y = Hor_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.5, height = 0.5) +
  labs(x = "mph", y = "Hor_Break", title = "Heatmap of Run Value") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()
OS_Speed_Horizontal

## Helpful
OS_Speed_Vert <- ggplot(OS_aggregatedata, aes(x = mph, y = Vert_Break, fill = run_value_per_100)) +
  geom_tile(width = 0.5, height = 1) +
  labs(x = "mph", y = "Vert_Drop", title = "Heatmap of OffSpeed Run Values", subtitle = 'By Vertical Drop and mph') +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  coord_cartesian(xlim = c(), ylim = c())
OS_Speed_Vert


FastballRMSE
BB_RMSE
OS_RMSE
