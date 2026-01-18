
##Visualise the diurnal patterns by pollutants and sites

#Calculate the mean value of each hour and transform into a long form
hourly_pattern <- df_new %>%
  group_by(hour, is_weekend, site) %>%
  summarise(no2 = mean(no2, na.rm = TRUE),
            pm25 = mean(pm25, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(no2, pm25), 
               names_to = "Pollutant", 
               values_to = "Concentration")

p_week <- 
  ggplot(hourly_pattern, aes(x = hour, y = Concentration, color = is_weekend)) +
  geom_line(size = 1.0) +
  facet_grid(Pollutant ~ site, scales = "free_y") +
  scale_color_manual(values = c("Weekday" = "#E74C3C", "Weekend" = "#3498DB")) +
  scale_x_continuous(breaks = c(0, 8, 12, 17, 23)) +
  labs(title = "Rush Hour & Weekend Effect",
       subtitle = "Gap between Red/Blue lines: Human Activity Impact",
       x = "Hour of Day", 
       y = "Concentration") +
  theme_minimal()

p_week



## Training classification models

library(caret)        # Machine leaning
library(rpart)        # Decision Tree
library(rpart.plot)   # Plot decision tree

# 1. Transform 24 hours into 24 feature vectors
class_no2 <- df_new %>% 
  select(site, year, month, day, no2, hour, is_weekend) %>%
  pivot_wider(names_from = hour, values_from = no2) %>%
  select(-year, -month, -day) %>%
  mutate(site = ifelse(site == "Barn", 1, site)) %>%
  mutate(site = ifelse(site == "Devon", 2, site)) %>%
  mutate(site = ifelse(site == "Tin", 3, site)) %>%
  na.omit()

class_pm25 <- df_new %>%
  select(site, year, month, day, pm25, hour, is_weekend) %>%
  pivot_wider(names_from = hour, values_from = pm25) %>%
  select(-year, -month, -day) %>%
  mutate(site = ifelse(site == "Barn", 1, site)) %>%
  mutate(site = ifelse(site == "Devon", 2, site)) %>%
  mutate(site = ifelse(site == "Tin", 3, site)) %>%
  na.omit()

#Assigning variables names
colnames(class_no2) <- make.names(colnames(class_no2))
colnames(class_pm25) <- make.names(colnames(class_pm25))

#Ensure that the target variable is a categorical factor
class_no2$is_weekend <- as.factor(class_no2$is_weekend)
class_pm25$is_weekend <- as.factor(class_pm25$is_weekend)


# 2. Divide the dataset into an 80% training set and a 20% test set
set.seed(123)
nrow(class_no2) == nrow(class_pm25) #Same number of columns
index <- sample(1:nrow(class_no2), 
                size=nrow(class_no2)*0.8, 
                replace = FALSE)

train_no2 <- class_no2[index, ]
test_no2  <- class_no2[-index, ]
train_pm25 <- class_pm25[index, ]
test_pm25  <- class_pm25[-index, ]



# 3. Logistic Regression
glm_no2 <- glm(is_weekend ~ ., data = train_no2, family = "binomial")
glm_pm25 <- glm(is_weekend ~ ., data = train_pm25, family = "binomial")

#Predictions
prob_glm_no2 <- predict(glm_no2, test_no2, type = "response")
prob_glm_pm25 <- predict(glm_pm25, test_pm25, type = "response")

pred_glm_no2 <- ifelse(prob_glm_no2 > 0.5, "Weekend", "Weekday")
pred_glm_no2 <- as.factor(pred_glm_no2) 
pred_glm_pm25 <- ifelse(prob_glm_pm25 > 0.5, "Weekend", "Weekday")
pred_glm_pm25 <- as.factor(pred_glm_pm25) 

#Check the confusion matix
confusionMatrix(pred_glm_no2, test_no2$is_weekend)
confusionMatrix(pred_glm_pm25, test_pm25$is_weekend)




# 4. K Nearest Neighbours
knn_no2 <- train(is_weekend ~ ., 
                 data = train_no2, 
                 method = "knn",
                 preProcess = c("center", "scale"),  #scale the data 
                 trControl = trainControl(method = "cv", number = 10))
                      #Cross-validation
knn_pm25 <- train(is_weekend ~ ., 
                  data = train_pm25, 
                  method = "knn",
                  preProcess = c("center", "scale"), 
                  trControl = trainControl(method = "cv", number = 10))

pred_knn_no2 <- predict(knn_no2, test_no2)
pred_knn_pm25 <- predict(knn_pm25, test_pm25)

confusionMatrix(pred_knn_no2, test_no2$is_weekend)
confusionMatrix(pred_knn_pm25, test_pm25$is_weekend)




# 5. Decision Tree
dt_no2 <- rpart(is_weekend ~ ., data = train_no2, method = "class")
dt_pm25 <- rpart(is_weekend ~ ., data = train_pm25, method = "class")

#Plot the tree
rpart.plot(dt_no2, main = "NO2 Decision Tree Logic")
rpart.plot(dt_pm25, main = "PM2.5 Decision Tree Logic")

pred_dt_no2 <- predict(dt_no2, test_no2, type = "class")
pred_dt_pm25 <- predict(dt_pm25, test_pm25, type = "class")

confusionMatrix(pred_dt_no2, test_no2$is_weekend)
confusionMatrix(pred_dt_pm25, test_pm25$is_weekend)



# 6. Random Forest
rf_no2 <- randomForest(is_weekend ~ ., 
                       data = train_no2,
                       importance = TRUE)
rf_pm25 <- randomForest(is_weekend ~ ., 
                        data = train_pm25, 
                        importance = TRUE)

#Plot variable importance
varImpPlot(rf_no2, main = "Feature Importance of NO2")
varImpPlot(rf_pm25, main = "Feature Importance of PM2.5")

pred_rf_no2 <- predict(rf_no2, test_no2)
pred_rf_pm25 <- predict(rf_pm25, test_pm25)

confusionMatrix(pred_rf_no2, test_no2$is_weekend)
confusionMatrix(pred_rf_pm25, test_pm25$is_weekend)


