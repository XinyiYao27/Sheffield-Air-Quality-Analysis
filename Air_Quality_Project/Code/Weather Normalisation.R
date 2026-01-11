
#ANOVA: Check the differences among sites

anova_no2 <- aov(no2 ~ site, data = df)
summary(anova_no2) # check the p-value (<0.05)

anova_pm25 <- aov(pm25 ~ site, data = df)
summary(anova_pm25) 


##Meteorology Normalisation
library(patchwork)
library(broom) 

# 1. Calculate the daily mean data before weather normalisation
daily_before <- df %>% 
  group_by(site, year, month, day) %>%
  summarise(no2 = mean(no2, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            ws = mean(ws, na.rm = T),
            wd = mean(wd, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            .groups = "drop")



# 2. Normalise the three sites respectively

#Barnsley
df_barn <- df %>% 
  filter(site=='Barn') %>% 
  select(-o3,-no,-nox,-pm10) %>%
  mutate(unix_time = as.numeric(date)) %>%
  na.omit()

#no2
rf_barn_no2_model <- 
  randomForest(no2 ~ temp + ws + wd + humidity + unix_time,
               data = df_barn,
               ntree = 300,
               do.trace = 10,      
               importance = TRUE)
weather_predicted = predict(rf_barn_no2_model, df_barn)
deweathered = df_barn$no2 - weather_predicted + mean(df_barn$no2, na.rm = TRUE)

df_barn <- df_barn %>% 
  mutate(no2 = deweathered)


#pm2.5
rf_barn_pm25_model <- 
  randomForest(pm25 ~ temp + ws + wd + humidity + unix_time,
               data = df_barn,
               ntree = 300,
               do.trace = 10,      
               importance = TRUE)
weather_predicted = predict(rf_barn_pm25_model, df_barn)
deweathered = df_barn$pm25 - weather_predicted + mean(df_barn$pm25,na.rm = TRUE)

df_barn <- df_barn %>% 
  mutate(pm25 = deweathered)

#Devonshire Green
df_devon <- df %>% 
  filter(site=='Devon') %>% 
  select(-o3,-no,-nox,-pm10) %>%
  mutate(unix_time = as.numeric(date)) %>%
  na.omit()

#no2
rf_devon_no2_model <- 
  randomForest(no2 ~ temp + ws + wd + humidity + unix_time,
               data = df_devon,
               ntree = 300,
               do.trace = 10,      
               importance = T)
weather_predicted = predict(rf_devon_no2_model, df_devon)
deweathered = df_devon$no2 - weather_predicted + mean(df_devon$no2, na.rm = T)

df_devon <- df_devon %>% 
  mutate(no2 = deweathered)


#pm2.5
rf_devon_pm25_model <- 
  randomForest(pm25 ~ temp + ws + wd + humidity + unix_time,
               data = df_devon,
               ntree = 300,
               do.trace = 10,      
               importance = T)
weather_predicted = predict(rf_devon_pm25_model, df_devon)
deweathered = df_devon$pm25 - weather_predicted + mean(df_devon$pm25, na.rm = T)

df_devon <- df_devon %>% 
  mutate(pm25 = deweathered)


#Tinsley
df_tin <- df %>% 
  filter(site=='Tin') %>% 
  select(-o3,-no,-nox,-pm10) %>%
  mutate(unix_time = as.numeric(date)) %>%
  na.omit()

#no2
rf_tin_no2_model <- 
  randomForest(no2 ~ temp + ws + wd + humidity + unix_time,
               data = df_tin,
               ntree = 300,
               do.trace = 10,      
               importance = TRUE)
weather_predicted = predict(rf_tin_no2_model, df_tin)
deweathered = df_tin$no2 - weather_predicted + mean(df_tin$no2, na.rm = TRUE)

df_tin <- df_tin %>% 
  mutate(no2 = deweathered)


#pm2.5
rf_tin_pm25_model <- 
  randomForest(pm25 ~ temp + ws + wd + humidity + unix_time,
               data = df_tin,
               ntree = 300,
               do.trace = 10,      
               importance = TRUE)
weather_predicted = predict(rf_tin_pm25_model, df_tin)
deweathered = df_tin$pm25 - weather_predicted + mean(df_tin$pm25, na.rm = TRUE)

df_tin <- df_tin %>% 
  mutate(pm25 = deweathered)



# 3. Merge into a new data frame
df_new <- rbind(df_barn, df_devon, df_tin) 
  select(-temp,-ws,-wd,-humidity,-unix_time)




# 4. Visualise the results

daily_after <- df_new %>% 
  group_by(site, year, month, day) %>%
  summarise(no2 = mean(no2, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            ws = mean(ws, na.rm = T),
            wd = mean(wd, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            .groups = "drop")
  
  
p_no2 <- bind_rows(Before = daily_before,
                   After = daily_after,
                   .id = "normalize") %>%
  pivot_longer(cols = c(temp, ws, wd, humidity),
               names_to = "Weather",
               values_to = "Value") %>%
  ggplot(aes(Value, no2, color = site)) +
  geom_point(alpha = 0.05)+
  geom_smooth(se = F)+
  facet_grid(normalize ~ Weather, scales = "free_x") +
  coord_cartesian(ylim = c(0,60))+
  labs(title = "Weather Normalized: Before and After",
       x = NULL,
       y = "NO2") +
    theme_minimal()
  
p_pm25 <- bind_rows(Before = daily_before,
                    After = daily_after,
                    .id = "normalize") %>%
  pivot_longer(cols = c(temp, ws, wd, humidity),
               names_to = "Weather",
               values_to = "Value") %>%
  ggplot(aes(Value, pm25, color = site)) +
  geom_point(alpha = 0.05)+
  geom_smooth(se = F)+
  facet_grid(normalize ~ Weather, scales = "free_x") +
  coord_cartesian(ylim = c(0,30))+
  labs(y = "PM2.5") +
  theme_minimal()
  
p_weather <- p_no2/p_pm25
p_weather


##Remove variables irrelevant to subsequent analysis
df_new <- df_new %>% select(-temp,-ws,-wd,-humidity,-unix_time)
