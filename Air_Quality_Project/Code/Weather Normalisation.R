
##EDA before weather normalisation

#Check whether the distribution is normal
no2 <- df$no2 %>% na.omit()
ks.test(no2, "pnorm", mean = mean(no2), sd = sd(no2))

no2_hist <- 
  df %>% filter(!is.na(no2)) %>%
  ggplot(aes(x = no2)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 2, color = "white", fill = "lightblue")+
  geom_density(fill = NA, color = "red", linetype = "dashed", linewidth = 1) +
  coord_cartesian(xlim = c(0,75)) +
  theme_minimal() + 
  labs(x = "NO2", y = "Density", title = "Distribution of Air Pollutants")


pm25 <- df$pm25 %>% na.omit()
ks.test(pm25, "pnorm", mean = mean(pm25), sd = sd(pm25))

pm25_hist <- 
  df %>% filter(!is.na(pm25)) %>%
  ggplot(aes(x = pm25)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 2, color = "white", fill = "lightgreen")+
  geom_density(fill = NA, color = "red", linetype = "dashed", linewidth = 1) +
  coord_cartesian(xlim = c(0,40)) +
  theme_minimal() + 
  labs(x = "PM2.5", y = "Density")

library(patchwork)
p_hist <- no2_hist | pm25_hist
p_hist


#Check the differences among sites

kruskal.test(no2 ~ site, data = df) # check the p-value (<0.05)
kruskal.test(pm25 ~ site, data = df)



##Meteorology Normalisation

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

