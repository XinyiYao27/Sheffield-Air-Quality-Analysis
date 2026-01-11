##Explore the data: Visualisation


#Heatmap
library(patchwork)

heatmap_data <- df_new %>%
  group_by(day_of_week, hour) %>%
  summarise(mean_no2 = mean(no2, na.rm=TRUE),
            mean_pm25 = mean(pm25, na.rm=TRUE))

p1 <- ggplot(heatmap_data, aes(x=hour, y=day_of_week, fill=mean_pm25)) +
  geom_tile(color="white") + 
  scale_fill_viridis_c(option="magma", direction=-1) + 
  labs(title="The Temporal Fingerprint: Pollution Heatmap",
       x=NULL, y="Day of Week", fill="PM2.5") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,23,2))

p2 <- ggplot(heatmap_data, aes(x=hour, y=day_of_week, fill=mean_no2)) +
  geom_tile(color="white") +
  scale_fill_viridis_c(option="magma", direction=-1) + 
  labs(x="Hour", y="Day of Week", fill="NO2") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,23,2))


p_week_heat = p1/p2
p_week_heat
p2




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

