df <- read.csv("Cleaned_AQI_Data.csv")

head(df)
str(df)
summary(df)

colSums(is.na(df))

head(df$date)

df$date <- as.Date(df$date, format = "%Y-%m-%d")

head(df$date)

str(df)

install.packages ("tidyverse") 
install.packages ("skimr") 
install.packages ("GGally") 
install.packages ("corrplot")
install.packages("dplyr")       
install.packages("ggplot2")     

df_1 <- df

library(dplyr)
library(ggplot2)
library(tidyr)

---------------  # Top 5 cities with highest average PM2.5#---------------------

#groupby
top5_pm25 <- df %>%
  group_by(city) %>%                       # Group by city
  summarise(avg_pm25 = mean(pm2.5, na.rm = TRUE)) %>%  # Calculate mean pm2.5
  arrange(desc(avg_pm25)) %>%              # Sort descending
  slice_head(n = 5)                        # Get top 5

# View result
top5_pm25

----# Pie chart--------
# Add percentage column
top5_pm25 <- top5_pm25 %>%
  mutate(perc = round(avg_pm25 / sum(avg_pm25) * 100, 1),
         label = paste0(perc, "%"))

#plot
ggplot(top5_pm25, aes(x = "", y = avg_pm25, fill = city)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart to pie
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Top 5 Cities with Highest Average PM2.5") +
  theme_void()



---------------# Top 5 cities with lowest average PM2.5#----------------------

#groupby
tail5_pm25 <- df %>%
  group_by(city) %>%
  summarise(avg_pm25 = mean(pm2.5, na.rm = TRUE)) %>%  # average PM2.5
  arrange(avg_pm25) %>%                                # Sort ascending
  slice_head(n = 5)                                    # Get first 5 → lowest values

#view_result
tail5_pm25


-------#Pie-Chart------
# Add percentage column
tail5_pm25 <- tail5_pm25 %>%
  mutate(perc = round(avg_pm25 / sum(avg_pm25) * 100, 1),
         label = paste0(perc, "%"))

# plot
ggplot(tail5_pm25, aes(x = "", y = avg_pm25, fill = city)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Bottom 5 Cities with Lowest Average PM2.5") +
  theme_void()



-----------------# Count number of readings per city#---------------------------

# groupby
city_counts <- df %>%
  group_by(city) %>%
  summarise(counts = n()) %>%        # n() counts rows
  arrange(desc(counts)) %>%
  slice_head(n = 10)

city_counts


------# Horizontal bar chart--------
ggplot(city_counts, aes(x = reorder(city, counts), y = counts, fill = city)) +
  geom_col() +  # Column/bar chart
  geom_text(aes(label = counts), hjust = -0.1) +  # Count labels outside the bars
  labs(title = "Top 5 Cities with Most Readings",
       x = "City",
       y = "Number of Readings") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # Flip coordinates to make horizontal



------------# Weekdays VS Weekend  AQI level Comparision BOX PLOT#--------------

df <- df %>%
  mutate(is_weekend = factor(is_weekend, levels = c("False", "True"),
                             labels = c("Weekday", "Weekend")))

# Boxplot of AQI by Weekday/Weekend
ggplot(df, aes(x = is_weekend, y = aqi, fill = is_weekend)) +
  geom_boxplot() +
  labs(title = "AQI Distribution: Weekdays vs Weekend",
       x = "",
       y = "AQI") +
  theme_minimal() +
  theme(legend.position = "none")


library(patchwork)


-------------# Top 4 cities by average pollution_index LINE PLOT# --------------

top4_city <- df %>%
group_by(city) %>%
summarise(avg_pollution = mean(pollution_index, na.rm = TRUE)) %>%
arrange(desc(avg_pollution)) %>%
slice_head(n = 4) %>%
pull(city)
        
# Create a plot for each city
plots <- lapply(top4_city, function(c) {
city_data <- df %>%
filter(city == c) %>%
group_by(day) %>%
summarise(avg_pollution = mean(pollution_index, na.rm = TRUE))
          
ggplot(city_data, aes(x = day, y = avg_pollution)) +
geom_line(color = "steelblue", size = 1) +
geom_point(color = "red", size = 2) +
geom_text(aes(label = round(avg_pollution, 2)), vjust = -0.5, size = 3) +
labs(title = paste("Pollution Index Trend –", c),x = "Day",y = "Pollution Index") +
            theme_minimal()
        })
        
#  Arrange the 4 plots in 2x2 grid
combined_plot <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]])
combined_plot
