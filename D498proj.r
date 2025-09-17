library(tidyverse)
library(lubridate)

#Reset all plots to only reflect plots created after running current code
graphics.off()

# Load data and clean up start times, dropping bad rows
load_city_data <- function(file_path, city_name) {
  data <- read_csv(file_path) %>%
    mutate(
      Start_Time_Parsed = parse_date_time(`Start Time`, orders = c("Ymd HMS", "m/d/Y H:M")),
      City = city_name
    ) %>%
    filter(!is.na(Start_Time_Parsed)) %>%
    mutate(
      Day_of_Week = wday(Start_Time_Parsed, label = TRUE, abbr = TRUE),
      Hour = hour(Start_Time_Parsed)
    )
  return(data)
}

# Load datasets
chicago <- load_city_data("chicago.csv", "Chicago")
new_york <- load_city_data("new-york-city.csv", "New York City")
washington <- load_city_data("washington.csv", "Washington")

# Combine everything
bike_data <- bind_rows(chicago, new_york, washington)

# 1. What is the most popular day of week in each city?
popular_day <- bike_data %>%
  group_by(City, Day_of_Week) %>%
  summarise(Trips = n()) %>%
  arrange(City, desc(Trips))

# Plot for question 1
ggplot(popular_day, aes(x = Day_of_Week, y = Trips, fill = City)) +
  geom_col(position = "dodge") +
  labs(title = "Most Popular Day of Week for Bike Trips by City",
       x = "Day of Week",
       y = "Number of Trips") +
  theme_minimal()

# 2. What is the average trip duration by user type and city?
avg_duration_user_type <- bike_data %>%
  filter(!is.na(`User Type`), !is.na(`Trip Duration`)) %>%
  group_by(City, `User Type`) %>%
  summarise(Average_Duration = mean(`Trip Duration`, na.rm = TRUE)) %>%
  arrange(City, `User Type`)

# Plot for question 2
ggplot(avg_duration_user_type, aes(x = `User Type`, y = Average_Duration, fill = City)) +
  geom_col(position = "dodge") +
  labs(title = "Average Trip Duration by User Type and City",
       x = "User Type",
       y = "Avg Trip Duration (seconds)") +
  theme_minimal()

# 3. What is the most common start stations in each city?
top_start_stations <- bike_data %>%
  group_by(City, `Start Station`) %>%
  summarise(Trips = n()) %>%
  arrange(City, desc(Trips)) %>%
  group_by(City) %>%
  slice_head(n = 1)  # top station per city

# Print out the top start stations to discuss in your presentation
print(top_start_stations)

# 4. What are the peak hours of usage by city?
peak_hours <- bike_data %>%
  filter(!is.na(Hour)) %>%
  group_by(City, Hour) %>%
  summarise(Trips = n())

# Plot for question 4
ggplot(peak_hours, aes(x = Hour, y = Trips, fill = City)) +
  geom_col(position = "dodge") +
  labs(title = "Peak Bike Usage Hours by City",
       x = "Hour of Day",
       y = "Number of Trips") +
  theme_minimal()
