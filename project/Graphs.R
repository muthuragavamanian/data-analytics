install.packages("tidyverse")
install.packages("maps")
library(tidyverse)
library(maps)

# Read the CSV files
metadata <- read_csv("/cloud/project/UnicefData/unicef_metadata.csv")
indicator_data <- read_csv("/cloud/project/UnicefData/unicef_indicator_2.csv")
continent_data <- read_csv("/cloud/project/UnicefData/data_right_3.csv")

# Merge the indicator_data with metadata and continent_data
total_data <- indicator_data %>%
  merge(metadata, by.x = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "time_period"),
        by.y = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "year")) %>%
  merge(continent_data, by = "country")

# Calculate the average migration rate for each continent across years
average_by_continent <- total_data %>%
  group_by(continent) %>%
  summarise(average_obs_value = mean(obs_value, na.rm = TRUE))

# Create a bar chart
average_by_continent %>%
  ggplot(aes(x = continent, y = average_obs_value, fill = continent)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(-3, 6), breaks = seq(-3, 6, by = 1)) +
  labs(title = "Average Migration Rate",
       x = "Continent",
       y = "Migration Rate") +
  scale_fill_brewer(palette = "Set1") + 
  guides(fill = FALSE) + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "#e6e6e3"), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank()) 


# Calculate max and min migration rate for each year in each continent
maxmin_by_continent <- total_data %>%
  group_by(continent, time_period) %>%
  summarise(max_obs_value = max(obs_value),
            min_obs_value = min(obs_value))


# Create time series plot for African continent
ggplot(maxmin_by_continent, aes(x = time_period, y = min_obs_value, ymax = max_obs_value)) +
  geom_line(aes(group = continent), color = "#db0802", size = 1) +  
  geom_line(aes(y = max_obs_value, group = continent), color = "#229602", size = 1) + 
  labs(x = "Year",
       y = "Migration Rate",
       color = "continent") + 
  facet_wrap(~continent, ncol = 5) +
  scale_x_continuous(breaks = seq(1960, 2022, by = 30)) + 
  theme_minimal() + 
  guides(color = FALSE) + 
  theme(strip.text.x = element_text(size = 8),
        panel.spacing.x = unit(1, "lines"))

# Load world map data
world_map <- map_data("world")

# Filter the total_data for the year 1990 and merge with world map data
migration_data_1990 <- total_data %>%
  filter(time_period == 1990) %>%
  merge(world_map, by.x = c("country"), by.y = c("region"), all.x = TRUE)


# Plot the world map
ggplot() +
  geom_polygon(data = migration_data_1990, aes(x = long, y = lat, group = group, fill = obs_value)) +
  scale_fill_gradient(low = "#e80c0c", high = "#37c43e", name = "obs_value", limits = c(-500, 0), breaks = seq(-500, 0, by = 100)) +
  theme_minimal() +
  theme(legend.position = "hide")


# Create scatter plot
total_data %>%
  filter(country == "Kuwait") %>%
ggplot(aes(x = time_period, y = obs_value, size = `Inflation, consumer prices (annual %)`, color = obs_value)) +
  geom_point(alpha=0.7) +
  scale_color_gradient(low = "#98e2eb", high = "#ed8582", name = "obs_value") +
  labs(x = "Year",
       y = "Migration Rate") +
  theme_minimal() + 
  theme(legend.position = "hide")


# Create scatter plot
total_data %>%
ggplot(aes(x = `GDP per capita (constant 2015 US$)`, y = obs_value, size = `Population, total`, color = continent)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Asia" = "#ed7c68", "Europe" = "#83b0f7", "Africa" = "#85a7d6", "Americas" = "#f7b474", "Oceania" = "#87e37b")) +
  labs(x = "GDP per capita", y = "Migration Rate", color="Continent") +
  theme_minimal() + 
  guides(size = FALSE) + 
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"), breaks = seq(0, 100000, by = 25000))
