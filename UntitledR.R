library(tidyverse)  
library(readr)     
library(ggplot2)
library(ggmap)
library(gganimate)

install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "dplyr"))

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

data <- read.csv("Downloads/Global_index36787_startsinJan.csv")

data$total_months <- data$year * 12 + data$month

time_frame <- data.frame(months = seq(0, 120, by = 2))

# Sum the probabilities for each month in 'time_frame' using a join and summarizing
time_summary <- time_frame %>%
  rowwise() %>%
  mutate(total_probability = sum(data$probability[data$total_months == months], na.rm = TRUE))

# Plot the results
plot(time_summary$months, time_summary$total_probability, type = "o",
     xlab = "Months since release", ylab = "Total Probability",
     main = "Total Fraction of Plastic Over Time")


# Adjust longitude to the -180 to 180 range
data$longitude_adjusted <- ifelse(data$lng > 180, data$lng - 360, data$lng)

# Plotting the dispersion on a world map
world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  geom_point(data = data, aes(x = longitude_adjusted, y = lat, color = probability), alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 0.40)) +
  labs(title = "Plastic Dispersion Patterns", x = "Longitude", y = "Latitude") +
  theme_minimal()


# Calculate the variance of latitude and longitude for each time point (total_months)
dispersion_rate <- data %>%
  group_by(total_months) %>%
  summarise(lat_variance = var(lat, na.rm = TRUE),
            lon_variance = var(longitude_adjusted, na.rm = TRUE),
            dispersion_rate = lat_variance + lon_variance)  # Sum variances as a measure of dispersion

# Plot the dispersion rate over time
ggplot(dispersion_rate, aes(x = total_months, y = dispersion_rate)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Rate of Dispersion Over Time",
       x = "Months Since Release",
       y = "Dispersion Rate (Sum of Variances)") +
  theme_minimal()



data$region <- case_when(
  data$lat >= 10 & data$lat <= 30 & data$longitude_adjusted >= -100 & data$longitude_adjusted <= -80 ~ "Gulf of Mexico",
  data$lat >= 10 & data$lat <= 25 & data$longitude_adjusted >= -80 & data$longitude_adjusted <= -60 ~ "Caribbean Sea",
  data$lat >= 25 & data$lat <= 45 & data$longitude_adjusted >= -80 & data$longitude_adjusted <= -30 ~ "North Atlantic Subtropical Gyre",
  data$lat >= 0 & data$lat <= 20 & data$longitude_adjusted >= -60 & data$longitude_adjusted <= 0 ~ "Equatorial Atlantic Extension",
  data$lat >= 0 & data$lat <= 20 & data$longitude_adjusted >= -60 & data$longitude_adjusted <= -30 ~ "Western Atlantic Coastal Waters",
  data$lat >= 45 & data$lat <= 60 & data$longitude_adjusted >= -30 & data$longitude_adjusted <= 20 ~ "North Atlantic Drift",
  data$lat >= 30 & data$lat <= 60 & data$longitude_adjusted >= -30 & data$longitude_adjusted <= 0 ~ "Eastern North Atlantic",
  TRUE ~ "Other"
)


# Calculate total probability in each region and total plastic at each time point
region_dispersion <- data %>%
  group_by(total_months, region) %>%
  summarise(total_probability = sum(probability, na.rm = TRUE)) %>%
  ungroup()

# Calculate the total plastic at each time point
total_plastic <- region_dispersion %>%
  group_by(total_months) %>%
  summarise(total_prob_all_regions = sum(total_probability, na.rm = TRUE))

# Merge to calculate the fraction of plastic in each region
region_dispersion <- region_dispersion %>%
  left_join(total_plastic, by = "total_months") %>%
  mutate(fraction_plastic = total_probability / total_prob_all_regions)

# Plot the fraction of plastic in each region over time
ggplot(region_dispersion, aes(x = total_months, y = fraction_plastic, color = region)) +
  geom_line() +
  labs(title = "Fraction of Plastic in Different Regions Over Time",
       x = "Months Since Release",
       y = "Fraction of Total Plastic",
       color = "Oceanic Region") +
  theme_minimal()






# Filter data for a specific time (e.g., total_months = 12) for a snapshot view
filtered_data <- data %>% filter(total_months == 12)

# Create a heatmap of plastic concentration
ggplot(filtered_data, aes(x = longitude_adjusted, y = lat)) +
  geom_tile(aes(fill = probability), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Probability") +
  labs(title = "Plastic Concentration Heatmap at 12 Months",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()



# Filter data for a specific time (e.g., total_months = 12)
region_data <- region_dispersion %>% filter(total_months == 12)

# Create a bar plot showing the fraction of plastic in each region
ggplot(region_data, aes(x = region, y = fraction_plastic, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Fraction of Plastic by Region at 12 Months",
       x = "Region",
       y = "Fraction of Total Plastic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(total_plastic, aes(x = total_months, y = total_prob_all_regions)) +
  geom_line(color = "darkgreen") +
  labs(title = "Total Plastic Concentration Over Time",
       x = "Months Since Release",
       y = "Total Probability (Concentration)") +
  theme_minimal()






data$year <- floor(data$total_months / 12) + 1
# Aggregate plastic probability by year and region
yearly_dispersion <- data %>%
  group_by(year, region) %>%
  summarise(total_probability = sum(probability, na.rm = TRUE)) %>%
  ungroup()

# Bar plot showing total probability for each region by year
ggplot(yearly_dispersion, aes(x = factor(year), y = total_probability, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Changes in Plastic Distribution Across Regions",
       x = "Year",
       y = "Total Probability (Plastic Concentration)",
       fill = "Region") +
  theme_minimal()







# Calculate total probability in each region and total plastic at each time point
region_dispersion <- data %>%
  group_by(total_months, region) %>%
  summarise(total_probability = sum(probability, na.rm = TRUE)) %>%
  ungroup()

# Calculate the total plastic at each time point
total_plastic <- region_dispersion %>%
  group_by(total_months) %>%
  summarise(total_prob_all_regions = sum(total_probability, na.rm = TRUE))

# Merge to calculate the fraction of plastic in each region
region_dispersion <- region_dispersion %>%
  left_join(total_plastic, by = "total_months") %>%
  mutate(fraction_plastic = total_probability / total_prob_all_regions)

# Filter data to include only 2-month increments
region_dispersion_filtered <- region_dispersion %>% filter(total_months %% 2 == 0)

# Create the animated bar plot with 2-month increments
animated_bar <- ggplot(region_dispersion_filtered, aes(x = region, y = fraction_plastic, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Fraction of Plastic by Region Over Time: Month {frame_time}",
       x = "Region",
       y = "Fraction of Total Plastic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_time(total_months) +
  ease_aes('linear')

# Render the animation
animate(animated_bar, nframes = length(unique(region_dispersion_filtered$total_months)), fps = 10, width = 800, height = 600)

# Export the animation to a GIF file
anim_save("plastic_dispersion_animation.gif", animation = animated_bar)




# Aggregate probabilities over all time points
heatmap_data <- data %>%
  group_by(lat, longitude_adjusted) %>%
  summarise(total_probability = sum(probability, na.rm = TRUE))

# Create the heatmap
ggplot(heatmap_data, aes(x = longitude_adjusted, y = lat, fill = total_probability)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Total Probability") +
  labs(title = "Plastic Concentration Heatmap Over All Time",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Load necessary libraries
library(magick)

# Load the images (update the paths to your image files)
image1 <- image_read("UU/IMS/Physics Lab/BeginningVisual.png")
image2 <- image_read("UU/IMS/Physics Lab/EndVisual.png")

image1_info <- image_info(image1)
image2_info <- image_info(image2)
max_height <- max(image1_info$height, image2_info$height)

# Create a white spacer image (10 pixels wide, with the maximum height of the images)
spacer <- image_blank(width = 10, height = max_height, color = "white")

# Combine the images with the white spacer in between
combined_image <- image_append(c(image1, spacer, image2), stack = FALSE)

# Save the combined image
image_write(combined_image, path = "combined_figure_with_line.png")
image_write(combined_image, path = "/path/to/save/combined_figure_with_line.png")


# Display the combined image
print(combined_image)



# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the plot
ggplot() +
  # Add the world map
  geom_sf(data = world, fill = "grey", color = "black") +
  # Plot the dispersion data using geom_tile(), fill by 'year'
  geom_tile(data = data, aes(x = longitude_adjusted, y = lat, fill = year)) +
  # Customize the color scale for years
  scale_fill_viridis_c(
    name = "Year",
    option = "viridis",
    breaks = seq(0, max(data$year, na.rm = TRUE), by = 2), # Adjust breaks based on data
    labels = seq(0, max(data$year, na.rm = TRUE), by = 2)
  ) +
  # Add map labels and titles
  labs(title = "Plastic Dispersion Over Time",
       x = "Longitude",
       y = "Latitude") +
  # Coordinate limits and map projection
  coord_sf(xlim = c(-100, 20), ylim = c(-10, 70), expand = FALSE) +
  theme_minimal()
