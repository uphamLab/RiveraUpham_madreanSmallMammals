library(ggplot2)
library(dplyr)

#Read in data
bat_specimens <- read.csv("Supplementary_Data_SD3.csv")

shrew_specimens <- read.csv("Supplementary_Data_SD4.csv")

rodent_specimens <- read.csv("Supplementary_Data_SD5.csv")

#combine specimen data tables
combined_specimens <- bind_rows(bat_specimens, shrew_specimens, rodent_specimens)

#remove specimens that have no reported elevation
specimens_reported_elevation <- combined_specimens %>%
  filter(elevation != 0, elevation != "", !is.na(elevation))

#remove specimens that have the same value for both reported elevation and elevation accuracy
specimens_reported_elevation_filtered <- specimens_reported_elevation %>%
  filter(
    !(elevation == elevationAccuracy & elevation != "" & elevationAccuracy != "" & !is.na(elevation) & !is.na(elevationAccuracy))
  )

#Plot reported elevation against interpolated elevation (Z)
ggplot(specimens_reported_elevation, aes(x = elevation, y = Z)) +
  geom_point(size = 3, color = "#4478a6") +
  geom_abline(linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "#94cef2") +
  labs(title = "", x = "Reported Elevation (m)", y = "Interpolated Elevation (m)") +
  scale_y_continuous(breaks = c(1500, 2000, 2500, 3000, 3500), limits = c(1500, 3500)) +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), limits = c(0, 3500)) +
  theme(
    plot.title = element_text(hjust = .5, size = 20),
    plot.margin = margin(b = 3, l = 5, r = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black"),
    legend.position = "none"  
  )


#Plot reported elevation against interpolated elevation (Z) after removing specimens with the same value for reported elevation and elevation accuracy
ggplot(specimens_reported_elevation_filtered, aes(x = elevation, y = Z)) +
  geom_point(size = 3, color = "#4478a6") +
  geom_abline(linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "#94cef2") +
  labs(title = "", x = "Reported Elevation (m)", y = "Interpolated Elevation (m)") +
  scale_y_continuous(breaks = c(1500, 2000, 2500, 3000, 3500), limits = c(1500, 3500)) +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), limits = c(0, 3500)) +
  theme(
    plot.title = element_text(hjust = .5, size = 20),
    plot.margin = margin(b = 3, l = 5, r = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black"),
    legend.position = "none"  
  )

