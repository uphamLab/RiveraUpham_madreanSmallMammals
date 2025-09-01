library(ggplot2)
library(dbplyr)

RBS_2025 <- read.csv("Supplemental Data 7- Specimen_Area_Matrix.csv")

non_zero_mountains <- dplyr::filter(RBS_2025, Total_Specimens>=1)

ggplot(non_zero_mountains, aes(x = Max_Elevation, y = TS_km, label = Mountain)) +
  geom_point(size = 4, color = "#4478a6") +
  geom_smooth(method = "lm", se = FALSE, color = "#94cef2") +
  geom_text(size = 3, nudge_y = -.09, nudge_x = 0, angle = 0) +  
  labs(title = "", x = "Maximum Elevation (m)", y = "Specimen-Area Ratio") +
  theme(
    plot.title = element_text(hjust = .5, size = 20),
    plot.margin = margin(b = 3, l = 30, r = 10),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black"),
    legend.position = "none"  
  )