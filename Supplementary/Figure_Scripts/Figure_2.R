library("zoo")
library("ggplot2")
library ("dplyr")

Chiricahua <- read.csv("Supplemental Table 8- Individual_Mountain_Elevation - Chiricahua.csv")
Peloncillo_South <- read.csv("Supplemental Table 8- Individual_Mountain_Elevation - Peloncillo South.csv")
Rincon <- read.csv("Supplemental Table 8- Individual_Mountain_Elevation - Rincon.csv")
Pinalenos <- read.csv("Supplemental Table 8- Individual_Mountain_Elevation - Pinalenos.csv")
Patagonia <- read.csv("Supplemental Table 8- Individual_Mountain_Elevation - Patagonia.csv")
Santa_Rita <- read.csv("Supplemental Table 8- Individual_Mountain_Elevation - Santa Rita.csv")

Peloncillo_South_smooth <- Peloncillo_South %>%
  arrange(Value) %>%
  mutate(Area = rollmean(Area, k = 9, fill = NA, align = "center"))

Rincon_smooth <- Rincon %>%
  arrange(Value) %>%
  mutate(Area = rollmean(Area, k = 9, fill = NA, align = "center"))

Chiricahua_smooth <- Chiricahua %>%
  arrange(Value) %>%
  mutate(Area = rollmean(Area, k = 9, fill = NA, align = "center"))

Pinaleno_smooth <- Pinalenos %>%
  arrange(Value) %>%
  mutate(Area = rollmean(Area, k = 9, fill = NA, align = "center"))

Patagonia_smooth <- Patagonia %>%
  arrange(Value) %>%
  mutate(Area = rollmean(Area, k = 9, fill = NA, align = "center"))

Santa_Rita_smooth <- Santa_Rita %>%
  arrange(Value) %>%
  mutate(Area = rollmean(Area, k = 9, fill = NA, align = "center"))

colors <- c(  "#E84B4F", "#009E73", "#AA865A", "#0072B2", "#A98FBA", "#87CEEB")
labels <- c("Rincon", "Peloncillo_South", "Chiricahuas", "Pinalenos", "Patagonia", "Santa_Rita")
combined_plot <- ggplot() +
  geom_line(data = Pinaleno_smooth, aes(x = Value, y = Area, color = "Pinalenos"), linewidth = 1.5) +
  geom_line(data = Chiricahua_smooth, aes(x = Value, y = Area, color = "Chiricahuas"), linewidth = 1.5) +
  geom_line(data = Santa_Rita_smooth, aes(x = Value, y = Area, color = "Santa_Rita"), linewidth = 1.5) +
  geom_line(data = Rincon_smooth, aes(x = Value, y = Area, color = "Rincon"), linewidth = 1.5) +
  geom_line(data = Patagonia_smooth, aes(x = Value, y = Area, color = "Patagonia"), linewidth = 1.5) +
  geom_line(data = Peloncillo_South_smooth, aes(x = Value, y = Area, color = "Peloncillo_South"), linewidth = 1.5) +
  guides(color = guide_legend(override.aes = list(shape = 15))) +
  labs(x = "Elevation (m)", y = expression("Area (km"^2*")"), title = "") +
  guides(
    color = guide_legend(override.aes = list(linewidth = 5)),
    fill = guide_legend(title = "Sky Island")) +
  scale_y_continuous(breaks = seq(0, 9, by = 1), expand = c(.01, .3)) +
  scale_x_continuous(breaks = seq(1600, 3500, by = 250), expand = c(.01, 0)) +
  scale_color_manual(values = setNames(colors, labels), name = NULL) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(.5, .75, .5, .5, "cm"),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black"),
    #legend.position = ("none")
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(size = 20),
    legend.position = c(400, 0.7),
    legend.key = element_blank()
  )
print(combined_plot)
#Plot was oriented vertically in Adobe Photoshop