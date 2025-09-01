library(ggplot2)
library(dplyr)
library(stringr)

bat <- read.csv("Supplemental Table 2- Chiroptera Specimens.csv")

custom_colors <- c("#4478a6", "#3c401e", "#a3afbb")

top_species_bat <- bat %>%
  count(species_mdd, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  pull(species_mdd)

bat_top10 <- bat %>%
  filter(species_mdd %in% top_species_bat) %>%
  mutate(species_mdd = factor(species_mdd, levels = custom_order_bat))  

custom_order_bat <- c(
  "Tadarida brasiliensis", "Leptonycteris yerbabuenae", "Antrozous pallidus",
  "Corynorhinus townsendii", "Eptesicus fuscus", "Lasiurus cinereus", "Myotis auriculus", "Myotis thysanodes", "Myotis velifer",
  "Myotis volans", "Parastrellus hesperus"
)

ggplot(bat_top10, aes(x = species_mdd, fill = family)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    position = position_nudge(y = 1.5),  
    size = 5
  )+
  scale_fill_manual(values = custom_colors) +
  labs(title = "", x = "", y = "Specimens") +
  scale_y_sqrt(breaks = c(0, 100, 500, 1000, 2000, 3000), limits = c(0, 2000))+
  theme(
    plot.title = element_text(hjust = .5, size = 20),
    plot.margin = margin(b = -10, l = 5, r = 10),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(size = 15, angle = 45, face = "italic", vjust = 1.01, hjust = 1.01),
    axis.text.y = element_text(size = 15),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black"),
    legend.position = "none"  
  )