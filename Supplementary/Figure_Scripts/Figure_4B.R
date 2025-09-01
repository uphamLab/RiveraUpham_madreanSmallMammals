library(ggplot2)
library(dplyr)
library(stringr)

rodent <-read.csv("Supplemental Table 3- Rodentia Specimens.csv")


top_species <- rodent %>%
  count(species_mdd, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  pull(species_mdd)

rodent_top10 <- rodent %>%
  filter(species_mdd %in% top_species) %>%
  mutate(species_mdd = factor(species_mdd, levels = custom_order))  

custom_colors <- c("#4478a6", "#94cef2", "#AA865A")

custom_order <- c(
  "Microtus longicaudus", "Neotoma albigula", "Neotoma mexicana", "Peromyscus boylii",
  "Peromyscus sonoriensis", "Peromyscus melanotis", "Megascapheus bottae", "Megascapheus umbrinus",
  "Neotamias dorsalis", "Otospermophilus variegatus"
)

ggplot(rodent_top10, aes(x = species_mdd, fill = family)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    position = position_nudge(y = 1),  # Nudges label 5 units above bar
    size = 5
  )+
  scale_fill_manual(values = custom_colors) +
  labs(title = "", x = "", y = "Specimens") +
  scale_y_sqrt(breaks = c(0, 100, 500, 1000, 2000, 3000))+
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