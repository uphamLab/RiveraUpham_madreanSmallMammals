library(ggplot2)
library(dplyr)
library(stringr)

bat <- read.csv("Supplemental Table 2- Chiroptera Specimens.csv")

custom_genus_order <- c("Eumops", "Nyctinomops", "Tadarida", "Mormoops", "Pteronotus", "Natalus", "Artibeus", "Choeronycteris", "Glossophaga", "Leptonycteris", "Macrotus", "Sturnira", "Antrozous", "Corynorhinus", "Eptesicus", "Idionycteris", "Lasionycteris", "Lasiurus", "Myotis", "Parastrellus")
bat$genus_mdd <- factor(bat$genus_mdd, levels = custom_genus_order)
label_data_bat <- bat %>%
  count(genus_mdd, family)
custom_colors <- c("#4478a6", "#898c26", "#94cef2", "#3c401e", "#a3afbb",  "#AA865A")
ggplot(label_data_bat, aes(x = genus_mdd, y = n, fill = family)) +
  geom_col() +
  geom_text(aes(label = n), 
            vjust = -0.2, 
            size = 5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "", x = "", y = "Specimens") +
  scale_y_sqrt(breaks = c(0, 100, 500, 1000, 2000, 3000), limits = c(0, 3600))+
  theme(plot.title = element_text(hjust = .5, size = 20),
        legend.position = c(.23, .75),
        plot.margin = margin(b = -20, r = 20, l = 5),
        axis.title.x = element_text(size = 17),              
        axis.title.y = element_text(size = 17),              
        axis.text.x = element_text(size = 15, angle = 45, face = "italic", vjust = 1.05, hjust = 1.01),               
        axis.text.y = element_text(size = 15))+
  guides(fill = guide_legend(title = "Family"))+
  theme(
    panel.background = element_blank(),        
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),         
    panel.grid.major = element_line(color = "grey90"),  
    panel.grid.minor = element_line(color = "grey90"),        
    axis.line = element_line(color = "black"),  
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(size = 15), 
    legend.title = element_text(size = 17)
  )