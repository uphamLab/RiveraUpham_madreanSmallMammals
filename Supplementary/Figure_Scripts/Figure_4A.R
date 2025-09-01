library(ggplot2)
library(dplyr)
library(stringr)

rodent <-read.csv("Supplemental Table 3- Rodentia Specimens.csv")

custom_genus_order_rodent <- c("Baiomys", "Microtus", "Neotoma", "Onychomys", "Peromyscus", "Reithrodontomys", "Sigmodon", "Erethizon", "Megascapheus", "Thomomys", "Chaetodipus", "Dipodomys", "Perognathus", "Mus", "Rattus", "Ammospermophilus", "Cynomys", "Neotamias", "Otospermophilus", "Sciurus", "Tamiasciurus", "Xerospermophilus")
rodent$genus_mdd <- factor(rodent$genus_mdd, levels = custom_genus_order_rodent)
label_data_rodent <- rodent %>%
  count(genus_mdd, family)
custom_colors <- c("#4478a6", "#898c26", "#94cef2", "#3c401e", "#a3afbb",  "#AA865A")
ggplot(label_data_rodent, aes(x = genus_mdd, y = n, fill = family)) +
  geom_col() +
  geom_text(aes(label = n), 
            vjust = -0.2, 
            size = 5) +
  scale_fill_manual(values = custom_colors) +
  scale_y_sqrt(breaks = c(0, 100, 500, 1000, 2000, 3000), limits = c(0, 3600))+
  labs(title = "", x = "", y = "Specimens") +
  theme(plot.title = element_text(hjust = .5, size = 20),
        legend.position = c(.65, .75),
        plot.margin = margin(b = -10, r = 20, l = 5),
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