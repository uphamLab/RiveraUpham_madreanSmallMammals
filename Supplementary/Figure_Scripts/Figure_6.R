library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

combined_data <- read.csv("Supplemental Table 5- Combined Specimens (filtered for year)")


order_colors <- c("Rodentia" = "#73DFFF", "Eulipotyphla" = "#E60000", "Chiroptera" = "#895A44")

# Step 1: Count specimens per year per order
yearly_counts <- combined_data %>%
  group_by(Order, year) %>%
  summarise(count = n(), .groups = "drop")

# Step 2: Compute cumulative counts
cumulative_data <- yearly_counts %>%
  arrange(Order, year) %>%
  group_by(Order) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()

cumulative_data <- cumulative_data %>%
  mutate(Order = fct_relevel(Order,  "Rodentia", "Chiroptera", "Eulipotyphla"))

ggplot(cumulative_data, aes(x = year, y = cumulative_count, color = Order)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(breaks = seq(1875, 2025, by = 25), limits = c(1875, 2025)) +
  scale_y_continuous() +
  scale_color_manual(values = order_colors) +
  labs(
    title = "",
    x = "Year",
    y = "Specimens"
  ) +
  geom_vline(xintercept = 1980, linetype = "dashed", color = "black", linewidth = 1) +
  theme(
    legend.position = c(.90, .7),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black"),
    legend.background = element_rect(color = "black", fill = "white")
  )
