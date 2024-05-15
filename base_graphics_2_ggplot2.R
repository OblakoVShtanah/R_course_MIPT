# Установка и загрузка необходимых пакетов
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

penguins <- read.csv("penguins.csv")

# Гистограмма с использованием ggplot2
p1 <- ggplot(penguins, aes(x = bill_length_mm)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(x = "Bill Length (mm)", y = "Frequency", 
       title = "Histogram of Bill Length") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 22))

# Гладкая линия плотности с использованием ggplot2
p2 <- ggplot(penguins, aes(x = bill_length_mm)) +
  geom_density(color = "blue", fill = "blue", alpha = 0.3) +
  labs(x = "Bill Length (mm)", y = "Density", 
       title = "Density Plot of Bill Length") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 22))

# Точечный график с использованием ggplot2
p3 <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)) +
  geom_point(size = 3) +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", 
       title = "Scatter Plot of Bill Length vs. Bill Depth") +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 22))

# Боксплоты с использованием ggplot2
p4 <- ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 2, color = "black") +
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(x = "Species", y = "Bill Length (mm)", 
       title = "Boxplots of Bill Length by Species") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15, angle = 90, hjust = 1),
        plot.title = element_text(size = 22))

# График с 4 панелями
p_combined <- grid.arrange(
  p1 + annotate("text", x = Inf, y = Inf, label = "A", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  p2 + annotate("text", x = Inf, y = Inf, label = "B", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  p4 + annotate("text", x = Inf, y = Inf, label = "C", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  p3 + annotate("text", x = Inf, y = Inf, label = "D", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  ncol = 2
)

# Сохранение графиков
ggsave("base_graphics_2_ggplot2_plots.png", plot = p_combined, width = 12, height = 10)
