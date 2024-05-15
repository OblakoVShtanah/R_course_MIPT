# Базовый уровень+дополнительный 1. 
# Повтор задания для базовой графики, только реализация через ggplot2

# Установка и загрузка необходимых пакетов
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

data(mtcars)

# Распределение qsec в виде гладкой линии и гистограммы
p1 <- ggplot(mtcars, aes(x = qsec)) +
  geom_density(color = "blue", fill = "blue", alpha = 0.3) +
  labs(x = "qsec", y = "Density", title = "Density Plot of qsec") +
  theme_minimal()

p2 <- ggplot(mtcars, aes(x = qsec)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 10) +
  labs(x = "Quarter Mile Time", y = "Frequency", 
       title = "Histogram of Quarter Mile Time",
       subtitle = "Distribution of qsec in mtcars dataset") +
  theme(axis.title = element_text(size = rel(2)),
        axis.text = element_text(size = rel(2)),
        plot.title = element_text(size = rel(1.5)),
        plot.subtitle = element_text(size = rel(1.2)))

# Точечный график зависимости mpg от disp
p3 <- ggplot(mtcars, aes(x = disp, y = mpg, color = factor(cyl), shape = factor(cyl))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "green", "blue")) +
  scale_shape_manual(values = c(16, 17, 18)) +
  labs(x = "Displacement", y = "Miles Per Gallon", 
       title = "Scatter Plot of MPG vs Displacement",
       color = "Cylinders", shape = "Cylinders") +
  theme_minimal()

# Боксплоты и джиттерплоты для mpg по значениям cyl
p4 <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(aes(fill = factor(cyl)), outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, color = "black") +
  scale_fill_manual(values = c("red", "green", "blue")) +
  labs(x = "Number of Cylinders", y = "Miles Per Gallon", 
       title = "Boxplots and Jitter Plots of MPG by Cylinders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# График с 4 панелями
p_combined <- grid.arrange(
  p1 + annotate("text", x = Inf, y = Inf, label = "A", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  p2 + annotate("text", x = Inf, y = Inf, label = "B", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  p4 + annotate("text", x = Inf, y = Inf, label = "C", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  p3 + annotate("text", x = Inf, y = Inf, label = "D", vjust = 1.5, hjust = 1.5, size = 5, colour = "black"),
  ncol = 2
)

# Сохранение графиков
ggsave("base_graphics_2_combined_plot.png", plot = p_combined, width = 12, height = 10)
