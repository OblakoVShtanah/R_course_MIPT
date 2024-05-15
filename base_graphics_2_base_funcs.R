# Загрузка датасета penguins в R
penguins <- read.csv("penguins.csv")

# Установим графические параметры и создадим несколько графиков
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

# 1. Распределение bill_length_mm в виде гистограммы с настройками
par(mfrow = c(2, 2))

# Гистограмма
hist(penguins$bill_length_mm,
     main = "Histogram of Bill Length",
     xlab = "Bill Length (mm)",
     col = "lightblue",
     border = "darkblue",
     cex.lab = 2,
     cex.axis = 2,
     cex.main = 1.5)

# Гладкая линия плотности
plot(density(na.omit(penguins$bill_length_mm)),
     main = "Density Plot of Bill Length",
     xlab = "Bill Length (mm)",
     ylab = "Density",
     col = "blue",
     lwd = 2, 
     cex.lab = 2, 
     cex.axis = 2, 
     cex.main = 1.5)
polygon(density(na.omit(penguins$bill_length_mm)),
        col = rgb(0, 0, 1, 0.3),
        border = "blue")

# Точечный график bill_length_mm vs. bill_depth_mm
colors <- c("red", "green", "blue")
shapes <- c(16, 17, 18)
plot(penguins$bill_length_mm, penguins$bill_depth_mm,
     col = colors[as.factor(penguins$species)],
     pch = shapes[as.factor(penguins$species)],
     cex = 2,
     xlab = "Bill Length (mm)",
     ylab = "Bill Depth (mm)",
     main = "Scatter Plot of Bill Length vs. Bill Depth",
     cex.lab = 2,
     cex.axis = 2,
     cex.main = 1.5)
legend("topright", legend = levels(as.factor(penguins$species)),
       col = colors,
       pch = shapes,
       title = "Species",
       cex = 2)

# Боксплоты bill_length_mm для каждого вида пингвинов
boxplot(bill_length_mm ~ species, data = penguins, 
        col = c("red", "green", "blue"), 
        xlab = "Species", 
        ylab = "Bill Length (mm)", 
        main = "Boxplots of Bill Length by Species",
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 1.5)
stripchart(bill_length_mm ~ species, data = penguins,
           vertical = TRUE, method = "jitter",
           pch = 16, col = "black", add = TRUE,
           cex = 2)

# Поворот подписей оси x на 90 градусов
axis(1, at = 1:3, labels = levels(factor(penguins$species)), las = 2)
par(resetPar())

# Сохранение графика
dev.copy(png, file = "base_graphics_2_base_funcs_plots.png", width = 1200, height = 1000)
dev.off()
