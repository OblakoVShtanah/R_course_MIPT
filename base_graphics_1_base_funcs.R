# Задание 1 по базовой графике:
# Для датасета mtcars визуализировать:
# • распределение qsec в виде гладкой линии и гистограммы (hist, plot-density-polygon)
# • для гистограммы: подписать имена оси x, заголовок, подзаголовок, заполнить цветом гистограмму, задать другим цветом границы столбцов гистограммы. Увеличить размер шрифтов для осей в 2 раза (подписи и тики). 
# • точечный график, на котором будет отображаться зависимость mpg от disp (plot). Задать свой цвет, форму элементов для каждого значения cyl. Размер элементов должен быть равен 2. 
# • боксплоты+джиттерплоты mpg для каждого значения cyl(три боксплота на 1 графике, джиттерплоты наложены на боксплоты). Надписи по оси абсцисс развернуть под углом 90 градусов.


# Установка и загрузка необходимых пакетов
if (!require("datasets")) install.packages("datasets")

data(mtcars)

# 1. Распределение qsec в виде гладкой линии и гистограммы
par(mfrow = c(1, 2)) # Разделение на 2 графика в одном ряду

# Гистограмма
hist(mtcars$qsec, 
     main = "Histogram of Quarter Mile Time", 
     sub = "Distribution of qsec in mtcars dataset", 
     xlab = "Quarter Mile Time", 
     col = "skyblue", 
     border = "black")

# Гладкая линия плотности
plot(density(mtcars$qsec), 
     main = "Density Plot of qsec", 
     xlab = "qsec", 
     ylab = "Density",
     col = "blue")
polygon(density(mtcars$qsec), 
        col = rgb(0, 0, 1, 0.3), 
        border = "blue")

# 2. Точечный график зависимости mpg от disp
par(mfrow = c(1, 1)) # Вернуть на один график
colors <- c("red", "green", "blue")
shapes <- c(16, 17, 18)
plot(mtcars$disp, mtcars$mpg, 
     col = colors[as.factor(mtcars$cyl)], 
     pch = shapes[as.factor(mtcars$cyl)], 
     cex = 2, 
     xlab = "Displacement", 
     ylab = "Miles Per Gallon", 
     main = "Scatter Plot of MPG vs Displacement"
     )
legend("topright", legend = levels(as.factor(mtcars$cyl)), 
       col = colors, 
       pch = shapes, 
       title = "Cylinders"
       )

# 3. Боксплоты и джиттерплоты для mpg по значениям cyl
par(mfrow = c(1, 1))
boxplot(mpg ~ factor(cyl), data = mtcars, 
        col = c("red", "green", "blue"), 
        xlab = "Number of Cylinders", 
        ylab = "Miles Per Gallon", 
        main = "Boxplots and Jitter Plots of MPG by Cylinders"
        )
stripchart(mpg ~ factor(cyl), data = mtcars, 
           vertical = TRUE, method = "jitter", 
           pch = 16, col = "black", add = TRUE 
          #  cex = 2
           )

# Поворот подписей оси x на 90 градусов
axis(1, at = 1:3, labels = levels(factor(mtcars$cyl)), las = 2)

# Дополнительный уровень 1: График с 4 панелями
par(mfrow = c(2, 2)) # Разделение на 4 графика

# Панель A: Гладкая линия плотности
plot(density(mtcars$qsec), 
     main = "A: Density Plot of qsec", 
     xlab = "qsec", 
     ylab = "Density", 
     col = "blue"
     )
polygon(density(mtcars$qsec), 
        col = rgb(0, 0, 1, 0.3), 
        border = "blue")

# Панель B: Гистограмма
hist(mtcars$qsec, 
     main = "B: Histogram of Quarter Mile Time", 
     sub = "Distribution of qsec in mtcars dataset", 
     xlab = "Quarter Mile Time", 
     col = "skyblue", 
     border = "black"
     )

# Панель C: Боксплоты и джиттерплоты
boxplot(mpg ~ factor(cyl), data = mtcars, 
        col = c("red", "green", "blue"), 
        xlab = "Number of Cylinders", 
        ylab = "Miles Per Gallon", 
        main = "C: Boxplots and Jitter Plots of MPG by Cylinders"
        )
stripchart(mpg ~ factor(cyl), data = mtcars, 
           vertical = TRUE, method = "jitter", 
           pch = 16, col = "black", add = TRUE
           )
axis(1, at = 1:3, labels = levels(factor(mtcars$cyl)), las = 2)

# Панель D: Точечный график зависимости mpg от disp
plot(mtcars$disp, mtcars$mpg, 
     col = colors[as.factor(mtcars$cyl)], 
     pch = shapes[as.factor(mtcars$cyl)], 
     cex = 2, 
     xlab = "Displacement", 
     ylab = "Miles Per Gallon", 
     main = "D: Scatter Plot of MPG vs Displacement"
     )
legend("topright", legend = levels(as.factor(mtcars$cyl)), 
       col = colors, 
       pch = shapes, 
       title = "Cylinders"
       )

# Сохранение графика
dev.copy(png, file = "base_graphics_1_combined_plot.png", width = 1200, height = 1000)
dev.off()
