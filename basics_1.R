# Установим необходимые пакеты, если они еще не установлены
if (!require("languageserver")) install.packages("languageserver")
if (!require("matrixStats")) install.packages("matrixStats")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("readxl")) install.packages("readxl")
if (!require("officer")) install.packages("officer")


library(matrixStats)
library(openxlsx)
library(readxl)
library(officer)

data(USArrests)
df <- USArrests

# 1. количество строк и столбцов(dim, ncol, nrow)
dim(df)
ncol(df)
nrow(df)

# 2. вывести на консоль содержание первых строк (head, tail, обращение по индексам строк)
head(df)
tail(df)

# 3. вывести на консоль 16-20 элементы 3 столбца, обратившись к нему по имени (обращение по именам столбцов, индексам строк)
df$UrbanPop[16:20]

# 4. тип/структуру/подтип данных для датасета в целом и каждого столбца (mode, class, typeof, цикл for, print)
mode(df)
class(df)
typeof(df)
print(df)

for(i in 1:ncol(df)) {
  print(colnames(df)[i])
  print(mode(df[,i]))
  print(class(df[,i]))
  print(typeof(df[,i]))
  cat('\n')
}

# 5. имена столбцов и строк (colnames, rownames, dimnames)
colnames(df)
rownames(df)
dimnames(df)

# 6. сумму, среднее, дисперсию, среднеквадратическое отклонение всего датасета (sum, mean, sd, var)
sum(df)
mean(as.matrix(df), na.rm = TRUE)
sd(as.matrix(df))
var(as.matrix(df))

# 7. сумму, среднее, дисперсию, среднеквадратическое отклонение каждого столбца 
colSums(df)
colMeans(df)
sapply(df, sd)
sapply(df, var)

# 8. сумму, среднее, дисперсию, среднеквадратическое  отклонение каждой строки
rowSums(df)
rowMeans(df)
rowSds(as.matrix(df))
rowVars(as.matrix(df))

# 9. вывести с 10 по 14 элементы для каждого столбца (обращение по индексам строк, for)
for (i in colnames(df)){
  print(df[[i]][10:14])
}

# 10. сколько в датасете элементов меньше 10 (sum, условие)
sum(df < 10)

# 11. сколько в каждом столбце элементов меньше 10 (sum, условие)
apply(df < 10 ,2, sum)

# 12 какие штаты содержат в названии “Miss” (which, rownames, grepl)
df_rows <- rownames(df)
df_rows[grepl("Miss", df_rows)]
which(grepl("Miss", df_rows))

# 13. вывести на консоль криминальную статистику для штатов, содержащих в названии “New”
ind <- which(grepl("New", df_rows))
summary(df[ind,])

# 14. записать файл .csv, содержащий 1-20 строки и 1 и 3 столбцы 
write.csv(df[1:20, c(1, 3)], "test_df.csv")

# 15. записать файл .xlsx, содержащий листы M и N со статистикой для штатов, начинающихся на букву M и N 
states_m <- which(grepl("^M", df_rows))
states_n <- which(grepl("^N", df_rows))
states_m_stats <- summary(df[states_m, ])
states_n_stats <- summary(df[states_n, ])

dataset_names <- list('M' = states_m_stats, 'N' = states_n_stats)

write.xlsx(dataset_names, file = "filename.xlsx")

# 16 прочитать записанные файлы
x_csv <- read.csv('test_df.csv')
head(x_csv, 2)

x_m <- readxl::read_excel("filename.xlsx", sheet = 1)
x_m <- openxlsx::read.xlsx("filename.xlsx", sheet = "M", rowNames = TRUE)
head(x_m, 2)

x_n <- readxl::read_excel("filename.xlsx", sheet = 2)
head(x_n, 2)

# Дополнительный уровень 1: Функция для выполнения действий 1-11, 14
perform_analysis <- function(dataset) {
  result <- list()

  # 1. количество строк и столбцов
  result$dim <- dim(dataset)
  result$ncol <- ncol(dataset)
  result$nrow <- nrow(dataset)

  # 2. первые и последние строки
  result$head <- head(dataset)
  result$tail <- tail(dataset)

  # 3. 16-20 элементы 3 столбца
  if (ncol(dataset) >= 3 && nrow(dataset) >= 20) {
    result$elements_16_20 <- dataset[16:20, 3]
  }

  # 4. тип/структура/подтип данных
  result$mode <- mode(dataset)
  result$class <- class(dataset)
  result$typeof <- typeof(dataset)

  column_info <- list()
  for(i in 1:ncol(dataset)) {
    column_info[[colnames(dataset)[i]]] <- list(
      mode = mode(dataset[, i]),
      class = class(dataset[, i]),
      typeof = typeof(dataset[, i])
    )
  }
  result$column_info <- column_info

  # 5. имена столбцов и строк
  result$colnames <- colnames(dataset)
  result$rownames <- rownames(dataset)
  result$dimnames <- dimnames(dataset)

  # 6. сумма, среднее, дисперсия, среднеквадратическое отклонение всего датасета
  result$sum <- sum(dataset)
  result$mean <- mean(as.matrix(dataset), na.rm = TRUE)
  result$sd <- sd(as.matrix(dataset))
  result$var <- var(as.matrix(dataset))

  # 7. сумма, среднее, дисперсия, среднеквадратическое отклонение каждого столбца
  result$colSums <- colSums(dataset)
  result$colMeans <- colMeans(dataset)
  result$colSd <- sapply(dataset, sd)
  result$colVar <- sapply(dataset, var)

  # 8. сумма, среднее, дисперсию, среднеквадратическое отклонение каждой строки
  result$rowSums <- rowSums(dataset)
  result$rowMeans <- rowMeans(dataset)
  result$rowSds <- rowSds(as.matrix(dataset))
  result$rowVars <- rowVars(as.matrix(dataset))

  # 9. вывести с 10 по 14 элементы для каждого столбца
  row_elements <- list()
  for (i in colnames(dataset)) {
    row_elements[[i]] <- dataset[10:14, i]
  }
  result$row_elements <- row_elements

  # 10. сколько в датасете элементов меньше 10
  result$elements_less_than_10 <- sum(dataset < 10)

  # 11. сколько в каждом столбце элементов меньше 10
  result$col_elements_less_than_10 <- apply(dataset < 10, 2, sum)

  # 14. записать файл .csv, содержащий 1-20 строки и 1 и 3 столбцы
  if (nrow(dataset) >= 20 && ncol(dataset) >= 3) {
    write.csv(dataset[1:20, c(1, 3)], "test_df.csv")
  }

  return(result)
}

# Применить функцию к датасетам iris и mtcars
iris_analysis <- perform_analysis(iris)
mtcars_analysis <- perform_analysis(mtcars)

# Преобразование результатов анализа в текст
format_analysis <- function(analysis) {
  text <- ""
  text <- paste(text, "Dimensions: ", paste(analysis$dim, collapse = " x "), "\n", sep = "")
  text <- paste(text, "Number of columns: ", analysis$ncol, "\n", sep = "")
  text <- paste(text, "Number of rows: ", analysis$nrow, "\n", sep = "")
  text <- paste(text, "First few rows: \n", paste(capture.output(print(analysis$head)), collapse = "\n"), "\n", sep = "")
  text <- paste(text, "Last few rows: \n", paste(capture.output(print(analysis$tail)), collapse = "\n"), "\n", sep = "")
  text <- paste(text, "Elements 16-20 of the 3rd column: ", paste(analysis$elements_16_20, collapse = ", "), "\n", sep = "")
  text <- paste(text, "Sum: ", analysis$sum, "\n", sep = "")
  text <- paste(text, "Mean: ", analysis$mean, "\n", sep = "")
  text <- paste(text, "Standard deviation: ", analysis$sd, "\n", sep = "")
  text <- paste(text, "Variance: ", analysis$var, "\n", sep = "")
  text <- paste(text, "Elements less than 10: ", analysis$elements_less_than_10, "\n", sep = "")
  text <- paste(text, "Elements less than 10 in each column: ", paste(analysis$col_elements_less_than_10, collapse = ", "), "\n", sep = "")
  return(text)
}

iris_text <- format_analysis(iris_analysis)
mtcars_text <- format_analysis(mtcars_analysis)

# Дополнительный уровень 2: Создание презентации .pptx с результатами
my_pres <- read_pptx()

# Добавить слайды с анализом iris
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, type = "title", str = "Iris Analysis")
my_pres <- ph_with(my_pres, type = "body", str = iris_text)

# Добавить слайды с анализом mtcars
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, type = "title", str = "Mtcars Analysis")
my_pres <- ph_with(my_pres, type = "body", str = mtcars_text)

# Сохранить презентацию
print(my_pres, target = "dataset_analysis.pptx")
