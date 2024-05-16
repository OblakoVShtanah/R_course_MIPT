# Install necessary packages if not already installed
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("GEOquery")
# install.packages(c("dplyr", "ggplot2", "ggpubr", "tibble"))

# Load the packages
library(GEOquery)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tibble)


# Load the data
data <- readRDS("rstudio-export(1)/data.rds")
metadata <- readRDS("rstudio-export(1)/metadata.rds")
target_taxon <- "Taxon_111"

p <- data %>%
  as.data.frame() %>%
  bind_cols(metadata) %>%
  select(target_taxon, sex) %>%
  rename(expression = target_taxon) %>%
  mutate(
    log_exp = log(expression + 1),
    sex = as.factor(sex)
  ) %>%
  filter(expression != 0) %>%
  ggplot(aes(x = sex, y = log_exp, fill = sex)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("male" = "#1f78b4", "female" = "#e31a1c")) +
  theme_minimal() +
  labs(
    title = "Log-transformed Expression of Taxon_111 by Sex",
    x = "Sex",
    y = "Log2(Expression + 1)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test", label.x = 1.5)

print(p)



# ######## Calculate the maximum absolute difference in mean expression  ############
# ######## between males and females ################################################
# # max_diff <- max(abs(colMeans(data[metadata$sex == "male", ]) - colMeans(data[metadata$sex == "female", ])))
# # print(max_diff)
# max_diff <- data %>%
#   as.data.frame() %>%
#   bind_cols(metadata) %>%
#   group_by(sex) %>%
#   summarise(across(everything(), mean, .names = "mean_{col}")) %>%
#   summarise(max_diff = max(abs(mean_male - mean_female))) %>%
#   pull(max_diff)
# print(max_diff)

# ########### Identify taxa with significant differences #############################
# ########### in expression between males and females ################################
# # diffs <- colMeans(data[metadata$sex == "male", ]) - colMeans(data[metadata$sex == "female", ])
# diffs <- data %>%
#   as.data.frame() %>%
#   bind_cols(metadata) %>%
#   group_by(sex) %>%
#   summarise(across(everything(), mean, .names = "mean_{col}")) %>%
#   summarise(across(starts_with("mean_"), ~ mean_male - mean_female)) %>%
#   select(-sex) %>%
#   pivot_longer(everything(), names_to = "taxon", values_to = "difference") %>%
#   filter(abs(difference) > 570000) %>%
#   pull(difference)
# print(diffs)


# # Target taxon
# target_taxon <- "Taxon_111"


# ###################### Prepare the data for ggplot2 ######################

# # df_to_ggplot2 <- as.data.frame(data[, "Taxon_111", drop = FALSE])
# # colnames(df_to_ggplot2) <- "expression"

# # # Add log-transformed expression and sex information
# # df_to_ggplot2$log_exp <- log(df_to_ggplot2$expression + 1)
# # df_to_ggplot2$sex <- as.factor(metadata$sex)

# # # Filter out rows with zero expression
# # df_to_ggplot2 <- df_to_ggplot2[df_to_ggplot2$expression != 0, ]

# df_to_ggplot2 <- data %>%
#   as.data.frame() %>%
#   select(all_of(target_taxon)) %>%
#   rename(expression = all_of(target_taxon)) %>%
#   mutate(log_exp = log(expression + 1),
#          sex = as.factor(metadata$sex)) %>%
#   filter(expression != 0)


# # Create the boxplot
# p <- ggplot(df_to_ggplot2, aes(x = sex, y = log_exp, fill = sex)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
#   scale_fill_manual(values = c("male" = "#1f78b4", "female" = "#e31a1c")) +
#   theme_minimal() +
#   labs(
#     title = "Log-transformed Expression of Taxon_111 by Sex",
#     x = "Sex",
#     y = "Log2(Expression + 1)"
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 12),
#     legend.position = "none"
#   ) +
#   stat_compare_means(aes(label = ..p.signif..), method = "t.test", label.x = 1.5)

# print(p)
