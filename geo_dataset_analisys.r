# Install necessary packages if not already installed
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")

BiocManager::install("GEOquery")
install.packages(c("dplyr", "ggplot2", "ggpubr", "tibble"))

# Load the packages
library(GEOquery)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tibble)


# Load the data
data <- readRDS("/Users/doblakov/R/rstudio-export(1)/data.rds")
metadata <- readRDS("/Users/doblakov/R/rstudio-export(1)/metadata.rds")

# Calculate the maximum absolute difference in mean expression between males and females
max_diff <- max(abs(colMeans(data[metadata$sex == "male",]) - colMeans(data[metadata$sex == "female",])))
print(max_diff)

# Identify taxa with significant differences in expression between males and females
diffs <- colMeans(data[metadata$sex == "male",]) - colMeans(data[metadata$sex == "female",])
significant_taxa <- diffs[abs(diffs) > 570000]
print(significant_taxa)

# Target taxon
target_taxon <- "Taxon_111"

# Prepare the data for ggplot2
df_to_ggplot2 <- as.data.frame(data[,"Taxon_111", drop = FALSE])
colnames(df_to_ggplot2) <- "expression"

# Add log-transformed expression and sex information
df_to_ggplot2$log_exp <- log(df_to_ggplot2$expression + 1)
df_to_ggplot2$sex <- as.factor(metadata$sex)

# Filter out rows with zero expression
df_to_ggplot2 <- df_to_ggplot2[df_to_ggplot2$expression != 0,]

# Create the boxplot
p <- ggplot(df_to_ggplot2, aes(x = sex, y = log_exp, fill = sex)) + 
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