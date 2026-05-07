library(tidyverse)

# Load the Ames Housing data from the project folder.
ames <- read_csv("AmesHousing.csv", show_col_types = FALSE)

# Quick first look at the dataset.
glimpse(ames)
summary(ames$SalePrice)

# Keep output images together for the report.
if (!dir.exists("plots")) {
  dir.create("plots")
}

base_theme <- theme_minimal(base_size = 12)

# 1. Distribution of home prices.
saleprice_hist <- ggplot(ames, aes(x = SalePrice)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Sale Prices",
    x = "Sale Price",
    y = "Number of Homes"
  ) +
  base_theme

print(saleprice_hist)
ggsave("plots/saleprice_histogram.png", saleprice_hist, width = 8, height = 5, dpi = 300)

# 2. Living area compared to sale price.
grlivarea_plot <- ggplot(ames, aes(x = `Gr Liv Area`, y = SalePrice)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Above-Ground Living Area vs Sale Price",
    x = "Above-Ground Living Area (sq ft)",
    y = "Sale Price"
  ) +
  base_theme

print(grlivarea_plot)
ggsave("plots/gr_liv_area_vs_saleprice.png", grlivarea_plot, width = 8, height = 5, dpi = 300)

# 3. Overall quality compared to sale price.
quality_plot <- ggplot(ames, aes(x = factor(`Overall Qual`), y = SalePrice)) +
  geom_boxplot(fill = "seagreen3", alpha = 0.8) +
  labs(
    title = "Overall Quality vs Sale Price",
    x = "Overall Quality",
    y = "Sale Price"
  ) +
  base_theme

print(quality_plot)
ggsave("plots/overall_qual_vs_saleprice.png", quality_plot, width = 8, height = 5, dpi = 300)
