# AdvancED Equity Brand Theme for R Visualizations
# Created: January 2025
# Based on Visual Brand Guidelines February 2024
# Tried Seaford after installing again

# Required packages
library(ggplot2)
library(tibble)
# windowsFonts(Seaford = windowsFont("Seaford")) 

library(showtext)
library(extrafont)

# Manually add Seaford (update with the correct font path)
font_add("Seaford", "~/Library/Group Containers/UBF8T346G9.Office/FontCache/4/CloudFonts/Seaford/19722397430.ttf")  
# Enable showtext rendering
showtext_auto()
#===============================
# Brand Color Definitions
#===============================
brand_colors <- list(
  dark_gray = "#303333",  # PMS 179-15-C
  yellow = "#e5de54",     # PMS 1-15 C
  sage = "#808c73",       # PMS 178-8 C
  gray = "#a1a1a1",       # PMS 179-7 C
  light_gray = "#bdbfbf"  # PMS 175-2 C
)

#===============================
# Color Palette Function
#===============================
brand_palette <- function(n = 5) {
  colors <- c(brand_colors$dark_gray, 
              brand_colors$yellow,
              brand_colors$sage,
              brand_colors$gray,
              brand_colors$light_gray)
  colors[1:n]
}

#===============================
# Brand Theme Definition
#===============================
theme_brand <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Text elements
      text = element_text(family = "Seaford"),  # Seaford
      plot.title = element_text(
        size = rel(1.2), 
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = rel(1)
      ),
      axis.title = element_text(
        face = "bold"
      ),
      
      # Grid elements
      panel.grid.major = element_line(color = brand_colors$light_gray),
      panel.grid.minor = element_blank(),
      
      # Legend
      legend.position = "bottom"
    )
}

#===============================
# Scale Functions
#===============================
# For categorical colors
scale_fill_brand <- function(...) {
  scale_fill_manual(values = brand_palette(), ...)
}

scale_color_brand <- function(...) {
  scale_color_manual(values = brand_palette(), ...)
}

#===============================
# Example Usage
#===============================
# Example data
example_data <- tibble(
  category = c("Product A", "Product B", "Product C", "Product D", "Product E"),
  sales = c(150, 280, 210, 320, 190),
  type = factor(c("Type 1", "Type 2", "Type 3", "Type 4", "Type 5"))
)

# Example 1: Basic bar chart with all brand colors
p1 <- ggplot(example_data, aes(x = reorder(category, sales), y = sales, fill = type)) +
  geom_col() +
  scale_fill_brand() +
  theme_brand() +
  labs(
    title = "Sales by Product Category",
    subtitle = "Using All Brand Colors",
    x = "Product Category",
    y = "Sales Volume",
    fill = "Product Type"
  ) +
  coord_flip()



# Example 2: Gradient using brand colors
p2 <- ggplot(example_data, aes(x = category, y = sales, fill = sales)) +
  geom_col() +
  scale_fill_gradient(
    low = brand_colors$light_gray,
    high = brand_colors$sage,
    guide = "colorbar"
  ) +
  theme_brand() +
  labs(
    title = "Sales Performance",
    subtitle = "Light Gray to Sage Gradient",
    x = "Product Category",
    y = "Sales Volume"
  )



# Example 3: Three-color gradient
p3 <- ggplot(example_data, aes(x = category, y = sales, fill = sales)) +
  geom_col() +
  scale_fill_gradientn(
    colors = c(brand_colors$light_gray, brand_colors$sage, brand_colors$dark_gray),
    guide = "colorbar"
  ) +
  theme_brand() +
  labs(
    title = "Sales Performance",
    subtitle = "Three-Color Gradient",
    x = "Product Category",
    y = "Sales Volume"
  )

# Display plots
print(p1)
print(p2)
print(p3)

#===============================
# Usage Notes
#===============================
# To use this theme in your own visualizations:
# 1. Load the required packages (ggplot2, tibble)
# 2. Run all the definitions above (colors, theme, and scale functions)
# 3. Apply theme_brand() to your ggplot
# 4. Use scale_fill_brand() for categorical colors
# 5. For gradients, use brand_colors$color_name to access specific colors