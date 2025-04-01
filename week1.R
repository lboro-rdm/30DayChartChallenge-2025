library(scales)
library(readxl)
library(tidyverse)
library(cowplot)

# Data cleaning -----------------------------------------------------------

# Load data, assigning column names manually
data_2016_2022 <- read_excel("RawData/2016-2022.xlsx", col_names = TRUE) %>%
  slice(-2)  # Remove second row

# Rename the first column as "year" since it lacks a header
colnames(data_2016_2022)[1] <- "year"

# W1D1: Fractions ---------------------------------------------------------


# Filter for 2022 data only and select relevant columns
data_2022 <- data_2016_2022 %>%
  filter(year == 2022) %>%  # Keep only 2022 responses
  select(region = `Derived variable_2`, response = `Q2.11_2`) %>%
  drop_na()  # Remove missing values

# Remove text within brackets from region names
data_2022 <- data_2022 %>%
  mutate(region = str_remove(region, "\\s*\\(.*\\)"))  # Remove text in parentheses

# Group responses into three categories
data_2022 <- data_2022 %>%
  mutate(grouped_response = case_when(
    response %in% c("Strongly disagree", "Somewhat disagree") ~ "No",
    response %in% c("Somewhat agree", "Strongly agree") ~ "Yes",
    response %in% c("Neutral / No opinion", "I am unaware of this practice") ~ "Erm...",
  )) %>%
  mutate(grouped_response = factor(grouped_response, levels = c("No", "Erm...", "Yes"))) %>%
  mutate(region = str_replace(region, "North America", "N. America"),
         region = str_replace(region, "South America", "S. America"))



# Count occurrences for each grouped response per region
data_summary <- data_2022 %>%
  count(region, grouped_response) %>%
  group_by(region) %>%
  mutate(percent = n / sum(n))  # Convert counts to percentages


custom_colors <- c("No" = "#361163", "Erm..." = "#8D9C27", "Yes" = "#B70062")

# Create stacked bar chart
p <- ggplot(data_summary, aes(x = region, y = percent, fill = grouped_response)) +
  geom_bar(stat = "identity", position = "fill") +  # Fill makes it 100% stacked
  scale_y_continuous(labels = scales::percent_format()) +  # Show as percentages
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  scale_fill_manual(values = custom_colors, name = NULL) +
  labs(
    title = "Should Open Data be common scholarly practice?",
    x = NULL,  # Remove x-axis label
    y = NULL,  # Remove y-axis label
    fill = "Response"
  ) +
  theme_minimal(base_family = "") +  # Set minimal theme
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "#f2f2f2"),  # Change panel background
    plot.background = element_rect(fill = "#f2f2f2"),
    panel.border = element_blank()  # Change plot background
  )


text_plot <- ggdraw() + 
  draw_label("The State of Open Data 2022", 
             x = 0,  # Left aligned
             hjust = 0, vjust = 0.5, size = 10, color = "black", fontface = "italic") +
  draw_label("#30DayChartChallenge\nDay 1: Fractions", 
             x = 1,  # Right aligned
             hjust = 1, vjust = 0.5, size = 10, color = "black", fontface = "italic") +
  theme(plot.background = element_rect(fill = "#f2f2f2"),  # Match background color
        panel.border = element_blank(),  # Ensure no border around text
        plot.margin = margin(t = 0, r = 20, b = 0, l = 20))  # Adjust margins if needed

# Combine the plot and text
final_plot <- plot_grid(p, text_plot, ncol = 1, rel_heights = c(1, 0.1))  # Adjust rel_heights as needed

# Print the final combined plot
print(final_plot)