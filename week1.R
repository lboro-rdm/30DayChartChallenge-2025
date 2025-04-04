library(scales)
library(readxl)
library(tidyverse)
library(cowplot)
library(ggplot2)

# Data cleaning -----------------------------------------------------------

# Load data, assigning column names manually
data_2016_2022 <- read_excel("RawData/2016-2022.xlsx", col_names = TRUE) %>%
  slice(-1) 

# Rename the first column as "year" since it lacks a header
colnames(data_2016_2022)[1] <- "year"

data_2024 <- read_excel("RawData/2024.xlsx", col_names = TRUE) %>%
  slice(-1)

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

# W1D2: Slope -------------------------------------------------------------

# Q3.39 How supportive would you be of a national mandate for making research data openly available?

# Clean and categorize responses
df_clean <- data_2016_2022 %>%
  mutate(Q3.39 = str_trim(Q3.39), # Remove any trailing spaces
         support_category = case_when(
           Q3.39 %in% c("Strongly support", "Somewhat support") ~ "Yes",
           Q3.39 == "Neutral" ~ "Erm...",
           Q3.39 %in% c("Somewhat oppose", "Strongly oppose") ~ "No",
           TRUE ~ NA_character_ # Treat NA and other values as missing
         )) %>%
  filter(!is.na(support_category)) # Remove NA values

# Summarize counts per year
df_summary <- df_clean %>%
  group_by(year, support_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percent = count / sum(count) * 100) # Convert to percentages

custom_colors <- c("Yes" = "#B70062", "Erm..." = "#8D9C27", "No" = "#361163")

df_summary <- df_summary %>%
  mutate(support_category = factor(support_category, levels = c("Yes", "Erm...", "No")))

# Create the line chart
p <- ggplot(df_summary, aes(x = year, y = percent, color = support_category, group = support_category)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = custom_colors, guide = guide_legend(title = NULL)) +  # Remove legend title
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +  # Fix percentage formatting
  labs(
    title = "Would you support a National Mandate on Open Data?",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme_minimal(base_family = "") +  # Minimal theme
  theme(
    legend.position = "top",  # Move legend to top
    panel.background = element_rect(fill = "#f2f2f2"),  # Change panel background
    plot.background = element_rect(fill = "#f2f2f2"),  # Match plot background
    panel.border = element_blank(),  # Remove panel border
    legend.text = element_text(size = 10)  # Adjust legend text size
  )

# Add annotation text
text_plot <- ggdraw() + 
  draw_label("The State of Open Data 2016-2022", 
             x = 0, hjust = 0, vjust = 0.5, size = 10, color = "black", fontface = "italic") +
  draw_label("#30DayChartChallenge\nDay 2: Slope", 
             x = 1, hjust = 1, vjust = 0.5, size = 10, color = "black", fontface = "italic") +
  theme(
    plot.background = element_rect(fill = "#f2f2f2"),  # Match background color
    panel.border = element_blank(),  
    plot.margin = margin(t = 0, r = 20, b = 0, l = 20)  # Adjust margins
  )

# Combine line chart and annotation
final_plot <- plot_grid(p, text_plot, ncol = 1, rel_heights = c(1, 0.1))  

# Print final combined plot
print(final_plot)

# W1D3: Circular ----------------------------------------------------------

# Define groupings
stem_fields <- c(
  "Engineering", "Medicine", "Physics", "Materials Science", "Biology",
  "Earth and Environmental Science", "Chemistry", "Astronomy and planetary science"
)

shape_fields <- c(
  "Social Sciences", "Arts & Humanities", "Business/Investment"
)

q8_grouped <- data_2024 %>%
  filter(!is.na(Q8)) %>%
  mutate(group = case_when(
    Q8 %in% stem_fields ~ "STEM",
    Q8 %in% shape_fields ~ "SHAPE",
    TRUE ~ "Other"
  )) %>%
  count(group)

# Set the order of the groups for the legend
q8_grouped$group <- factor(q8_grouped$group, levels = c("STEM", "SHAPE", "Other"))

# Calculate percentages for legend labels
q8_grouped <- q8_grouped %>%
  mutate(percentage = n / sum(n) * 100,
         legend_label = paste0(group, " (", round(percentage, 1), "%)")  # Create legend label with percentage
  )

# Custom colors for groups
custom_colors <- c("STEM" = "#B70062", "SHAPE" = "#8D9C27", "Other" = "#361163")

# Create title text
text_title <- ggdraw() + 
  draw_label("Who responded to The State of Open Data survey?", 
             x = 0.5, hjust = 0.5, vjust = 0.5, size = 14, color = "black") +
  theme(
    panel.border = element_blank(),  
    plot.margin = margin(t = 0, r = 20, b = 0, l = 20)  # Adjust margins
  )

donut_plot <- ggplot(q8_grouped, aes(x = "", y = n, fill = group)) +  # x is set to an empty string
  geom_col(color = NA) +  # No outline color
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors, labels = q8_grouped$legend_label, guide = guide_legend(title = NULL)) +  # Use legend labels with percentages
  theme_void(base_size = 14) +  # Use theme_void to remove backgrounds
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10)
  )

# Create annotation text
text_plot <- ggdraw() + 
  draw_label("The State of Open Data 2024", 
             x = 0, hjust = 0, vjust = 0.5, size = 10, color = "black", fontface = "italic") +
  draw_label("#30DayChartChallenge\nDay 3: Circular", 
             x = 1, hjust = 1, vjust = 0.5, size = 10, color = "black", fontface = "italic") +
  theme(
    panel.border = element_blank(),  
    plot.margin = margin(t = 0, r = 20, b = 0, l = 20)  # Adjust margins
  )

# Combine title, donut chart, and annotation
final_plot <- plot_grid(text_title, donut_plot, text_plot, ncol = 1, rel_heights = c(0.1, 0.8, 0.1))   

# Print final combined plot
print(final_plot)
