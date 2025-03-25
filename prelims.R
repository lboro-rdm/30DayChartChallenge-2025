library(scales)
library(readxl)
library(tidyverse)

show_col("#361163") #primary 1
show_col("#B70062") #primary 2
show_col("#8D9C27") #accent
show_col("#f2f2f2") #background


# Data cleaning -----------------------------------------------------------

data_2016_2022 <- read_excel("RawData/2016-2022.xlsx") %>%
  slice(-2) # Reads file in, removes the second row - which is the question details. This information is present in the codebook.csv

data_2023 <- read_excel("RawData/2023.xlsx")
data_2024 <- read_excel("RawData/2024.xlsx")



# Add YearOfSurvey column to 2023 and 2024
data_2023 <- data_2023 %>%
  mutate(YearOfSurvey = 2023) %>%
  relocate(YearOfSurvey)

data_2024 <- data_2024 %>%
  mutate(YearOfSurvey = 2024) %>%
  relocate(YearOfSurvey)




