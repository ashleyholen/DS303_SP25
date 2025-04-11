####
# script to process HPD Crime Incident data from 
# https://data.honolulu.gov/Public-Safety/HPD-Crime-Incidents/vg88-5rn5/data_preview
# Amber Camp, Ashley Holen, April 2025
####

# load packages
library(tidyverse)
library(plotly)

# read in the data
spending <- read_csv("data/Hawaii Tourism Data (from DBEDT Data Warehouse).csv")

# pivot and mutate from wide to long format
spending_long <- spending %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Spending") %>%
  mutate(
    Year = as.integer(Year),
    Spending = as.numeric(Spending),
    Group = as.factor(Group),
    Category = as.factor(Category)
  ) %>%
  filter(!is.na(Spending))  # remove missing spending values

# view the cleaned data
glimpse(spending_long)

# save your new dataframe
write_csv(spending_long, "data/spending_long.csv")

## Ashley you can stop here or see some examples below (including a model and some exploration)

## sample model: predict spending based on where the visitor is from and the interaction with category
model <- lm(Spending ~ Group * Category, data = spending_long) 
summary(model)

p <- ggplot(spending_long, aes(x = Year, y = Spending, color = Group)) +
  stat_summary(fun = mean, geom = "line") +
  facet_wrap(~ Category) +
  theme_minimal() +
  labs(title = "Yearly Trends in Spending by Group and Category")
ggplotly(p)

## we didn't have the time to try forecasting in class, but here is a simple way to predict spending 
model_time <- lm(Spending ~ Group * Category * Year, data = spending_long) # huge model

# create forecast data for 2024, 2025, and 2026 (on a subset only for now)
forecast_data <- expand_grid(
  Group = c("Visitors from China", "Visitors from Japan", "Visitors from Korea"),
  Category = c("Total Shopping"),
  Year = c(2024, 2025, 2026)
) %>%
  mutate(
    Group = factor(Group, levels = levels(spending_long$Group)),
    Category = factor(Category, levels = levels(spending_long$Category))
  )

# predict future spending
forecast_data$Predicted_Spending <- predict(model_time, newdata = forecast_data)

# view results
print(forecast_data)

# HUGE caveat: this assumes a linear relationship, and does not factor out "bad" years like 2020/2021 when these visitors couldn't really even travel here. 
# for a better forecasting, you'd do a lot more data manipulation and apply more powerful models.


