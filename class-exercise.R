# 1) Load packages
library(dplyr)
library(ggplot2)

# 2) Import data
data_raw <- read.csv(
  "shopping_behavior.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 3) Remove duplicates
data_unique <- data_raw %>%
  distinct(.keep_all = TRUE)

# 4) Create age groups
data_age <- data_unique %>%
  mutate(
    age_group = case_when(
      Age < 25 ~ "Under 25",
      Age >= 25 & Age <= 40 ~ "25–40",
      Age > 40 ~ "Over 40",
      TRUE ~ "Unknown"
    )
  )

# 5) Calculate average purchase
summary_table <- data_age %>%
  group_by(age_group) %>%
  summarise(
    avg_purchase = mean(`Purchase Amount (USD)`, na.rm = TRUE),
    n_customers = n(),
    .groups = "drop"
  ) %>%
  arrange(age_group)

# 6) Round values
summary_table$avg_purchase <- round(summary_table$avg_purchase, 2)

# 7) Display results
print(summary_table)

# 8) Simple plot
ggplot(summary_table, aes(x = age_group, y = avg_purchase)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Purchase Amount by Age Group",
    x = "Age Group",
    y = "Average Spend (USD)"
  ) +
  theme_minimal()
