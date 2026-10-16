library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )
unemp <- unemp |>
  arrange(region, date) |> 
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_no_covid <- unemp |> 
  filter(year(date) != "2020")

unemp_top_increases_no_covid  <- unemp_no_covid |>
  slice_max(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid  <- unemp_no_covid |>
  slice_min(order_by = unemployment_rate_change, n = 5, with_ties = TRUE)

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  labs(title = "Unemployment Rates by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Unemployment Rate by Region",
    x = "Date",
    y = "Unemployment Rate"
  ) +
  theme_minimal()

ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  geom_col() + 
  labs(
    title = "Average Unemployment Rate by Region",
    x = "Region",
    y = "Average Unemployment Rate"
  ) +
  theme_minimal()

# this next section of code generates a scatter plot comparing unemployment rates with the change in the rate found going into the next month
# this is interesting to me because it highlights the cyclical nature of unemployment; when rates are high, they tend to decrease in the next month, and when rates are low, they tend to increase in the next month.
# it is also worth noting that there seem to be larger jumps in the upwards direction than there are in the changes trending downwards.

unemp$date <- as.Date(unemp$date)

ggplot(unemp, aes(x = unemployment_rate, y = lead(unemployment_rate_change), color = region)) +
  geom_point(alpha = 0.7, size = 1.8) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(
    title = "Unemployment Rate vs. Upcoming Change (points)",
    x = "Unemployment Rate",
    y = "Next Month Unemployment Change",
    color = "Region"
  ) +
  theme_minimal()
