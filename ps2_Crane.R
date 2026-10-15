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

# 4.3: Repeat the month-over-month top increases/decreases but exclude all observations from 2020
# Remove 2020 observations, compute month-over-month change within each region, then
# find the top 5 (plus ties) increases and decreases.

unemp_no_covid <- unemp |> 
  filter(format(date, "%Y") != "2020")

unemp2_no_covid <- unemp_no_covid |> 
  arrange(region, date) |> 
  group_by(region) |> 
  mutate(mom_change = unemployment_rate - lag(unemployment_rate)) |> 
  ungroup()

unemp_top_increases_no_covid <- unemp2_no_covid |> 
  filter(!is.na(mom_change)) |> 
  slice_max(order_by = mom_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid <- unemp2_no_covid |> 
  filter(!is.na(mom_change)) |> 
  slice_min(order_by = mom_change, n = 5, with_ties = TRUE)

# Print results for quick inspection
unemp_top_increases_no_covid
unemp_top_decreases_no_covid
