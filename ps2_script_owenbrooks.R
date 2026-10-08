library(tidyquant)
library(tidyverse)
# 
# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")


unemp <-  unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "US",
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

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

unemp_recent_summary <- unemp |>
  filter(date >= as.Date("2020-01-01")) |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )
unemp_top_increases <- unemp |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(region, desc(unemployment_rate_change))

unemp_top_decreases <- unemp |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(region, unemployment_rate_change)

unemp_top_increases_no_covid <- unemp |>
  filter(date < as.Date("2020-03-01")) |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(region, desc(unemployment_rate_change))
unemp_top_decreases_no_covid <- unemp |>
  filter(date < as.Date("2020-03-01")) |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(region, unemployment_rate_change)
unemp_line_plot <- unemp |>
  ggplot(aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rate Over Time by Region",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()
unemp_line_plot

unemp_line_plot_indv <- unemp |>
  ggplot(aes(x = date, y = unemployment_rate)) +
  geom_line() +
  facet_wrap(~region) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  labs(
    title = "Unemployment Rate Over Time by Region",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()
unemp_line_plot_indv

unemp_bar_chart <- unemp_summary |>
  ggplot(aes(x = region, y = mean_rate, fill = region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Unemployment Rate by Region",
    x = "Region",
    y = "Mean Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
unemp_bar_chart


us <- unemp |>
  filter(region == "US") |>
  select(date, us_rate = unemployment_rate)

unemp_gap_plot <- unemp |>
  filter(region != "US") |>
  left_join(us, by = "date") |>
  mutate(gap = unemployment_rate - us_rate) |>
  ggplot(aes(x = date, y = gap, color = region)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  labs(title = "State unemployment relative to the US",
       subtitle = "Above zero = worse than the national rate",
       x = "Date", y = "Gap from US rate (percentage points)", color = "Region") +
  theme_minimal()

unemp_gap_plot
