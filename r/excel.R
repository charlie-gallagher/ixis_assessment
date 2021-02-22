library(tidyverse)
library(janitor)
library(lubridate)
library(openxlsx)

# TODO: Make data dictionary (append to README)
# TODO: Add dataset for in-app browser data

# Get data ---------
cart <- read_csv("data/DataAnalyst_Ecom_data_addsToCart.csv")
session <- read_csv("data/DataAnalyst_Ecom_data_sessionCounts.csv")

# Make date variables
session <- mutate(session, date = mdy(dim_date))
cart <- mutate(cart, date = ymd(glue::glue("{dim_year}-{dim_month}-01")))


# Sheet 1: Month/device aggregation --------

# "The first sheet should contain a Month * Device aggregation of the data 
# with the following metrics: Sessions, Transactions, QTY, and ECR 
# (= Transactions / Sessions)."

month_device <- session %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(year, month, dim_deviceCategory) %>% 
  summarize(
    sessions = sum(sessions),
    transactions = sum(transactions),
    qty = sum(QTY)
  ) %>% 
  mutate(ecr = transactions / sessions) %>% # ECR: Efficient Consumer Response
  arrange(dim_deviceCategory)
  
  
# Supplementary data: in-app browsers vs non-in-app browsers
in_app <- session %>% 
  mutate(
    year = year(date), 
    month = month(date),
    in_app = grepl("in-app", dim_browser)
  ) %>% 
  group_by(in_app, dim_deviceCategory, year, month) %>% 
  summarize(
    n = n(),
    sessions = sum(sessions),
    transactions = sum(transactions),
    qty = sum(QTY)
  ) %>% 
  mutate(
    tps = transactions / sessions,
    qpt = qty / transactions
  ) %>% 
  group_by(dim_deviceCategory) %>% 
  mutate(
    pct_sessions = round(sessions / sum(sessions), digits = 2),
    pct_transactions = round(transactions / sum(transactions), digits = 2),
    pct_qty = round(qty / sum(qty), digits = 2)
  ) %>% 
  arrange(dim_deviceCategory)

# Sheet 2: Month aggregation, including `cart` --------

# "The second sheet should contain a Month over Month comparison (for the most 
# recent two months in the data) for all available metrics (including Adds to 
# Cart), showing: the most recent month’s value, the prior month’s value, and 
# both the absolute and relative differences between them."

month_diff <- session %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month) %>% 
  summarize(
    sessions = sum(sessions),
    transactions = sum(transactions),
    qty = sum(QTY),
    .groups = "drop"
  ) %>% 
  left_join(cart, by = c("year" = "dim_year", "month" = "dim_month")) %>% 
  mutate(
    tps = transactions / sum(transactions),
    d_sessions = sessions - lag(sessions),
    d_transactions = transactions - lag(transactions),
    d_qty = qty - lag(qty),
    d_atc = addsToCart - lag(addsToCart),
    rd_sessions = d_sessions / lag(sessions),
    rd_transactions = d_transactions / lag(transactions),
    rd_qty = d_qty / lag(qty),
    rd_atc = d_atc / lag(addsToCart)
  ) %>% 
  # Arrange columns
  select(
    date, year, month, contains("sessions"), contains("transactions"),
    contains("qty"), contains("atc")
  )

# Reduce to only last two months
# use max() instead of doing this by hand to keep it general
month_diff_last2 <- filter(month_diff, 
                           date %in% c(max(date), max(date) - months(1)))


# Make Excel --------
ixis <- list(
  "month_device" = month_device,
  "month_diff" = month_diff_last2,
  "in_app" = in_app
)

write.xlsx(ixis, file = "data/ixis_assessment_table.xlsx", asTable = TRUE)

# Save data -----
save(cart, session, month_device, month_diff, in_app, file = "data/ixis.RData")
