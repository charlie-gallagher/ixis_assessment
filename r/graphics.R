library(tidyverse)
library(lubridate)

load('data/ixis.RData')

# TODO: Finalize graphics and narrative

# Graphic ideas

# For the first dataset, I'm most interested in transactions per session
# for both devices and browser type (in-app versus non-in-app)

# Quick change to the data that will be useful for graphing
month_device <- month_device %>% 
  mutate(
    date = ymd(glue::glue("{year}-{month}-01")),
    dim_deviceCategory = factor(dim_deviceCategory, 
                                levels = c("desktop", "tablet", "mobile"))
  )

# To present this, a simple line graph


# Text dataset
df <- month_device %>% 
  group_by(dim_deviceCategory) %>% 
  filter(date == max(date)) %>% 
  select(date, dim_deviceCategory, ecr)

month_device %>% 
  ggplot() + 
  geom_line(aes(x = date, y = ecr, 
                group = dim_deviceCategory, 
                color = dim_deviceCategory),
            size = 1.5) + 
  geom_text(data = df, aes(x = date + weeks(1), y = ecr, 
                           label = dim_deviceCategory,
                           color = dim_deviceCategory),
            hjust = 0) +
  scale_y_continuous(limits = c(0, 0.045), 
                     minor_breaks = NULL,
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) + 
  scale_x_date(expand = c(0, 0, 0.15, 0)) +
  scale_color_manual(values = c(grey(0), grey(0.4), grey(0.8))) + 
  guides(color = FALSE) +
  theme_minimal()





## Absolute and Relative Differences waterfalls ------
# Method: generate rectangles with geom_rect()
# Coordinates are given as *min and *max for x and y

# ymin = lag total quantity
# ymax = current total quantity
# xmin and xmax are some amount on either side of date (~2 weeks)
# Change first to full col

# Variable of interest: quantity sold
# Highlight: Most recent two months (make indicator)

# Problems: Months are an uneven (but accurate) way to distribute x-axis
# Maybe better to use categorical year-month


# TODO: Combine into a single dataset and use twice
# Theme TBD

month_diff %>% 
  select(date, qty, rd_qty) %>% 
  mutate(
    is_last2 = date %in% c(max(date), max(date) - months(1)),
    is_pos = rd_qty > 0 | date == min(date),
    xmin = date - weeks(2),
    xmax = date + weeks(2),
    ymin = lag(qty, default = 0),
    ymax = qty
  ) %>% 
  ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = interaction(is_pos, is_last2))) + 
  geom_line(aes(x = date, y = qty), color = 'red') + 
  geom_point(aes(x = date, y = qty), color = 'red') + 
  scale_fill_manual(values = c('#ff4f4d70', '#32a85270', '#ff4f4d', '#32a852')) + 
  scale_y_continuous(limits = c(0, 65000), expand = c(0,0)) + 
  theme_minimal()

# Relative differences
month_diff %>% 
  select(date, rd_qty) %>% 
  mutate(
    rd_qty = replace_na(rd_qty, 0),
    cum_rd_qty = cumsum(rd_qty),
    is_last2 = date %in% c(max(date), max(date) - months(1)),
    is_pos = rd_qty > 0 | date == min(date),
    xmin = date - weeks(2),
    xmax = date + weeks(2),
    ymin = lag(cum_rd_qty, default = 0),
    ymax = cum_rd_qty
  ) %>% 
  ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = interaction(is_pos, is_last2))) + 
  geom_step(aes(x = date - weeks(2), y = cum_rd_qty), color = 'red') + 
  geom_point(aes(x = date, y = cum_rd_qty), color = 'red') + 
  labs(
    title = "Change in Quantity Sold",
    subtitle = "Relative change since July, 2012",
    caption = "Charlie Gallagher  |  IXIS Assessment"
  ) + 
  scale_fill_manual(values = c('#ff4f4d70', '#32a85270', '#ff4f4d', '#32a852')) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  guides(fill = FALSE) + 
  theme_minimal()
