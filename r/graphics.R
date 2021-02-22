library(tidyverse)
library(lubridate)

load('data/ixis.RData')



# Month/device over time ---------
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
month_device_text <- month_device %>% 
  group_by(dim_deviceCategory) %>% 
  filter(date == max(date)) %>% 
  select(date, dim_deviceCategory, ecr)

p1 <- month_device %>%
# month_device %>% 
  ggplot() + 
  geom_line(aes(x = date, y = ecr, 
                group = dim_deviceCategory, 
                color = dim_deviceCategory),
            size = 1.5) + 
  geom_text(data = month_device_text, aes(x = date + weeks(1), y = ecr, 
                           label = dim_deviceCategory,
                           color = dim_deviceCategory),
            hjust = 0) +
  labs(
    title = "Consumer Response across Devices",
    subtitle = "Transactions per Session, 2012-2013",
    caption = "Charlie Gallagher  |  IXIS Assessment"
  ) + 
  scale_y_continuous(name = "ECR (transactions / session)",
                     limits = c(0, 0.045), 
                     minor_breaks = NULL,
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) + 
  scale_x_date(expand = c(0, 0, 0.15, 0)) +
  scale_color_brewer(type = 'qual', palette = 3, direction = -1) + 
  guides(color = FALSE) +
  theme_light() + 
  theme(
    axis.title.x = element_blank()
  )

ggsave('img/month_device.png', plot = p1, height = 7, width = 6)



# In-app browser consumer response --------
in_app_text <- tibble(
  dim_deviceCategory = c('desktop', 'mobile', 'mobile', 'tablet', 'tablet'),
  in_app = c(F,F,T,F,T),
  x = ymd('2013-06-01'),
  y = c(0.032, 0.016, 0.007, 0.028, 0.012),
  text = c('Standard Browser', 'In-app Browser')[c(1, 1, 2, 1, 2)]
)


p2 <- in_app %>% 
  filter(tps != 0) %>% 
  mutate(date = ymd(glue::glue("{year}-{month}-01"))) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = tps, group = in_app, color = in_app),
            size = 1) + 
  geom_text(data = in_app_text, aes(x = x, y = y, label = text,
                                    color = in_app),
            hjust = 1) + 
  labs(
    title = "Consumer response in-app and not in-app",
    subtitle = "Transactions per Session for in-app browsers and non-in-app browsers",
    caption = "Charlie Gallagher  |  IXIS Assessment"
  ) + 
  scale_y_continuous(name = "ECR (transactions / session)", 
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(type = 'qual', palette = 2, direction = 1) +
  guides(color = FALSE) +
  facet_wrap(vars(dim_deviceCategory)) + 
  theme_light()

ggsave('img/in_app.png', height = 6, width = 8)


## Relative Differences waterfalls ------
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



# Relative differences
p3 <- month_diff %>% 
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
  geom_step(aes(x = date - weeks(2), y = cum_rd_qty), color = 'black') + 
  # geom_point(aes(x = date, y = cum_rd_qty), color = 'red') + 
  labs(
    title = "Change in Quantity Sold",
    subtitle = "% change since July, 2012",
    caption = "Charlie Gallagher  |  IXIS Assessment"
  ) + 
  scale_fill_manual(values = c('#ff4f4d70', '#32a85270', '#ff4f4d', '#32a852')) + 
  scale_y_continuous(name = "% Change in Quantity Sold", 
                     labels = scales::percent_format()) + 
  guides(fill = FALSE) + 
  theme_light() + 
  theme(
    axis.title.x = element_blank()
  )

ggsave('img/quantity.png', height = 6, width = 8)




# Supplemental graphs --------

# Monthly sessions up in 2013
ggplot(month_device) + 
  geom_line(aes(x = date, y = sessions, group = dim_deviceCategory,
                color = dim_deviceCategory))

# In-app sessions are also up
ggplot(in_app) + 
  geom_line(aes(x = paste0(year, month), y = sessions, 
                group = interaction(in_app, dim_deviceCategory),
                color = in_app))

# Absolute differences in quantity sold
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