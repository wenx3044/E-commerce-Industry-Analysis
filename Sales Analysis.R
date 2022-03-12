## Renford 4 Plots

dta <- read.csv("F:/BUS240/Report 2_R/GMV.csv")
Sys.setlocale("LC_TIME", "English")

library(tidyverse)
library(ggplot2)

new_dta <- dta %>% as_tibble(dta) %>% 
  mutate(order_date = order_date, order_month = substr(order_time, 1, 7)) 


# 1. Line Chart: Monthly GMV Trend
month_gmv <- new_dta %>% group_by(order_month) %>% 
  summarise_at(vars(payment_value),
               list(gmv = sum))
month_gmv <- month_gmv %>% filter(!order_month %in% c("","2016-10","2016-12","2018-09"))

plot1 <- ggplot(month_gmv, aes(order_month, gmv)) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(size = 12,angle = 45,hjust = 1))+
  scale_x_discrete(name="Month") +
  scale_y_continuous(name="GMV Per Month") +
  ggtitle("Monthly GMV Trend") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1200000) +
  scale_y_continuous(limits = c(0, 1200000), breaks = seq(0, 1200000, 200000))
ggsave("2_Monthly GMV Trend.png")




# 2 Bar Chart: Monthly Sales Number

removed_new_dta <-  new_dta %>% filter(!order_month %in% c("","2016-10","2016-12","2018-09"))

ggplot(removed_new_dta, aes(order_month)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 12,angle = 45,hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Monthly Sales Number")
ggsave("2_Monthly Sales Number.png")


# 3 line Chart: Average Transaction Value(ATV) per Month
monthly_order_num <- new_dta %>% group_by(order_month) %>% summarise(count = n()) %>%
  filter(!order_month %in% c("","2016-10","2016-12"))

monthly_atv <- month_gmv %>% inner_join(monthly_order_num, by = "order_month") %>% 
  mutate(atv = gmv/count)

ggplot(monthly_atv, aes(order_month, atv)) + 
  geom_line(group = 1) +
  ylim(130,170) +
  ggtitle("Average Transaction Value(ATV) per Month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Month", y = "Average Transaction Value(ATV)")
ggsave("3 Average Transaction Value(ATV) per Month.png")



## 4 facet line chart: Monthly GMV across Different Product Categories
dta2 <- read.csv("F:/BUS240/Report 2_R/Category GMV.csv")
dta2 <- dta2 %>% mutate(order_month = substr(order_approved_at,1,7))

category_month_gmv <- dta2 %>% group_by(order_month, product_category_name_english) %>% 
  summarise_at(vars(payment_value),list(gmv = sum)) %>%
  filter(order_month != "")

# Select top 10 category
category_gmv <- dta2 %>% group_by(product_category_name_english) %>% 
  summarise_at(vars(payment_value),list(gmv = sum))

top_cate <- category_gmv %>% arrange(desc(gmv)) %>% top_n(10)

top_10_category <- c(top_cate$product_category_name_english)

category_month_gmv <- category_month_gmv %>% filter(product_category_name_english %in% top_10_category)


cate_gmv <- ggplot(category_month_gmv, aes(order_month, gmv, color = product_category_name_english)) +
  geom_line(group = 1) +
  facet_wrap(vars(product_category_name_english)) +
  theme(axis.text.x = element_text(size = 12,angle = 45,hjust = 1)) +
  scale_x_discrete(breaks = c("2017-01","2017-04","2017-07","2017-10","2018-01","2018-04","2018-07")) +
  ggtitle("Monthly GMV across Different Product Categories")
ggsave("4_Monthly GMV across Different Product Categories.png")


##5 Ellipse Chart: The Relationship between weekly num of customers and weekly ATV 
customer_dta <- dta2 %>% 
  select(customer_id, order_id, order_approved_at, payment_value) %>%
  filter(!is.na(order_approved_at)) %>%
  mutate(order_date = as.Date(order_approved_at, "%Y-%m-%d")) %>%
  mutate(order_year = format(order_date, "%Y")) %>%
  filter(order_year != "2016")  %>%
  mutate(order_month = format(order_date, "%Y-%m")) %>%
  mutate(order_weekday = weekdays(order_date, abbreviate = TRUE)) %>%
  mutate(week = cut.Date(order_date, breaks = "1 week", label = FALSE))
  

orders_per_customer <- customer_dta %>%
  filter(order_year != 2016) %>%
  group_by(order_month) %>%
  summarize(num_of_orders = n(),
            num_of_customers = n_distinct(customer_id),
            num_of_orders_per_customer = round(num_of_orders/num_of_customers,2))

orders_and_atv <- merge(orders_per_customer, monthly_atv, by = "order_month") 

## per week sales VS atv
weekly_orders_per_customer <- customer_dta %>%
  filter(order_year != 2016) %>%
  group_by(week) %>%
  summarize(num_of_orders = n(),
            num_of_customers = n_distinct(customer_id),
            num_of_orders_per_customer = round(num_of_orders/num_of_customers,2))

weekly_gmv <- new_dta %>% 
  filter(!is.na(order_approved_at)) %>%
  mutate(order_date = as.Date(order_approved_at, "%Y-%m-%d")) %>%
  mutate(order_year = format(order_date, "%Y")) %>%
  filter(order_year != 2016) %>%
  mutate(week = cut.Date(order_date, breaks = "1 week", label = FALSE)) %>% 
  group_by(week) %>% 
  summarize(weekly_sum = sum(payment_value),
            weekly_num = n()) %>% 
  mutate(weekly_atv = weekly_sum/weekly_num)

weekly_orders_per_customer <- merge(weekly_orders_per_customer, weekly_gmv, by = "week")


ggplot(weekly_orders_per_customer, aes(num_of_customers, weekly_atv)) + 
  geom_point() +
  stat_ellipse() +
  ggtitle("The Relationship between weekly num of customers and weekly ATV") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Num of Customers.", y = "Weekly ATV")
ggsave("5_The Relationship between weekly num of customers and weekly ATV.png")



## 6 Scattor plot: Weekly GMV Trend of Olist
ggplot(weekly_orders_per_customer, aes(week,weekly_sum)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Weekly GMV Trend of Olist") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week No.", y = "Weekly GMV")
ggsave("6_Weekly GMV Trend of Olist.png")
