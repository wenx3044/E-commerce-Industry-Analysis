library(tidyverse)
library(ggplot2)
library(mvoutlier)
library(ggpmisc)

dta2 <- read.csv("F:/BUS240/Report 2_R/Category GMV.csv")

### Report3 - 1. Clusters -- clusters low num and low sales // geom_text
ggplot(weekly_orders_per_customer, aes(weekly_num, weekly_sum)) +
  geom_point() 
km <- kmeans(weekly_orders_per_customer, 3, iter=20)
ggplot(weekly_orders_per_customer, aes(weekly_num, weekly_sum,color = km$cluster)) +
  geom_point() +
  ggtitle("Clusters for Weekly Sales Performance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week Number", y = "Weekly GMV") +
  geom_text(label = rownames(weekly_orders_per_customer),
            check_overlap = T)

## 2. Boxplot without outliers -- Price Distribution across Different Product Categories
ggplot(top_10_dta2, aes(product_category_name_english, price, fill = product_category_name_english)) +
  geom_boxplot( alpha = 0.8,
                outlier.shape = NA,
                notch=TRUE,
                notchwidth = 0.8,
                outlier.size=3) +
  scale_y_log10() +
  theme(axis.text.x = element_text(size = 12,angle = 45,hjust = 1)) +
  ggtitle("Price Distribution across Different Product Categories") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Product Categories", y = "Prices for SKUs within every product category") 

##3. reg with outliers -- The relationship between the number of sellers and sales performance for each product category         
dta2 <- dta2 %>% 
  filter(!is.na(order_approved_at)) %>%
  mutate(order_date = as.Date(order_approved_at, "%Y-%m-%d")) %>%
  mutate(order_year = format(order_date, "%Y")) %>%
  filter(order_year != "2016")  %>%
  mutate(order_month = format(order_date, "%Y-%m")) %>%
  mutate(order_weekday = weekdays(order_date, abbreviate = TRUE)) %>%
  mutate(week = cut.Date(order_date, breaks = "1 week", label = FALSE))

sellers_each_cate <- dta2 %>%
  filter(order_year != 2016) %>%
  group_by(product_category_name_english) %>%
  summarize(num_of_sellers = n_distinct(seller_id),
            sales_sum_each_cate = sum(payment_value))
sellers_each_cate <- as.data.frame(sellers_each_cate)

ggplot(sellers_each_cate, aes(num_of_sellers, sales_sum_each_cate, main="With Outliers")) +
  geom_point() +
  geom_text(label = rownames(sellers_each_cate),
            check_overlap = T) +
  geom_smooth(method = 'lm', se = FALSE) +
  ggtitle("The relationship between the number of sellers and sales performance for each product category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "The Number of Sellers within each product category", y = "Different GMVs for each product category")
#  coord_cartesian(ylim = quantile(sellers_each_cate$sales_sum_each_cate, c(0.1, 0.9)))


# outliers <- sign2(cbind(sellers_each_cate$num_of_sellers, sellers_each_cate$sales_sum_each_cate))
# outliers
# viz <- ggplot(income_payment, aes(income_per_capital, sum_of_payment, color = outliers$wfinal01))+
#   geom_point(size=3)
# viz
#  geom_smooth()

##4. reg without outliers -- The Relationship between Product Abundance and Sales Performance 

products_each_cate <- dta2 %>%
  filter(order_year != 2016) %>%
  group_by(product_category_name_english) %>%
  summarize(num_of_products = n_distinct(product_id),
            sales_sum_each_cate = sum(payment_value))

products_each_cate <- as.data.frame(products_each_cate)

plot2 <- ggplot(products_each_cate, aes(num_of_products, sales_sum_each_cate)) +
  geom_point() +
  geom_smooth(method= 'lm' , color="red", se=FALSE, formula = y~x) +
  ggtitle("The Relationship between Product Abundance and Sales Performance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "The Number of SKUs within each product category", y = "Different GMVs for each product category") +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")), 
               parse = TRUE)   


reg1 <- lm(sales_sum_each_cate~num_of_products,products_each_cate)
summary(reg1)

##3 Sales performance vs product name length & product description length & product photos quantity
product_dta <-read.csv("F:/BUS240/raw data/olist_products_dataset.csv")
dta3 <- merge(dta2, product_dta, by = "product_id") 
sales_each_name <- dta3 %>%
  filter(order_year != 2016) %>%
  group_by(product_name_lenght.x) %>%
  summarize(sales_each_name_len = sum(payment_value),
            sales_num_each_name_len = n())

# plot 3 without outliers -- The Relationship between Product Name Length and Sales Performance
plot1 <- ggplot(sales_each_name, aes(product_name_lenght.x, sales_num_each_name_len)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 2) +
  #  xlim(0,60) +
  ggtitle("3.The Relationship between Product Name Length and Sales Performance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Product Name Length", y = "Sales Volume for Each Product Name Length")   # outliers
