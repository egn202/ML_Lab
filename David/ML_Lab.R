library(dplyr)
library(lubridate)
library(ggplot2)


orders = read.csv('./data/Orders.csv')
returns = read.csv('./data/Returns.csv')

# Problem 1: Dataset Import & Cleaning ----

orders = orders %>% mutate(Profit=as.numeric(gsub("[\\$,]","",Profit)),
                           Sales = as.numeric(gsub("[\\$,]","",Profit)))

# Problem 2: Inventory Management ----
orders$Order.Month = as.factor(month(as.POSIXlt(orders$Order.Date, format="%m/%d/%Y")))

orders %>%
  ggplot() +
  geom_bar(aes(x = Quantity, y = Order.Month), stat = 'identity')

orders %>%
  ggplot() +
  geom_bar(aes(x = Quantity, y = Order.Month), stat = 'identity') +
  facet_wrap(~ Category)


# Problem 3: Why did customers make returns? ----
df <- inner_join(orders, returns, by = c('Order.ID', 'Region'))

#3.1 How much profit did we lose due to returns each year?

df$Order.Year = as.factor(paste0('20', year(as.POSIXlt(df$Order.Date, format="%m/%d/%Y"))))

df %>%
  group_by(Order.Year) %>%
  summarize(sum(Profit))
  
#3.2 How many customer returned more than once? more than 5 times?

df %>%
  group_by(Customer.ID, Order.ID) %>%
  summarise(nrow = n()) %>%
  filter(nrow > 1) %>%
  nrow()

# 547 customers returned more than once

df %>%
  group_by(Customer.ID, Order.ID) %>%
  summarise(nrow = n()) %>%
  filter(nrow > 5) %>%
  nrow()

# 41 customers returned more than 5 times 

#3.3 Which regions are more likely to return orders?

df %>%
  group_by(Region, Order.ID) %>%
  summarise(Total.Returns = 1) %>%
  ungroup() %>%
  group_by(Region) %>%
  summarise(Total.Returns = n()) %>%
  arrange(desc(Total.Returns)) %>%
  ungroup() %>%
  head(5) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Region, -Total.Returns), y = Total.Returns), stat = 'identity', fill = 'goldenrod') +
  ggtitle('Number of Returns by Region') +
  xlab('Region')


# In terms of quantity of returned orders, the top 5 regions highest number of returns in order are:
# Western Europe, Central America, Oceania, Western US, and Eastern US.


# 3.4
df %>% group_by(Category) %>%
  summarise(num_returns=n()) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Category, -num_returns), y= num_returns), stat='identity') +
  xlab("Category") + ylab("Number of Returns") +
  ggtitle("Most Frequent Returns by Category") + theme(plot.title = element_text(hjust = 0.5))

df %>% group_by(Sub.Category) %>%
  summarise(num_returns=n()) %>%
  arrange(desc(num_returns)) %>%
  head(5) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Sub.Category, -num_returns), y= num_returns), stat='identity') +
  xlab("Sub Category") + ylab("Number of Returns") +
  ggtitle("Most Frequent Returns by Subcategory") + theme(plot.title = element_text(hjust = 0.5))

