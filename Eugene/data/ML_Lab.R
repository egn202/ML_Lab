library(ggplot2)
library(dplyr)
library(lubridate)

orders = read.csv('Orders.csv')
returns = read.csv('Returns.csv')


orders = orders %>% mutate(Profit=as.numeric(gsub("[\\$,]","",Profit)))

orders$Order.Month = as.factor(month(as.POSIXlt(orders$Order.Date, format="%m/%d/%Y")))
orders$Order.Year = as.factor(year(as.POSIXlt(orders$Order.Date, format="%m/%d/%Y")))

orders %>% ggplot() + geom_bar(aes(x=Order.Month, y=Quantity), stat='identity')

newDF = right_join(orders,returns, by='Order.ID')
#2220 long now

newDF %>% group_by(Order.ID, Customer.ID) %>% summarize(n=n())










newDF %>% group_by(Order.ID) %>% summarise(n=n()) %>% filter(n>1) %>% nrow()
newDF %>% group_by(Order.ID) %>% summarise(n=n()) %>% filter(n>5) %>% nrow()

newDF %>% group_by(Order.ID, Customer.ID) %>% summarise(n=n())


returns = newDF %>%
  group_by(Customer.ID) %>%
  summarise(nrow = n()) %>%

unique.df = left_join(returns, newDF, by='Order.ID')
  

unique_orders = newDF %>% group_by(Order.ID)


newDF %>%
  group_by(Order.ID) %>%
  group_by(Customer.ID) %>% 
  summarise(nrow = n()) %>%
  nrow()
