library(ggplot2)
library(dplyr)
library(lubridate)

orders = read.csv('Orders.csv')
returns = read.csv('Returns.csv')
View(orders)
View(returns)

#Part 1

#Problem 1
orders = orders %>% mutate(Profit=as.numeric(gsub("[\\$,]","",Profit)),
                  Sales=as.numeric(gsub("[\\$,]","",Sales)))

#Problem 2

orders$Order.Month = as.factor(month(as.POSIXlt(orders$Order.Date, format="%m/%d/%Y")))
orders$Order.Year = as.factor(paste0('20', year(as.POSIXlt(orders$Order.Date, format="%m/%d/%Y"))))

orders %>% ggplot() + geom_bar(aes(x=Order.Month, y=Quantity), stat = "identity") + facet_wrap('Category')

#Problem 3

newdf = inner_join(orders,returns, by=c('Order.ID','Region'))
View(newdf)


length(unique(newdf$Order.ID))

#1
newdf %>% group_by(Order.Year) %>% 
  summarise(sum(Profit))

#2

newdf %>% group_by(Order.ID, Customer.ID) %>% 
  summarise(nrow=n()) %>% 
  filter(nrow > 1) %>% 
  nrow()

newdf %>% group_by(Customer.ID, Order.ID) %>% 
  summarise(nrow=n()) %>% 
  filter(nrow > 5) %>% 
  nrow()

#3
newdf %>%
  group_by(Region, Order.ID) %>%
  summarise(Total.Returns = 1) %>%
  ungroup() %>%
  group_by(Region) %>%
  summarise(Total.Returns = n()) %>%
  arrange(desc(Total.Returns)) %>%
  ungroup() %>%
  head(5) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Region, -Total.Returns), y = Total.Returns), stat = 'identity')

#4
newdf %>% group_by(Category) %>%
  summarise(num_returns=n()) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(Category, -num_returns), y= num_returns), stat='identity') +
  xlab("Category") + ylab("Number of Returns") + 
  ggtitle("Most Frequent Returns by Category") + theme(plot.title = element_text(hjust = 0.5))

newdf %>% group_by(Sub.Category) %>%
  summarise(num_returns=n()) %>% 
  arrange(desc(num_returns)) %>% 
  head(5) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(Sub.Category, -num_returns), y= num_returns), stat='identity') +
  xlab("Sub Category") + ylab("Number of Returns") + 
  ggtitle("Most Frequent Returns by Subcategory") + theme(plot.title = element_text(hjust = 0.5))


#Part 2

#Problem 4

#Step 1
retdf = left_join(orders,returns, by=c('Order.ID','Region'))
retdf$Returned = ifelse(is.na(retdf$Returned), 'No', retdf$Returned)

#Step 2
retdf$Order.Date = as.Date(retdf$Order.Date, format="%m/%d/%y")
retdf$Ship.Date = as.Date(retdf$Ship.Date, format="%m/%d/%y")
retdf <- retdf %>% mutate(Process.Days= Ship.Date - Order.Date)

#Step 3
retdf <- retdf %>% 
  mutate(Returned = ifelse(Returned == 'Yes',1,0)) %>% 
  group_by(Product.ID) %>% 
  mutate(Num_Returns=sum(Returned))

retdf = retdf[,c(1, 7, 10, 15, 17, 18, 20:28)]
#Problem 5
#retdf$Order.Priority = as.numeric(factor(retdf$Order.Priority, levels = c('Critical', 'High', 'Medium', 'Low')))
#retdf$Ship.Mode = as.numeric(factor(retdf$Ship.Mode, levels = c('Same Day', 'First Class', 'Second Class', 'Standard Class')))
#retdf$Order.Month = as.numeric(retdf$Order.Month)

retdf$Returned = ifelse(retdf$Returned == 'Yes', 1, 0)

library(tidyverse)
library(caret)
library(glmnet)

# 80/20 train/test
set.seed(0)
index = sample(1:nrow(retdf), size = nrow(retdf) * 0.8)
logit = glm(Returned ~ ., data = retdf[index, ], family = 'binomial')
prob = predict(logit, retdf[-index, ], type = "response")
mean((prob>=0.5) == (retdf$Returned[-index] == 'Yes'))


test = createDataPartition(retdf$Returned, list = F, times = 1, p = 0.8)
test.set = retdf[test,]
train.set = retdf[-test,]
nrow(test.set) / nrow(retdf)
nrow(train.set) / nrow(retdf)


folds = createFolds(retdf$Returned, 5)
str(folds)
n=5
accuracy = numeric(n)
for(i in 1:n){
  index = -folds[[i]]
  logit = glm(Returned~., data = retdf[index, ],
              family = 'binomial')
  prob = predict(logit, retdf[-index, ], type="response")
  accuracy[i] = mean(
    (prob>=0.5) == (retdf$Returned[-index]=='Yes')
  )
}
accuracy
3:26
retdf$Returned = as.factor(retdf$Returned)
