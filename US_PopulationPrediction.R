#Simple linear regression
datasets <- read.csv("US_Population.csv")

install.packages('ggplot2')
library(ggplot2)
ggplot(datasets, aes(Year, Population))+
geom_col() +
  ggtitle('Year vs Population of US 1979-2020') +
  xlab('Year') +
  ylab('Population (Millions)')

#splitting data in training set and test set
install.packages('caTools')
library(caTools)
set.seed(123)
split =sample.split(datasets$Population , SplitRatio = 2/3)
split
training_set = subset(datasets, split == TRUE )
test_set = subset(datasets , split == FALSE)

#fitting simple linear regressor 
regressor = lm(formula = Population ~ Year , data = training_set)
summary(regressor)

#Predit the data
y_pred = predict(regressor , newdata = test_set)
summary(y_pred)

#Visualising the Training set result
coords1 = paste(training_set$Year,training_set$Population,sep=",")
ggplot()+
  geom_point(aes( x = training_set$Year , y = training_set$Population) , colour = 'red') +
  geom_line(aes(x = training_set$Year , predict(regressor , newdata = training_set)) , color = 'green') +
  geom_label(aes(training_set$Year+3.5,training_set$Population, label = coords1)) +
  ggtitle('Year vs Population of US 1979-2020') +
  xlab('Year') +
  ylab('Population (Millions)')


coords2 = paste(test_set$Year,test_set$Population,sep=",")
ggplot()+
  geom_point(aes( x = test_set$Year , y = test_set$Population) , colour = 'red') +
  geom_line(aes(x = training_set$Year , predict(regressor , newdata = training_set)) , color = 'blue') +
  geom_label(aes(test_set$Year+3.5,test_set$Population, label = coords2)) +
  ggtitle('Year vs Population of US 1979-2020') +
  xlab('Year') +
  ylab('Population (Millions)')