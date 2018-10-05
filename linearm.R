library("caret")
library("dplyr")
library("magrittr")

p<-read.csv("trainn.csv")#read read the data using read.csv() and assign it to a dataframe
colSums(is.na(p))#check for nullvalues
p<-p[,-c(7,58,73,74,75)]#omit the columns with null values by making them vectors and assign them to a dataframe
p<-na.omit(p)
View(p)
i<-createDataPartition(p$SalePrice,p=0.9,list = FALSE)%>%c()#create data partition select your Y,im having a 90-10 test training ratio,i dont want a list so its false,andIam pipelining it and vectoring
train<-p[i,]
test<-p[-i,]
#create your model assign it to a dataframe as lm(linear model)(Y~.,train)
Model4<-lm(SalePrice~OverallQual+LotArea+Street+GarageArea+Neighborhood+GrLivArea+LotFrontage+KitchenQual+LotShape+CentralAir+Fireplaces+YearBuilt ,train)
summary(Model4)
d<-predict(Model4,test)#predict is a function and were checking it with our model vs test
RMSE(d,test$SalePrice)#to check the root mean square error
