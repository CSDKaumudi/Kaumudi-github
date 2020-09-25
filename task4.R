#load the data
install.packages('dplyr')
install.packages('treemap')
library('dplyr')
library('magrittr')
library('treemap')
store_data=read.csv("C:/Users/Admin/Documents/R/SampleSuperstore.csv")
#column names
colnames(store_data)
#deleting unwanted rows
store_data=store_data[-c(3,6)]
#null values
sum(is.null(store_data))
#there are no null values
#looking at the summary of all columns
summary(store_data)
#there are few categorical variables and few continuous variables
# top few rows and columns of the data
head(store_data)
#visualisation packages
library('ggplot2')
#table of categorical variables
table(store_data$Ship.Mode)
ggplot(store_data,aes(x=Ship.Mode))+geom_bar(fill='Light Pink',color='Black')
#THere are four categories in ship mode- first class,second,standard and same day
#highest numbers are in standard class(6000),Second class(2000),first class(1700),same day(250)
table(store_data$Segment)
ggplot(store_data,aes(x=Segment))+geom_bar(fill='Light Blue',color='Black')
#there are 3 types consumer,corporate,home office- highest is consumer
table(store_data$Region)
ggplot(store_data,aes(x=Region))+geom_bar()+ggtitle('Total orders by region')
#Highest no of entries are from west region
#Category
table(store_data$Category)
ggplot(store_data,aes(x=Category))+geom_bar()+ggtitle('Total orders by Category')
#Higher count is  for Office supplies
#Quantity ordered
ggplot(store_data,aes(x=Quantity))+geom_bar()+ggtitle('Frequency distribution of quantity ordered')+scale_x_continuous(breaks = seq(1,15))
#2 and 3 items (quantity) are ordered most and as quantity increased ,frequency decreased gradually.
#percentage sales by category
#first group by category
grp_data= store_data %>% group_by(Category) %>% summarize(Sales=sum(Sales))
perct_data=round(grp_data$Sales/sum(grp_data$Sales)*100)
lbls=paste(grp_data$Category,perct_data)
lbls=paste(lbls,'%',sep = " ")
colors=c('SkyBlue','Pink','Yellow')
pie(grp_data$Sales,labels = lbls, main = 'Percentage Sales by category', col=colors)
#Higher contribution in sales is by Technology which is 36% ,followed by Furniture 32%,office supplies 31%
treemap(store_data,index = c("Category","Sub.Category"),vSize = 'Sales',vColor = 'Profit',type = 'value',align.labels=list(c("center","center"),c("left","top")))
#Highest number of sales is for Phones and lowest is for Labels.
#High profits are obtained from Copiers followed by Phones.
#Heavy losses are from Tables.Supplies seem to show no profit no loss.
#correlation and plot
cor_data=store_data[8:11]
cor(cor_data)
library('corrplot')
corrplot(cor(cor_data))
#We see strong relation between profit and sales,weak relation between quantity and sales and negative relation between 
#very weak relation between Profit and Quantity is observed
#This is the Exploratory Data Analysis of the given SuperStore data.