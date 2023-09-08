# Install pacman if needed
if (!require("pacman")) install.packages("pacman")

# load packages
pacman::p_load(pacman,tidyverse, openxlsx, modeltime, parsnip, rsample, timetk, broom, ggthemes)
#------------------------------------------------------------------------------------------------
##DESCRIPTIVE STATISTICS

#Show Dataset as is
str(data_sales)

#Create list of variables that need transformation to factors
fac_vars <- c("Segment", "Category", "Region", "Sub-Category", "Discount")

#Factor Segment, Category, Region, Discount variables
data_sales[,fac_vars] <- lapply(data_sales[,fac_vars], factor) 

#Check results
str(data_sales)

#Create New Formatted Date Column supplying origin argument
data_sales$new_date <- as.Date(data_sales$Order_Date, origin = "1899-12-30")

#Check results
str(data_sales$new_date)

## Familiarize with the dataset with visualizations

#From timetk package - visualize sales
data_sales %>%
  plot_time_series(new_date, Sales, .interactive = TRUE)

#Check data distribution
ggplot(data = data_sales, aes(x=Sales)) + geom_histogram() + theme_minimal()

#Distribution is skewed to the right, that means that this dataset has many 
#occurrences in the lower value cells; 

#Utilizing log transformation to normalize distribution of Sales;
ggplot(data = data_sales, aes(x=log(Sales))) + geom_histogram() + theme_minimal()

#Sales visually normalized after log transformation.

#Add new column Month to represent the sales months and evaluate monthly trends, if any.

data_sales$month <- format(as.Date(data_sales$new_date, format="%y/%m/%d"),"%m")
str(data_sales)

#Visualize Sales by month
ggplot(data = data_sales, aes(x=month, y = Sales)) + geom_boxplot() + theme_minimal()

#Many outliers, sales trend distributed very evenly through the months. December, February,
# June, July, November seem to have a little bit higher sales.

#Evaluate Sales by Category
data_sales %>%  
  group_by(Category) %>% 
  summarize(mean_sales = mean(Sales), median_sales = median(Sales), sd_sales = sd(Sales))

#Evaluate Sales by Customer Segment
data_sales %>%  
  group_by(Segment) %>% 
  summarize(mean_sales = mean(Sales), median_sales = median(Sales), sd_sales = sd(Sales))

#Evaluate Sales by Discount
data_sales %>%  
  group_by(Discount) %>% 
  summarize(mean_sales = mean(Sales), median_sales = median(Sales), sd_sales = sd(Sales))


#LINEAR REGRESSION 
install.packages("caTools")
library(caTools)

#Split dataset into training and testing
ind = sample.split(Y = data_sales$Sales, SplitRatio = 0.7)

#subsetting into Train data
train = data_sales[ind,]

#subsetting into Test data
test = data_sales[!ind,]

dim(train)
dim(test)

##Explore the data.
ggplot(train, aes(Sales)) + geom_density(fill="blue")
ggplot(train, aes(log(Sales))) + geom_density(fill="blue")
ggplot(train, aes(sqrt(Sales))) + geom_density(fill="blue")

#Make default model.
model1 = lm(log(Sales) ~ Category + Segment + Region + Discount, data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

#Adjust and remove insignificant variables
model2 = lm(log(Sales) ~ Category + Discount, data=train)
summary(model2)
par(mfrow=c(2,2))
plot(model2)


pred1 <- predict(model2, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$Sales)^2)/length(test$Sales))
c(RMSE = rmse, R2=summary(model2)$r.squared)

par(mfrow=c(1,1))
plot(test$Sales, exp(pred1))
#----------------------------------------------------------------------
#-------------------------------------------------------------------------
##FORECAST MODEL BUILDING
install.packages("lubridate") 
library("lubridate")

#AGGREGATE SALES DATA and GROUP BY MONTH to accommodate for analysis and forecasting
#Duplicate dataset for further adjustments
data_new <- data_sales

# Create year-month column
data_new$year_month <- floor_date(data_new$Order_Date,
                                   "month")
head(data_new) 
str(data_new)

library(dplyr)
data_new %>%
  mutate_at(vars("year_month"), funs(format(as.Date(.), "%Y-%m-%d")))
str(data_new)
library("dplyr") 

# Aggregate data and sum by month
data_aggr <- data_new %>%                        
  group_by(new_date) %>% 
  dplyr::summarize(value = sum(Sales)) %>% 
  as.data.frame()
head(data_aggr)  

plot(data_aggr)
summary(data_aggr)

library(tidyverse)
df <- data_aggr %>%
  group_by(month = lubridate::floor_date(new_date, 'month')) %>%
  summarize(sum_of_sales = sum(value))

#PLot historical monthly sales

plot(df, type = "o", col = "green",
     xlab = "Month", ylab = "Sum of Sales",
     main = "Total Monthly Sales")
#Inspect the data
summary(df)
class(df)
str(df)

#Converting data frame to time series
install.packages("xts")                      
library("xts")

tt <- ts(c(t(df[-1])), start = df$month[1], freq = 12)
#Inspect results
frequency(tt)
class(tt)
summary(tt)

#Inspect the data cycle and Boxplot of the cyclical sales
boxplot(tt~cycle(tt))

cycle(tt)

ts_tt <- ts(tt, frequency = 12)
d_tt <- decompose(ts_tt, "multiplicative")
plot(d_tt)

library(tseries)

#Running the additional test to make sure Sales data is stationary.
adf.test(tt)

#ADF test results: We cannot reject the null hypothesis because the p-value is not smaller than 0.05.
#This indicates that the time series is non-stationary. 
#It has some time-dependent structure and does not exhibit constant variance over time.


head(tt)
str(tt)
#Changing data to stationary (had to run double diff to get the applicable p value)
stationary_tt=diff(log(tt))
adf.test(stationary_tt)
dtt=diff(diff(tt))
adf.test(dtt)

#ARIMA model fitting for Time Series analysis and forecasting
library(forecast)
fit <- auto.arima(dtt)
fit
# Next 12 forecasted values
f_df <- forecast(fit, 12)
f_df


# plotting the graph with next
# 12 forecasted values
plot(forecast(fit, 12), xlab ="Period",
     ylab ="Sales",
     main ="Projected Monthly Sales Data", col.main ="darkgreen")