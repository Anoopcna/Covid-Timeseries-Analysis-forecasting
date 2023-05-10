
## NAme : Anoop Nair
## Student id: R00223644

### set path ####

setwd('D:\\STUDY MATERIAL\\Masters DS & Analytics\\Semester 2\\Time Series\\Project')

#### import libraries ###

#install.packages('imputeTS')
# install.packages('zoo')
# install.packages('msts')

library(readxl)
library(imputeTS)
library(zoo)
library(xts)
library(lubridate)
library(astsa)
library(tseries)
library(forecast)
library(ggplot2)

'************** Data cleaning and manipulation ******************'

'#### import excel file 
## while importing the entire file I was getting memory error so I just chose my country as UAE and took the data points of that country'

my_data = read_excel("UAE_Data.xlsx")
#my_data = read_excel("Chile_data.xlsx")
#str(my_data)

'#### To get columns date and new_tests for '

UAE_df = my_data[c("date", "new_tests")]
#nrow(UAE_df) 1174
#head(UAE_df,100)

'## remove all the values before 28th Jan and after  as all are Null values'


UAE_data = UAE_df[UAE_df$date>'2020-01-28' & UAE_df$date<='2022-06-23', ]


'### find how many NA values are there between the first data point and the last data point '

na_values = UAE_data[is.na(UAE_data$new_tests), ]

nrow(na_values) # 31

'##### imputation of the missing values '

'#Linear Interpolation: This method involves replacing missing values 
#with a linear interpolation between the previous and next observed values.'

UAE_data$Impnew_tests = as.integer(na_interpolation(UAE_data$new_tests))

#any(is.na(UAE_data$Impnew_tests)) # FALSE

'####### Final dataset creation ####'

daily_data = UAE_data[c("date", "Impnew_tests") ]


str(daily_data)

'###### convert the data fromat #########'

daily_data$date <- strptime(daily_data$date, "%Y-%m-%d" )
daily_data$date <- as.POSIXct(daily_data$date)

str(daily_data)


'###### convert the dataframe into a time series ########'
'### taking the data from 03 of march because the bew week start from there'


#daily_ts <- ts(daily_data$Impnew_tests, start = c("2020-02-03"), frequency = 7)
daily_ts <- ts(daily_data$Impnew_tests, frequency = 7)
#daily_ts <- ts(daily_data$Impnew_tests, start = decimal_date(as.Date("2020-02-03")), frequency = 7)

autoplot(daily_ts) +
  ggtitle("Daily Time Series for UAE") +
  ylab('Numbe of tests')+
  theme(plot.title = element_text(hjust = 0.5))

#str(daily_ts)


acf(daily_ts)
pacf(daily_ts)
#autoplot(daily_ts)

#autoplot(decompose(daily_ts, "additive"))
##### summarize the time series

summary(daily_ts)

'##### plot and decompose daily time series'

autoplot(daily_ts)
autoplot(decompose(daily_ts,"additive"))
#autoplot(decompose(daily_ts,"multiplicative"))
autoplot(mstl(daily_ts))

'######## create a weekly time series from daily time series ####'

Week <- as.Date(cut(daily_data$date, "week"))
Weekly_data = aggregate(Impnew_tests ~ Week, daily_data, sum)

weekly_ts <- ts(Weekly_data$Impnew_tests, start = decimal_date(ymd("2020-02-03")), frequency = 52)

#weekly_ts <- ts(Weekly_data$Impnew_tests, frequency = 52)

autoplot(weekly_ts) +
  ggtitle("Weekly Time Series for UAE") +
  ylab('Numbe of tests')+
  theme(plot.title = element_text(hjust = 0.5))



summary(weekly_ts)
acf(weekly_ts)
pacf(weekly_ts)




autoplot(weekly_ts)
autoplot(decompose(weekly_ts,"additive"))
autoplot(decompose(weekly_ts,"multiplicative"))
plot(mstl(weekly_ts))
autoplot(decompose(weekly_ts))


'######## create a monthly time series from daily time series ####'


Monthly_data <- aggregate(Impnew_tests ~ format(date, "%Y-%m"), daily_data, sum)
names(Monthly_data)[1] <- "Month" # rename the column to date

monthly_ts <- ts(Monthly_data$Impnew_tests,start = decimal_date(ymd("2020-02-03")), frequency = 12)


autoplot(monthly_ts) +
  ggtitle("Monthly Time Series for UAE") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))



autoplot(monthly_ts)
summary(monthly_ts)
autoplot(decompose(monthly_ts,"additive"))
autoplot(decompose(monthly_ts,"multiplicative"))
plot(mstl(monthly_ts))

summary(monthly_ts)

'************** Preliminar analysis ******************'

'### summary stats and plots to describe the dataset'

### daily Data 

autoplot(daily_ts,main="Daily Time Series")
#autoplot(stl(daily_ts,s.window = 7))
summary(daily_ts)
boxplot(daily_ts,main ='Distribution of daily tests for UAE')
par(mfrow=(c(1,2)))
acf(daily_ts)
pacf(daily_ts)

par(mfrow=(c(1,3)))
autoplot(decompose(daily_ts))
autoplot(decompose(daily_ts,"additive"))
autoplot(decompose(daily_ts,"multiplicative"))
autoplot(stl(daily_ts,s.window = 7))
#autoplot(mstl(daily_ts))
#autoplot(stlf(daily_ts ))

#1) Looks like there is some trend and seasonlaity in the daily data.
#2) The mean value for it is 0.196 million and mean is around 0.192 million
#3)  The slow decrease in the ACF as the lags increase is due to the trend, while the “scalloped” 
#    shape is due to the seasonality. MA (0)
#4) There is a strong correlation between first four lags in PACF hence we can say is AR(4) with some seasonality in the inverse way
#5) The additive model captures the trend well and trend is significant component than seasonality 
#6) The amplitude of the variances increases towards the second half as 
#   more and more tests are done and the variability is more
#7) in the mstl we can see that the periodicity is 7 and maybe thats effecting the seasonality.
    #looks like some seasonality within adjacent timeperiods.
    # but there doesn't look to be any evident seasonality.


### weekly Data 
par(mfrow=c(1,1))
autoplot(weekly_ts, main="Weekly Time Series")
summary(weekly_ts)
boxplot(weekly_ts,main ='Distribution of Weekly tests for UAE')
par(mfrow=c(1,2))
acf(weekly_ts)
pacf(weekly_ts)
autoplot(decompose(weekly_ts)) # additve
autoplot(decompose(weekly_ts,"additive"))
autoplot(decompose(weekly_ts,"multiplicative"))
autoplot(mstl(weekly_ts,s.window = 'periodic'))
#autoplot(stlf(weekly_ts ))

#1) The plot looks like it has a trend and multilpicative  as the variances are increasing as progresses
#2) the median # of tests seems to be around 1.5 million in a week
#3) Weehly graph has a better slowly decreasing trend than the daily ACF
#4) This looks like AR(2)
#5) Trend is more significant, ther trend doesnt appear in first and last values.
# some seasonality is present repears yearly in the first half of the year.
#  
#6) 
#7) Trend values estimated for  all periods unlike decompose functions.and the data shows trend


#### monthly data 

autoplot(monthly_ts,main="Monthly Time Series")
summary(monthly_ts)
par(mfrow=c(1,1))
boxplot(monthly_ts,main ='Distribution of monthly tests for UAE')
par(mfrow=c(1,2))
acf(monthly_ts)
pacf(monthly_ts)
autoplot(decompose(monthly_ts)) # additive
autoplot(decompose(monthly_ts,"additive"))
autoplot(decompose(monthly_ts,"multiplicative"))
autoplot(mstl(monthly_ts,s.window = 'periodic'))
autoplot(stlf(monthly_ts ))


#1) Trend and multiplicative
#2) median is around 6.1 million tests in a month
#3) it decreases slowly , which shows some trend MA(0)
#4) only the immediate lag seems significant so its AR (1)
#5) Trend and seasonality present , trend is more significant

#6) 
#7) tend for the entire period , seasonality and trend present




########### Divide daily dataset into 3 ########

daily_data
plot(daily_ts, type="l")
autoplot(daily_ts)

daily_data[daily_data$Impnew_tests>200000,]
daily_data[daily_data$Impnew_tests==563330,]
max(daily_data$Impnew_tests)#563330
daily_ts

wave1=daily_data[daily_data$date<='2021-02-14',] # Steady Increase
wave2=daily_data[daily_data$date>'2021-02-14' & daily_data$date<='2022-01-25',] # unsteady increase
wave3=daily_data[daily_data$date>'2022-01-25',] # from top to slow decline


######## wave 1 analysis 

wave1
wave1_ts <- ts(wave1$Impnew_tests,frequency = 7)

autoplot(wave1_ts) +
  ggtitle("Wave1 Daily Time Series for UAE") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(wave1_ts,main="wave1 Time Series")
summary(wave1_ts)
par(mfrow=c(1,1))
boxplot(wave1_ts,main ='Distribution of Wave 1 for UAE')
par(mfrow=c(1,2))
acf(wave1_ts)
pacf(wave1_ts)
autoplot(decompose(wave1_ts))
autoplot(decompose(wave1_ts,"additive"))
autoplot(decompose(wave1_ts,"multiplicative"))
autoplot(mstl(wave1_ts))
autoplot(stlf(wave1_ts ))

#1) steep positive trend, multiplicative since different variances,seasonality exist.Not stationary.
#2) Median is around 60k.
#3)
#4) ACF is having slight decline each time , we can say its almost constant.
#5) PACF looks like an AR (1) or AR (2), small spike after every lag, but the importance reduces with time.
#6) Trend seem to have a bigger impact, decomposition nicely catching seasonality
#7) for mstl the random component are pretty steady around the mean , but the size of seasonality
#   varying alot.


###### wave 2 ######

wave2
wave2_ts <- ts(wave2$Impnew_tests,frequency = 7)

autoplot(wave2_ts) +
  ggtitle("Wave1 Daily Time Series for UAE") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))


autoplot(wave2_ts,main="wave2 Time Series")
summary(wave2_ts)
par(mfrow=c(1,1))
boxplot(wave2_ts,main ='Distribution of Wave 2 for UAE')
par(mfrow=c(1,2))
acf(wave2_ts)
pacf(wave2_ts)
autoplot(decompose(wave2_ts))
autoplot(decompose(wave2_ts,"additive"))
autoplot(decompose(wave2_ts,"multiplicative"))
autoplot(mstl(wave2_ts))
autoplot(stlf(wave2_ts ))


#1) The trend is varying with time not as steep a s wave 1, looks more additive in nature,
#2) meadian tests for this wave is around 0.2 million. 
#Has outliers still looks pretty normally distributed
#3) the acf curve is declining in more steeper way.
#4) Looks like an AR(1)
#5) Trend captured well and is more important.seasonality too , but less important.variances look fine
#6) for mstl() the seasonality factor is varying probably the periodicity of 7 is not the right one.

###### Wave 3 


wave3
wave3_ts <- ts(wave3$Impnew_tests,frequency = 7)

autoplot(wave3_ts,main="wave3 Time Series")

autoplot(wave3_ts) +
  ggtitle("Wave3 Daily Time Series for UAE") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

summary(wave3_ts)
par(mfrow=c(1,1))

boxplot(wave3_ts,main ='Distribution of Wave 3 for UAE')
par(mfrow=c(1,2))
acf(wave3_ts)
pacf(wave3_ts)
autoplot(decompose(wave3_ts))
autoplot(decompose(wave3_ts,"additive"))
autoplot(decompose(wave3_ts,"multiplicative"))
autoplot(mstl(wave3_ts))
autoplot(stlf(wave3_ts ))

#1) Initially a declining trend and slight increase after a point
#2) median around 0.3 million.
#3) MA(0), AR(1)
#4) trend captured, seasonality beautifully captured
#5) both additive and multiplicative captures the trend and seasonality well.
#6) mstl() does not capture the seaonality well and the random component seems to be some variable across


'**************************** TIME SERIES MODELING ****************************'

'# classical model implementation for 3 waves and full daily time series set'

daily_ts
wave1_ts
wave2_ts
wave3_ts

#######  For full daily data ##########

## exponential smoothing (SES)

ses_daily_ts <- ses(daily_ts)
plot(ses_daily_ts, main = "Simple Exponential Smoothing Forecast")
lines(fitted(ses_daily_ts), col = "red")
summary(ses_daily_ts)  # Aic 24118.63 RMSE 31538.65
accuracy(ses_daily_ts)

####### Holts method ()

hotl_daily_ts <- holt(daily_ts)
plot(hotl_daily_ts, main = "Holt's Method Forecast")
lines(fitted(hotl_daily_ts), col = "blue")
summary(hotl_daily_ts)  # Aic 24122.62 RMSE 31538.53
accuracy(hotl_daily_ts)

####### Holts winter method ####

hw_daily_ts <- hw(daily_ts)
plot(hw_daily_ts, main = "Holt winter Method Forecast")
lines(fitted(hw_daily_ts), col = "green")
summary(hw_daily_ts)  # Aic 23919.65  RMSE 27868.77
accuracy(hw_daily_ts)

'### Hoslts winter is best model for the entire model'


hw_model_daily_ma <- hw(daily_ts, seasonal = "multiplicative")
summary(hw_model_daily_ma)


###### plotting the graphs between additive and multiplicative ########
par(mfrow=c(2,1))

plot(hw_daily_ts, main = "Holt Winter Additive model (full model)")
lines(fitted(hw_daily_ts), col = "red")

plot(hw_model_daily_ma, main = "Holt's Multiplicative Forecast (full model)")
lines(fitted(hw_model_daily_ma), col = "green")



###### to train alpha for best model with best trend #######

###### alpha 

alphas <- seq(0.1, 0.95, by = 0.05)

models <- lapply(alphas, function(alpha) hw(daily_ts, alpha = alpha))
errors <- sapply(models, function(model) accuracy(model)[2])

plot(alphas, errors, type = "b", xlab = "Alpha", ylab = "RMSE", main = "Alpha Vs RMSE")

best_alpha <- alphas[which.min(errors)] #0.6

print(best_alpha)

#final_model <- hw(daily_ts, alpha = best_alpha)
#summary(final_model)
  

##### to check the best for seasonality (multiplicative / additive )



######### For wave 1 ######


## exponential smoothing (SES)

ses_wave1_ts <- ses(wave1_ts)
plot(ses_wave1_ts, main = "Simple Exponential Smoothing Forecast")
lines(fitted(ses_wave1_ts), col = "red")
summary(ses_wave1_ts)  # Aic 9593.080  RMSE 13929.34
accuracy(ses_wave1_ts)

####### Holts method ()

hotl_wave1_ts <- holt(wave1_ts)
plot(hotl_wave1_ts, main = "Holt's Method Forecast")
lines(fitted(hotl_wave1_ts), col = "blue")
summary(hotl_wave1_ts)  # Aic 9587.057 RMSE 13748.26
accuracy(hotl_wave1_ts)

####### Holts winter method ####

hw_wave1_ts <- hw(wave1_ts)
plot(hw_wave1_ts, main = "Holt winter Method Forecast")
lines(fitted(hw_wave1_ts), col = "green")
summary(hw_wave1_ts)  # Aic 9384.929  RMSE 10368.39
accuracy(hw_wave1_ts)


hw_model_wave1_ts_m <- hw(wave1_ts, seasonal = "multiplicative")
summary(hw_model_wave1_ts_m)


###### plotting the graphs between additive and multiplicative ########
par(mfrow=c(2,1))

plot(hw_wave1_ts, main = "Holt Winter Additive model (Wave 1)")
lines(fitted(hw_wave1_ts), col = "red")

plot(hw_model_wave1_ts_m, main = "Holt's Multiplicative Forecast (Wave 1)")
lines(fitted(hw_model_daily_ma), col = "green")




######### For wave 2 ######


## exponential smoothing (SES)

ses_wave2_ts <- ses(wave2_ts)
plot(ses_wave2_ts, main = "Simple Exponential Smoothing Forecast")
lines(fitted(ses_wave2_ts), col = "red")
summary(ses_wave2_ts)  # Aic 9305.515  RMSE 38398.95
accuracy(ses_wave2_ts)

####### Holts method ()

hotl_wave2_ts <- holt(wave2_ts)
plot(hotl_wave2_ts, main = "Holt's Method Forecast")
lines(fitted(hotl_wave2_ts), col = "blue")
summary(hotl_wave2_ts)  # Aic 9310.194 RMSE 38436.79
accuracy(hotl_wave2_ts)

####### Holts winter method ####

hw_wave2_ts <- hw(wave2_ts)
plot(hw_wave2_ts, main = "Holt winter Method Forecast")
lines(fitted(hw_wave2_ts), col = "green")
summary(hw_wave2_ts)  # Aic 9221.859  RMSE 33138.74
accuracy(hw_wave2_ts)


#### check for multiplicative
hw_model_wave2_ts <- hw(wave2_ts, seasonal = "multiplicative")
summary(hw_model_wave2_ts)


######### For wave 3 ######


## exponential smoothing (SES)

ses_wave3_ts <- ses(wave3_ts)
plot(ses_wave3_ts, main = "Simple Exponential Smoothing Forecast")
lines(fitted(ses_wave3_ts), col = "red")
summary(ses_wave3_ts)  # Aic 3929.756  RMSE 42829.9
accuracy(ses_wave3_ts)

####### Holts method ()

hotl_wave3_ts <- holt(wave3_ts)
plot(hotl_wave3_ts, main = "Holt's Method Forecast")
lines(fitted(hotl_wave3_ts), col = "blue")
summary(hotl_wave3_ts)  # Aic 3933.515 RMSE 42795.37
accuracy(hotl_wave3_ts)

####### Holts winter method ####

hw_wave3_ts <- hw(wave3_ts)
plot(hw_wave3_ts, main = "Holt winter Method Forecast")
lines(fitted(hw_wave3_ts), col = "green")
summary(hw_wave3_ts)  # Aic 3889.999  RMSE 35283.75
accuracy(hw_wave3_ts)




hw_model_wave3_ts <- hw(wave3_ts, seasonal = "multiplicative")
summary(hw_model_wave3_ts)



'############### ARIMA MOdelling ########################'

###### For full model ######

daily_ts

BoxCox.lambda(daily_ts) # 0.214 closer to zero
### formal tests  for checking stationary ##

adf.test(daily_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((daily_ts)) ## Not stationary

par(mfrow=c(1,2))
acf(daily_ts)
pacf(daily_ts)


autoplot(daily_ts, main="Daily Time series")

autoplot(daily_ts) +
  ggtitle("Full Daily Time Series for UAE") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(daily_ts))

###### differntiate seasonally #######

#diff_season_daily_ts = diff(diff(daily_ts,  differences = 7))


# remove variability 
log_daily_ts = log(daily_ts)

# Remove trend 
difflog_daily_ts = diff(log_daily_ts,1)


autoplot(difflog_daily_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))


autoplot(decompose(difflog_daily_ts))


adf.test(difflog_daily_ts, alternative = "stationary",k = 0) #
kpss.test((difflog_daily_ts)) ## 

par(mfrow=c(1,2))
acf(difflog_daily_ts)
pacf(difflog_daily_ts)

########## remove sesaonality

#seasdifflog_daily_ts = diff(difflog_daily_ts, lag=1, differences=7)

seasdifflog_daily_ts = diff(difflog_daily_ts,lag=7)

autoplot(difflog_daily_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

adf.test(seasdifflog_daily_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((seasdifflog_daily_ts)) ## stationary


#### check correlograms ###
par(mfrow=c(1,2))
acf(seasdifflog_daily_ts)
pacf(seasdifflog_daily_ts)



par(mfrow=c(1,1))
autoplot(decompose(seasdifflog_daily_ts)) +
  ggtitle("Decompose plot after doing transformations") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

## We can try few models like

#Model 1: ARIMA (5,1,2) (0,1,0)
#Model 2: ARIMA (5,1,2) (1,1,0) 
#Model 3: ARIMA (5,1,2) (0,1,1)



#### few  probable models ####

####### model 1  ########
model_arima1= arima(
  log_daily_ts, 
  order=c(5,1,2), 
  seasonal=c(order=c(0,1,0)))


summary(model_arima1) #AIC = -67.38 and RMSE 0.229204
checkresiduals(model_arima1)
ggtsdisplay(model_arima1$residuals)


model_arima2= arima(
  log_daily_ts, 
  order=c(5,1,2), 
  seasonal=c(order=c(1,1,0)))


summary(model_arima2)  #AIC = -228.16 and RMSE 0.2083846
checkresiduals(model_arima2)
ggtsdisplay(model_arima2$residuals)



model_arima3= arima(
  log_daily_ts, 
  order=c(5,1,2), 
  seasonal=c(order=c(0,1,1)))


summary(model_arima3)  #AIC = -382.27 and RMSE 0.1902913
checkresiduals(model_arima3)
ggtsdisplay(model_arima3$residuals)



#### apply auto arima

model_autoarima=auto.arima(log_daily_ts)
summary(model_autoarima) # ARIMA(5,2,3)(0,0,2)[7
checkresiduals(model_autoarima)
ggtsdisplay(model_autoarima$residuals)
 # AIC=-357.7 and RMSE 0.1937928


#*############ WAVE 1 ###############**###

wave1_ts

BoxCox.lambda(wave1_ts) # 0,377, some transformation is required

### formal tests  for checking stationary ##

adf.test(wave1_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((wave1_ts)) ## Not stationary

#### apply the same transformations as full model , i.e. taking log and differentiate

autoplot(decompose(wave1_ts))

autoplot(decompose(wave1_ts)) +
  ggtitle("Wave1 daily time series ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

###### differntiate seasonally #######



# remove variability 
log_wave1_ts = log(wave1_ts)

# Remove trend 
difflog_wave1_ts = diff(log_wave1_ts,1)


autoplot(difflog_wave1_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(difflog_wave1_ts))

adf.test(difflog_wave1_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((difflog_wave1_ts)) ## Not stationary


######## remove seasonality

seasdifflog_wave1_ts = diff(difflog_wave1_ts,lag=7)

autoplot(seasdifflog_wave1_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(seasdifflog_wave1_ts))


adf.test(seasdifflog_wave1_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((seasdifflog_wave1_ts)) ## stationary


autoplot(decompose(seasdifflog_wave1_ts)) +
  ggtitle("Wave 1 after trsanfomations ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))


par(mfrow=c(2,1))
acf(seasdifflog_wave1_ts)
pacf(seasdifflog_wave1_ts)

### Looks like we have a differer model for wave 1 

###  probable Arima models

#RIMA(3,1,2)(0,1,0)

model_wave1_arima1= arima(
  log_wave1_ts, 
  order=c(3,1,2), 
  seasonal=c(order=c(0,1,0)))


summary(model_wave1_arima1) #AIC = 187.82 and RMSE 0.3020884
checkresiduals(model_wave1_arima1)
ggtsdisplay(model_arima1$residuls)


model_wave1_arima2= arima(
  log_wave1_ts, 
  order=c(3,1,2), 
  seasonal=c(order=c(0,1,1)))


summary(model_wave1_arima2)  #AIC = 40.25 and RMSE 0.2452903
checkresiduals(model_wave1_arima2)
ggtsdisplay(model_wave1_arima2$residuals)



model_wave1_arima3= arima(
  log_wave1_ts, 
  order=c(3,1,2), 
  seasonal=c(order=c(0,1,2)))


summary(model_wave1_arima3)  #AIC = 28.67 and 0.2396152
checkresiduals(model_wave1_arima3)
ggtsdisplay(model_wave1_arima3$residuals)



#### apply auto arima

model_wave1_autoarima=auto.arima(log_wave1_ts)
summary(model_wave1_autoarima) # AIC = 36.59  and RMSE 0.2442726
checkresiduals(model_wave1_autoarima)
ggtsdisplay(model_wave1_autoarima$residuals)
# AIC= and RMSE 


################# WAVE 2 ##########################


wave2_ts

BoxCox.lambda(wave2_ts) # 0,377, some transformation is required

### formal tests  for checking stationary ##

adf.test(wave2_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((wave1_ts)) ## Not stationary

#### apply the same transformations as full model , i.e. taking log and differentiate


autoplot(decompose(wave2_ts)) +
  ggtitle("Wave1 daily time series ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

###### differntiate seasonally #######



# remove variability 
log_wave2_ts = log(wave2_ts)

# Remove trend 
difflog_wave2_ts = diff(log_wave2_ts,1)


autoplot(difflog_wave2_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(difflog_wave2_ts))

adf.test(difflog_wave2_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((difflog_wave2_ts)) ## Not stationary


######## remove seasonality

seasdifflog_wave2_ts = diff(diff(difflog_wave2_ts,lag=7))

#seasdifflog_wave2_ts = diff(difflog_wave2_ts,lag=7)

autoplot(seasdifflog_wave2_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(seasdifflog_wave2_ts))


adf.test(seasdifflog_wave2_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((seasdifflog_wave2_ts)) ## stationary


autoplot(decompose(seasdifflog_wave2_ts)) +
  ggtitle("Wave 2 after trsanfomations ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))


par(mfrow=c(2,1))
acf(seasdifflog_wave2_ts)
pacf(seasdifflog_wave2_ts)

### Looks like we have a differer model for wave 2

###  probable Arima models

#RIMA(3,1,2)(0,1,0)

model_wave2_arima1= arima(
  log_wave2_ts, 
  order=c(5,1,2), 
  seasonal=c(order=c(0,1,0)))


summary(model_wave2_arima1) #AIC = -318.03  and RMSE 0.1446592
checkresiduals(model_wave2_arima1)
ggtsdisplay(model_wave2_arima1$residuls)


model_wave2_arima2= arima(
  log_wave2_ts, 
  order=c(5,1,2), 
  seasonal=c(order=c(0,1,1)))


summary(model_wave2_arima2)  #AIC =  -453.3 and RMSE 0.1166549
checkresiduals(model_wave2_arima2)
ggtsdisplay(model_wave2_arima2$residuals)



model_wave1_arima3= arima(
  log_wave2_ts, 
  order=c(6,1,2), 
  seasonal=c(order=c(0,1,2)))


summary(model_wave1_arima3)  #AIC = -450.74 and RMSE 0.1162029
checkresiduals(model_wave1_arima3)
ggtsdisplay(model_wave1_arima3$residuals)



#### apply auto arima

model_wave2_autoarima=auto.arima(log_wave2_ts)
summary(model_wave2_autoarima) # AIC = -439.23  and RMSE 0.1234631
checkresiduals(model_wave2_autoarima)
ggtsdisplay(model_wave2_autoarima$residuals)
# AIC= and RMSE 


############### WAVE 3 ############


### formal tests  for checking stationary ##

adf.test(wave3_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((wave3_ts)) ## Not stationary

#### apply the same transformations as full model , i.e. taking log and differentiate


autoplot(decompose(wave3_ts)) +
  ggtitle("Wave3 daily time series ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

###### differntiate seasonally #######



# remove variability 
log_wave3_ts = log(wave3_ts)

# Remove trend 
difflog_wave3_ts = diff(log_wave3_ts,1)


autoplot(difflog_wave3_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(difflog_wave3_ts))

adf.test(difflog_wave3_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((difflog_wave3_ts)) ## Not stationary


######## remove seasonality

seasdifflog_wave3_ts = diff(diff(difflog_wave3_ts,lag=7))

#seasdifflog_wave2_ts = diff(difflog_wave2_ts,lag=7)

autoplot(seasdifflog_wave3_ts) +
  ggtitle("Differentiation after taking logs ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))

autoplot(decompose(seasdifflog_wave3_ts))


adf.test(seasdifflog_wave3_ts, alternative = "stationary",k = 0) ## Stationary
kpss.test((seasdifflog_wave3_ts)) ## stationary


autoplot(decompose(seasdifflog_wave3_ts)) +
  ggtitle("Wave 3 after trsanfomations ") +
  ylab('Number of tests')+
  theme(plot.title = element_text(hjust = 0.5))


par(mfrow=c(2,1))
acf(seasdifflog_wave3_ts)
pacf(seasdifflog_wave3_ts)

### Looks like we have a differer model for wave 2

###  probable Arima models

#RIMA(3,1,2)(0,1,0)

model_wave3_arima1= arima(
  log_wave3_ts, 
  order=c(4,1,2), 
  seasonal=c(order=c(0,1,0)))


summary(model_wave3_arima1) #AIC =   and RMSE 
checkresiduals(model_wave3_arima1)
ggtsdisplay(model_wave3_arima1$residuls)


model_wave3_arima2= arima(
  log_wave3_ts, 
  order=c(4,1,2), 
  seasonal=c(order=c(0,1,1)))


summary(model_wave3_arima2)  #AIC =  and RMSE 
checkresiduals(model_wave3_arima2)
ggtsdisplay(model_wave3_arima2$residuals)



model_wave3_arima3= arima(
  log_wave3_ts, 
  order=c(4,1,2), 
  seasonal=c(order=c(1,1,2)))


summary(model_wave3_arima3)  #AIC =  and RMSE 
checkresiduals(model_wave1_arima3)
ggtsdisplay(model_wave1_arima3$residuals)



#### apply auto arima

model_wave3_autoarima=auto.arima(log_wave3_ts)
summary(model_wave3_autoarima) # AIC = -99.42  and RMSE 0.1659124
checkresiduals(model_wave2_autoarima)
ggtsdisplay(model_wave2_autoarima$residuals)
# AIC= and RMSE 

'******************* Forecasting *************************'

############### FULL DATA ###############
## split the time series into traing and test set of 28 days (4 weeks )

daily_ts <- ts(daily_data$Impnew_tests, frequency = 7)

train_full = head(daily_ts, -28)
test_full = tail (daily_ts, 28)

### Holt winter with additive seasonality
model_full_add <- hw(train_full, seasonal = "additive",h=28)

forecast_full_add <- forecast(model_full_add, h = length(test_full))

accuracy_full_add <- accuracy(forecast_full_add, test)
print(accuracy_full_add) # RMSE 46733.48 # MAE 17950.95


summary(model_full_add)
par(mfrow=c(1,1))
plot(model_full_add, main = "Holt winter Additve Model for full Time Series")
lines(fitted(model_full_add), col = "red")

### Holt winter with multilicative seasonality

model_full_mul <- hw(train_full, seasonal = "multiplicative",h=28)

forecast_full_mul <- forecast(model_full_mul, h = length(test_full))

accuracy_full_mul <- accuracy(forecast_full_mul, test_full)
print(accuracy_full_mul) # RMSE 48755 # MAE 18006


summary(model_full_mul)



####### Auto ARIMA 

log_daily_ts = log(daily_ts)
train_log_full = head(log_daily_ts, -28)


model_full_log_autoarima=auto.arima(train_log_full) # train on log data 


forecast_log  = forecast(model_full_log_autoarima, h = 28); # forecast for 28 days
point_forecasts_log <- forecast_log$mean # get the point value

###### convert log values to normal scale
point_forecasts=exp(point_forecasts_log) # convert into normal scale

square_difference=(point_forecasts-test_full)^2 


# calculate RMSE
sqrt(mean(square_difference)) # 66230.83
# calculate MAE
mean(abs(point_forecasts-test_full)) # 51900.75


######### chosen ARIMA model ####

model_full_log_arima=arima(
  train_log_full, 
  order=c(5,1,2), 
  seasonal=c(order=c(0,1,1))) 


forecast_log_arima  = forecast(model_full_log_arima, h = 28); # forecast for 28 days
point_forecasts_log_aqarima <- forecast_log_arima$mean # get the point value

point_forecasts_arima <- exp(point_forecasts_log_aqarima)

square_difference_arima=(point_forecasts_arima-test_full)^2 


# calculate RMSE
sqrt(mean(square_difference_arima)) # 68552.91
# calculate MAE
mean(abs(point_forecasts_arima-test_full)) # 52113.




'############################ wave 1 #######################################'

wave1_ts

train_wave1 = head(wave1_ts, -28)
test_wave1 = tail (wave1_ts, 28)

### Holt winter with additive seasonality
model_wave1_add <- hw(train_wave1, seasonal = "additive",h=28)

forecast_wave1_add <- forecast(model_wave1_add, h = length(test_wave1))

accuracy_wave1_add <- accuracy(forecast_wave1_add, test_wave1)
print(accuracy_wave1_add) # RMSE 16539.92 # MAE 14369.875


summary(model_full_add)

plot(model_wave1_add, main = "Holt winter Additve")
lines(fitted(model_full_add), col = "red")

### Holt winter with multilicative seasonality

model_wave1_mul <- hw(train_wave1, seasonal = "multiplicative",h=28)

forecast_wave1_mul <- forecast(model_wave1_mul, h = length(test_wave1))

accuracy_full_mul <- accuracy(forecast_wave1_mul, test_wave1)
print(accuracy_full_mul) # RMSE 48755 # MAE 18006


summary(model_full_mul)


####### Auto ARIMA 

log_wave1_ts = log(wave1_ts)
train_log_wave1 = head(log_wave1_ts, -28)


model_wave1_log_autoarima=auto.arima(train_log_wave1) # train on log data 


forecast_log_wave1  = forecast(model_wave1_log_autoarima, h = 28); # forecast for 28 days
point_forecasts_log_wave1 <- forecast_log_wave1$mean # get the point value

point_forecasts_wave1=exp(point_forecasts_log_wave1) # convert into normal scale

square_difference_wave1=(point_forecasts_wave1-test_wave1)^2 


# calculate RMSE
sqrt(mean(square_difference_wave1)) # 17592
# calculate MAE
mean(abs(point_forecasts_wave1-test_wave1)) # 51900.75


######### chosen ARIMA model ####

model_wave1_log_arima=arima(
  train_log_wave1, 
  order=c(3,1,2), 
  seasonal=c(order=c(0,1,2))) 


forecast_log_arima_wave1  = forecast(model_wave1_log_arima, h = 28); # forecast for 28 days
point_forecasts_log_arima_wave1 <- forecast_log_arima_wave1$mean # get the point value

point_forecasts_arima_wave1 <- exp(point_forecasts_log_arima_wave1)

square_difference_arima_wave1=(point_forecasts_arima_wave1-test_wave1)^2 


# calculate RMSE
sqrt(mean(square_difference_arima_wave1)) # 68552.91
# calculate MAE
mean(abs(point_forecasts_arima_wave1-test_wave1)) # 52113.


plot(forecast_log_arima_wave1, main = "ARIMA (3,1,2) (0,1,2) forecast on log-data")
lines(fitted(forecast_log_arima_wave1), col = "red")



'#################### wave 2 ##########################'

wave2_ts

train_wave2 = head(wave2_ts, -28)
test_wave2 = tail (wave2_ts, 28)

### Holt winter with additive seasonality
model_wave2_add <- hw(train_wave2, seasonal = "additive",h=28)

forecast_wave2_add <- forecast(model_wave2_add, h = length(test_wave2))

accuracy_wave2_add <- accuracy(forecast_wave2_add, test_wave2)
print(accuracy_wave2_add) # RMSE 16539.92 # MAE 14369.875


summary(model_full_add)

plot(model_wave2_add, main = "Holt winter Additve")
lines(fitted(model_full_add), col = "red")

plot(model_wave2_add, main = "HW(additve) forecast for Wave 2")
lines(fitted(model_wave2_add), col = "red")



### Holt winter with multilicative seasonality

model_wave2_mul <- hw(train_wave2, seasonal = "multiplicative",h=28)

forecast_wave2_mul <- forecast(model_wave2_mul, h = length(test_wave2))

accuracy_full_mul <- accuracy(forecast_wave2_mul, test_wave2)
print(accuracy_full_mul) # RMSE 48755 # MAE 18006


summary(model_full_mul)


####### Auto ARIMA 

log_wave2_ts = log(wave2_ts)
train_log_wave2 = head(log_wave2_ts, -28)


model_wave2_log_autoarima=auto.arima(train_log_wave2) # train on log data 


forecast_log_wave2  = forecast(model_wave2_log_autoarima, h = 28); # forecast for 28 days
point_forecasts_log_wave2 <- forecast_log_wave2$mean # get the point value

point_forecasts_wave2=exp(point_forecasts_log_wave2) # convert into normal scale

square_difference_wave2=(point_forecasts_wave2-test_wave2)^2 


# calculate RMSE
sqrt(mean(square_difference_wave2)) # 17592
# calculate MAE
mean(abs(point_forecasts_wave2-test_wave2)) # 51900.75


######### chosen ARIMA model ####

model_wave2_log_arima=arima(
  train_log_wave2, 
  order=c(5,1,2), 
  seasonal=c(order=c(0,1,1))) 


forecast_log_arima_wave2  = forecast(model_wave2_log_arima, h = 28); # forecast for 28 days
point_forecasts_log_arima_wave2 <- forecast_log_arima_wave2$mean # get the point value

point_forecasts_arima_wave2 <- exp(point_forecasts_log_arima_wave2)

square_difference_arima_wave2=(point_forecasts_arima_wave2-test_wave2)^2 


# calculate RMSE
sqrt(mean(square_difference_arima_wave2)) # 68552.91
# calculate MAE
mean(abs(point_forecasts_arima_wave2-test_wave2)) # 52113.


'########################## wave 3   ###########################'

wave3_ts

train_wave3 = head(wave3_ts, -28)
test_wave3 = tail (wave3_ts, 28)

### Holt winter with additive seasonality
model_wave3_add <- hw(train_wave3, seasonal = "additive",h=28)

forecast_wave3_add <- forecast(model_wave3_add, h = length(test_wave3))

accuracy_wave3_add <- accuracy(forecast_wave3_add, test_wave3)
print(accuracy_wave3_add) # RMSE 16539.92 # MAE 14369.875


summary(model_full_add)

plot(model_wave3_add, main = "Holt winter Additve")
lines(fitted(model_full_add), col = "red")

### Holt winter with multilicative seasonality

model_wave3_mul <- hw(train_wave3, seasonal = "multiplicative",h=28)

forecast_wave3_mul <- forecast(model_wave3_mul, h = length(test_wave3))

accuracy_full_mul <- accuracy(forecast_wave3_mul, test_wave3)
print(accuracy_full_mul) # RMSE 48755 # MAE 18006


summary(model_full_mul)


plot(model_wave3_mul, main = "HW(multiplicative) forecast for Wave 3")
lines(fitted(model_wave3_mul), col = "red")


####### Auto ARIMA 

log_wave3_ts = log(wave3_ts)
train_log_wave3 = head(log_wave3_ts, -28)


model_wave3_log_autoarima=auto.arima(train_log_wave3) # train on log data 


forecast_log_wave3  = forecast(model_wave3_log_autoarima, h = 28); # forecast for 28 days
point_forecasts_log_wave3 <- forecast_log_wave3$mean # get the point value

point_forecasts_wave3=exp(point_forecasts_log_wave3) # convert into normal scale

square_difference_wave3=(point_forecasts_wave3-test_wave3)^2 


# calculate RMSE
sqrt(mean(square_difference_wave3)) # 17592
# calculate MAE
mean(abs(point_forecasts_wave3-test_wave3)) # 51900.75


######### chosen ARIMA model ####

model_wave3_log_arima=arima(
  train_log_wave3, 
  order=c(4,1,2), 
  seasonal=c(order=c(0,1,1))) 


forecast_log_arima_wave3  = forecast(model_wave3_log_arima, h = 28); # forecast for 28 days
point_forecasts_log_arima_wave3 <- forecast_log_arima_wave3$mean # get the point value

point_forecasts_arima_wave3 <- exp(point_forecasts_log_arima_wave3)

square_difference_arima_wave3=(point_forecasts_arima_wave3-test_wave3)^2 


# calculate RMSE
sqrt(mean(square_difference_arima_wave3)) #104434.3
# calculate MAE
mean(abs(point_forecasts_arima_wave3-test_wave3)) # 88561.97.

