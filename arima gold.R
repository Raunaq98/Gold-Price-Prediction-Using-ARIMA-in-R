#Imporunt necessary libaries
library(forecast)
library(tseries)

#Reading data from CSV
gold=read.csv("GOLD.csv")

#Converting to time series formate
gold_price <- ts(gold$Price,start = c(1964,1), frequency = 1)
class(gold_price)
View(gold_price)

#Examining the Data
plot(gold_price)
adf.test(gold_price, alternative = "stationary") 
acf(gold_price)
pacf(gold_price)

#Differencing once
diff1<- diff(gold_price)
plot(diff1)
adf.test(diff1)
acf(diff1,main="Differencing once")
pacf(diff1,main="Differencing once")

#Differencing twice
diff1<- diff(gold_price,differences = 2)
plot(diff1)
adf.test(diff1)
acf(diff1,main="Differencing twice")
pacf(diff1,main="Differencing twice")

#Natural log transformation and differenced once
ln=log(gold_price)
diffln=diff(ln)
plot(diffln)
adf.test(diffln)
acf(diffln,main="ln Transformation & Differencing once")
pacf(diff1,main="ln Transformation & Differencing once")

#Natural log transformation and differenced twice
lnGold=log(gold_price)
difflnGold=diff(lnGold,differences = 2)
plot(difflnGold)
adf.test(difflnGold)
acf(difflnGold,main="ln Transformation & Differencing once")
pacf(difflnGold,main="ln Transformation & Differencing once")

cat('Data is stationary at log transformation and differencing once')

#Fiting ARIMA model
arimaFit=auto.arima(lnGold)

#Comparing with mannual fitted model
fit <- arima(log(gold_price), c(0, 1, 1))
summary(fit)


#Forecast using model
pred <- predict(fit, n.ahead = 5)

#Converting natural log to decimal values
View(2.718^pred$pred)
arima <- forecast(fit, h=5)
accuracy(arima)

#Plotting the forecast
plot(arima) + abline(h=5)
