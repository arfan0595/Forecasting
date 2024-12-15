
#Step 1: Importing the time series to R
#upload the dataset
time_series <- read.csv("C:/Users/arfan/OneDrive/Desktop/Sem 6/STA572/Assignment/Group Assignment 1/Dataset/5 IV 1 DV/Life Expectancy.csv", header = TRUE)
head(time_series)
na_count <- colSums(is.na(time_series))
# Check the structure of the data
str(time_series)

#define variable
LE = time_series[,2] #Life Expectancy (LE) <-- masalahhhhhhh

#define as time series objects
LE.ts = ts(LE, frequency = 1, start = "2000") #freq 1 sebab dia yearly

#Step 2: Summary statistics
summary(LE)
sd(LE)
skewness(LE)
head(LE)
tail(LE) 

#Step 3: Shape of the distribution
# Plot histogram
hist(LE, main = "Histogram of Life Expectancy", xlab = "Life Expectancy", col = "skyblue", border = "black")
# Plot boxplot
boxplot(LE, main = "Boxplot of Life Expectancy", col = "lightgreen", border = "black")
# Calculate potential outliers
outliers <- boxplot.stats(LE)$out
outliers

#Step 4: Time series components
library(forecast)
plot.ts(LE)
acf(LE.ts)
ggseasonplot(LE.ts)

#Step 5: Check stationary
library(tseries)
#H0: non-stationary
adf.test(LE.ts) #> 0.05; non-stationary

#Step 6: differencing
#Trend exist hence not stationary 
diff_LE= diff(LE.ts,differences = 3)
plot.ts(diff_LE)
adf.test(diff_LE)

#comment: P-value(0.01) is now lower than 0.05, the model is stationary

#Step 7: obtain p and q values
acf(c(diff_LE)) #q=1
pacf(c(diff_LE)) #p=1

#Step 8: Fit autoregressive time series models
#Model1: AR(p)
model1=arima(diff_LE, order=c(1,0,0)) #AIC = -107.94
checkresiduals(model1) #0.3; have serial correlation
#Ljung-Box Test
#H0: No serial correlation

#Model2: ARMA(p,q)
model2=arima(diff_LE, order=c(1,0,1)) #AIC = -113.33
checkresiduals(model2) #0.5; no serial correlation
#Ljung-Box Test
#H0: No serial correlation

#Model3: ARIMA(p,d,q)
model3=arima(diff_LE, order=c(1,3,1)) #AIC = -54.72
checkresiduals(model3) #0.06; no serial correlation
#Ljung-Box Test
#H0: No serial correlation

#Model4: auto.arima
model4 = auto.arima(LE.ts, trace = TRUE) #ARIMA(0,0,1) AIC = -115.307
fitted_arima = fitted(model4)
checkresiduals(model4) #0.9; no serial correlation
#Ljung-Box Test
#H0: No serial correlation

#Step 9: Error measures(accuracy)
# Calculating error measures
accuracy1 = accuracy(model1)
accuracy2 = accuracy(model2)
accuracy3 = accuracy(model3)
accuracy4 = accuracy(model4)

# Calculating AIC
aic_model1 = AIC(model1)
aic_model2 = AIC(model2)
aic_model3 = AIC(model3)
aic_model4 = AIC(model4) #-116.0129

#Step 10: Residual checks
bestmodel_residuals = model4$residuals
stand_resid = bestmodel_residuals / sd(bestmodel_residuals)

#1.Identical and independently distributed
# Scatterplot
plot(c(bestmodel_residuals),main="Residual of arima(0,0,1)", ylab="Residuals")
hist(bestmodel_residuals,main="Histogram of Standardized Residuals", xlab="Standardized Residuals", col="lightblue", border="black")
Box.test(bestmodel_residuals)
Box.test(rnorm(length(bestmodel_residuals)))

#2.Normality - QQnorm & Anderson Darling
qqnorm(bestmodel_residuals,main="Q-Q plot of residuals")
qqline(bestmodel_residuals, col=2,lwd=2,lty=2)
ad.test(bestmodel_residuals)

#3.Homoscedasticity - Standardized residuals versus fitted values & Breusch Pagan
fitted_values = model4$fitted
plot(fitted_values, stand_resid, xlab="Fitted values",ylab="Standartized Residuals")
abline(h=0,col=2)

par(mfrow = c(1,2))
plot.ts(fitted_values, main="Auto Arima fitted model")
plot(c(fitted_values),c(stand_resid))

par(mfrow = c(1,1))
plot(fitted_values,stand_resid,xlab="Fitted Values",ylab="Standardized Residuals",main = "Standardized Residuals vs Fitted Values")

lm_model = lm(bestmodel_residuals ~ 1 + model4$fitted )
bptest(lm_model)

#4.Absence of serial correlation - ACF & Durbin Watson
acf(c(bestmodel_residuals),main="ACF for residuals")
dwtest(lm(bestmodel_residuals ~model4$fitted))

#5.Absence of lag dependence - Scaetterplot & Ljung-Box
plot(c(bestmodel_residuals),ylab="Auto.arima model residual", main="Scatter plot of residual")
Box.test(bestmodel_residuals, lag=1, type = "Ljung-Box")

#6.Absence of influential observations such as outliers
boxplot(bestmodel_residuals,xlab="x",ylab="y",main="Boxplot of auto.arima model")
#Identify and print the outliers
boxplot.stats(bestmodel_residuals)$out

#7.Absence of volatility
#Square the residuals
acf(c(bestmodel_residuals^2),main="ACF of residuals^2")

#the best model auto arima (0,0,1)

#Step 11: Forecasting
library(forecast)
# Forecast 12 steps ahead with the best model (replace 'fit_ets' with the actual name of your ETS model)
forecast_values = forecast(model4, h = 12)
autoplot(LE.ts, series = "Time Series Model", ylab = "Life Expectancy", xlab = "Year", main = "ARIMA Life Expectancy Forecast", alpha = 0.5) +
  autolayer(fitted_arima, series = "Fitted Model") +
  autolayer(forecast_values$mean, series = "Forecasted Model") +
  scale_color_manual(name = "Fitted Models", values = c("blue", "green", "red"))
# Show the plot
# Report forecasted values
print(forecast_values)

#forecast_values <- forecast(model4, h = 12)
#summary(forecast_values)
#plot(forecast_values)
