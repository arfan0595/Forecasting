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
library(PerformanceAnalytics)
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

#Step 5
# Load necessary librarieshttp://127.0.0.1:35737/graphics/plot_zoom_png?width=1920&height=1027
library(forecast)
library(ggplot2)

# Fit Naïve Model
naive_model <- naive(LE.ts)
summary(naive_model)

# Fit Simple Moving Average (MA) Model
ma_order <- 3  # Specify the order (window size) for the moving average
ma_model <- ma(LE.ts, order = ma_order)
summary(ma_model)

# Fit Exponential Smoothing Method Model
ets_model <- ets(LE.ts, model = "AAN")
fitted_ets <- fitted(ets_model)
summary(ets_model)

# Plot the fitted models
autoplot(LE.ts, series = "Time Series", alpha = 0.5) +
  autolayer(fitted(naive_model), series = "Naïve Model") +
  autolayer(ma_model, series = "Moving Average Model") +
  autolayer(fitted(ets_model), series = "Exponential Smoothing Model") +
  scale_color_manual(name = "Fitted Models", values = c("blue", "green", "red", "black"))

#Step 6
# Plot the fitted models
autoplot(LE.ts, series = "Time Series", alpha = 0.5) +
  autolayer(fitted(ets_model), series = "Exponential Smoothing Model") +
  scale_color_manual(name = "Fitted Models", values = c("blue" , "black"))

#Step 7
?residuals
#naive residuals
ets_residuals = ets_model$residuals
ets_residuals
accuracy(ets_model)

#Step 8
standard_residuals = ets_residuals / sd(ets_residuals)


#1.Identical and independently distributed
# Scatterplot
plot(standard_residuals, main="Residuals Scatterplot", ylab="Residuals")
symbols(standard_residuals, circles = rep(0.1, length(LE.ts)), inches = 0.05, add = TRUE)
# Histogram
hist(standard_residuals, main="Histogram of Standardized Residuals", xlab="Standardized Residuals", col="lightblue", border="black")

#2.Normality - QQnorm & Anderson Darling
qqnorm(standard_residuals)
qqline(standard_residuals, col=2,lwd=2,lty=2)

library(nortest)
ad.test(standard_residuals)

#3.Homoscedasticity - Standardized residuals versus fitted values & Breusch Pagan
data = data.frame(
  y = LE.ts,
  lagged_y = lag(LE.ts)
)
data = na.omit(data)
par(mfrow=c(1,1))
homo_res = residuals(ets_model)
homo_residuals = homo_res / sd(homo_res)
homo_fitted_values = fitted(ets_model)
homo_standardized_residuals = homo_residuals/sqrt(var(homo_residuals))
plot(homo_fitted_values,homo_standardized_residuals,xlab="Fitted Values",ylab="Standardized Residuals",main = "Standardized Residuals vs Fitted Values")

homo_fitted_values = ets_model$fitted

plot(homo_fitted_values, main="Residuals Scatterplot", ylab="Residuals")
plot(homo_fitted_values,c(standard_residuals))
plot(fitted(ets_model),standard_residuals, main="Residuals Scatterplot", ylab="Residuals")


#Breusch Pagan
bptest_homoscedasticity = bptest(ets_residuals^2 ~ fitted(ets_model) + I(fitted(ets_model)^2))
cat("Breusch-Pagan Test p-value:", bptest_homoscedasticity$p.value, "\n")

#4.Absence of serial correlation - ACF & Durbin Watson
acf_residuals = acf(standard_residuals, main="ACF Residuals")

#Durbin Watson
library(lmtest)
library(car)
x = standard_residuals
y = fitted_ets
lm_model = lm(y ~ x, data = LE.ts)
durbin_watson = durbinWatsonTest(lm_model)
print(durbin_watson)

#5.Absence of lag dependence - Scatterplot & Ljung-Box
plot(standard_residuals,ylab = "Residuals")
plot(standard_residuals[1:(length(standard_residuals)-1)],standard_residuals[2:length(standard_residuals)])

#Ljung-Box
ljung_box_test = Box.test(standard_residuals, lag = 20, type = "Ljung-Box")
ljung_box_statistic = ljung_box_test$statistic
ljung_box_p_value = ljung_box_test$p.value
print(paste("Ljung-Box Test X-Squared Statistic:", ljung_box_statistic))
print(paste("P-value:", ljung_box_p_value))

#6.Absence of influential observations such as outliers
boxplot(standard_residuals, main = "Boxplot for Residuals", col = "lightgreen", border = "black")
# Calculate potential outliers
outliers = boxplot.stats(standard_residuals)$out
outliers

#7.Absence of volatility
votatility = acf(standard_residuals^2, main="Absence Of Votatility")

#Step 9 


#Step 10

library(forecast)

# Forecast 12 steps ahead with the best model (replace 'fit_ets' with the actual name of your ETS model)
forecast_values <- forecast(ets_model, h = 12)

autoplot(LE.ts, series = "Time Series Model", ylab = "Life Expectancy", xlab = "Year", main = "Life Expectancy Forecast", alpha = 0.5) +
  autolayer(fitted_ets, series = "Fitted Model") +
  autolayer(forecast_values$mean, series = "Forecasted Model") +
  scale_color_manual(name = "Fitted Models", values = c("blue", "green", "red"))

# Show the plot
# Report forecasted values
print(forecast_values)
