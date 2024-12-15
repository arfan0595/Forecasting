
#upload the dataset
dataset = read.csv("Life Expectancy.csv", header = TRUE)
head(dataset)
na_count <- colSums(is.na(dataset))

#define variable
LE = dataset[,2] #Life Expectancy (LE) <-- masalahhhhhhh
CO2 = dataset[,3] #CO2 emission (CO2) <-- Kalau CO2 positif(+), LE negatif(-)
UR = dataset[,4] #Unemployment Rate (UR) <-- Kalau UR positif(+), LE negatif(-)
ES = dataset[,5] #Education Expenditure (ES) <-- Kalau ES positif(+), Le positif(+)
GNI = dataset[,6] #Gross National Income (GNI) <-- Kalau GNI positif(+), GDP positif(+)
HDR = dataset[,7] #Heart Disease Rate (HDR) <-- Kalau HDR positif(+), GDP negatif(-)

#define as time series objects
LE.ts = ts(LE, frequency = 1, start = "2000") #freq 1 sebab dia yearly
CO2.ts = ts(CO2, frequency = 1, start = "2000")
UR.ts = ts(UR, frequency = 1, start = "2000") 
ES.ts = ts(ES, frequency = 1, start = "2000")
GNI.ts = ts(GNI, frequency = 1, start = "2000")
HDR.ts = ts(HDR, frequency = 1, start = "2000")

#time series plot
par(mfrow = c(3,2))
plot.ts(LE.ts, xlab = "Time (Year)", ylab = "Life Expectancy", type = "o", pch = 1, col = "blue") #DV
plot.ts(CO2.ts, xlab = "Time (Year)", ylab = "CO2 Emmission") #IV1
plot.ts(UR.ts, xlab = "Time (Year)", ylab = "Unemployment Rate") #IV2
plot.ts(ES.ts, xlab = "Time (Year)", ylab = "Education Expenditure") #IV3
plot.ts(GNI.ts, xlab = "Time (Year)", ylab = "Growth National Income") #IV4
plot.ts(HDR.ts, xlab = "Time (Year)", ylab = "Heart Disease Rate") #IV5

#descriptive statistics
summary(LE)
summary(CO2)
summary(UR)
summary(ES)
summary(GNI)
summary(HDR)

#standard deviation (sd)
sd(LE)
sd(CO2)
sd(UR)
sd(ES)
sd(GNI)
sd(HDR)

library(PerformanceAnalytics)

#skewness
skewness(LE)
skewness(CO2)
skewness(UR)
skewness(ES)
skewness(GNI)
skewness(HDR)

#correlation matrix(multicollinearity between IVs)
#corr nak check multicollinearity
df_dataset = data.frame(LE,CO2,UR,ES,GNI,HDR)
chart.Correlation(df_dataset, method = "pearson", histogram = FALSE) #nak tengok linear je
#correlation between 2 IVs more that 0.8 indicates multicollinearity problem 
#check Variance Inflation Factor (VIF) in the modelling, VIF > 10 confirming multicollinearity status
#redundancy/overlapping information sebab ada 2 info yg sama akan bagi ke DV
#hubungan antara faktor dengan masalah nak > dari 0.8, tapi hubungan antara faktor2 taknak lebih 0.8
#takkesah positive atau negative


#model fitting MLR (multiple linear )
DV = LE #the problem
IV1 = CO2 
IV2 = UR
IV3 = ES
IV4 = GNI
IV5 = HDR

#MODEL 1
model1 = lm(DV~IV1+IV2+IV3+IV4+IV5)
summary(model1)
library(car)
?vif
vif(model1)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model1 = model1$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model1) #result: normal; fail to reject(accept) the H0 sebab p-value is 0.3 > 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model1) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model1)
library(lmtest)
dwtest(model1) #ada serial corr sebab value tak dalam range (1.5~2) closer to 2
#p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model1) #reject H0 because the p-value < 0.05
summary(model1) #kalau well-specified semua ada star including intercept
#exclude IV4 to obtain model2
vif(model1)

#MODEL 2
model2 = lm(DV~IV1+IV2+IV3+IV5)
summary(model2)
library(car)
?vif
vif(model2)#remove IV4 from model1 to obtain model2 because the value of vif is above 10 
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model2 = model2$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model2) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model2) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model2)
library(lmtest)
dwtest(model2) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model2) #reject H0 because the p-value < 0.05
summary(model2) #kalau well-specified semua ada star including intercept
#exclude IV3 to obtain model3
vif(model2)

#MODEL 3
model3 = lm(DV~+IV1+IV2+IV5)
summary(model3)
library(car)
?vif
vif(model3)#remove IV3 from model2 to obtain model3 because incorrect estimated symbol and not significant
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model3 = model3$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model3) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model3) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model3)
library(lmtest)
dwtest(model3) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model3) #reject H0 because the p-value < 0.05
summary(model3) #kalau well-specified semua ada star including intercept
#exclude IV1 to obtain model4
vif(model3)


#MODEL 4
model4 = lm(DV~IV2+IV5)
summary(model4)
library(car)
?vif
vif(model4)
#remove IV1 from model3 to obtain model 4 because IV1 has incorrect estimated symbol such as IV5 but IV1 was choosen because less significant
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model4 = model4$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model4) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model4) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model4)
library(lmtest)
dwtest(model4) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model4) #reject H0 because the p-value < 0.05
summary(model4) #kalau well-specified semua ada star including intercept
vif(model4)


#nak hilangkan masalah serial correlation kena tambah lag variable tapi tak tahu yg mana kena tambah
#Add lag variables to eliminate serial correlation problem in residual
#Define lag variables
X = DV
lag1DV = c(NA, X[1:length(X)-1])
X = lag1DV
lag2DV = c(NA, X[1:length(X)-1])
X = lag2DV
lag3DV = c(NA, X[1:length(X)-1])
X = lag3DV
lag4DV = c(NA, X[1:length(X)-1])
X = lag4DV
lag5DV = c(NA, X[1:length(X)-1])


X = IV1
lag1IV1 = c(NA, X[1:length(X)-1]) 
X = lag1IV1
lag2IV1 = c(NA, X[1:length(X)-1]) 
X = lag2IV1
lag3IV1 = c(NA, X[1:length(X)-1]) 
X = lag3IV1
lag4IV1 = c(NA, X[1:length(X)-1]) 


X = IV2
lag1IV2 = c(NA, X[1:length(X)-1]) 
X = lag1IV2
lag2IV2 = c(NA, X[1:length(X)-1]) 
X = lag2IV2
lag3IV2 = c(NA, X[1:length(X)-1])
X = lag3IV2
lag4IV2 = c(NA, X[1:length(X)-1])
X = lag4IV2
lag5IV2 = c(NA, X[1:length(X)-1])
X = lag5IV2
lag6IV2 = c(NA, X[1:length(X)-1])

X = IV3
lag1IV3 = c(NA, X[1:length(X)-1]) 
X = lag1IV3
lag2IV3 = c(NA, X[1:length(X)-1]) 
X = lag2IV3
lag3IV3 = c(NA, X[1:length(X)-1]) 
X = lag3IV3
lag4IV3 = c(NA, X[1:length(X)-1]) 

X = IV4
lag1IV4 = c(NA, X[1:length(X)-1]) 
X = lag1IV4
lag2IV4 = c(NA, X[1:length(X)-1]) 

X = IV5
lag1IV5 = c(NA, X[1:length(X)-1]) 
X = lag1IV5
lag2IV5 = c(NA, X[1:length(X)-1]) 
X = lag2IV5
lag3IV5 = c(NA, X[1:length(X)-1]) 
X = lag3IV5
lag4IV5 = c(NA, X[1:length(X)-1]) 


#MODEL 5
model5 = lm(DV~IV2+IV5+lag1DV) #add lag1DV
#P-value is not satisfied, therefore reject H0 and IV2 is not significant - model not suitable
summary(model5)
library(car)
?vif
vif(model5)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model5 = model5$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model5) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model5) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model5)
library(lmtest)
dwtest(model5) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model5) #reject H0 because the p-value < 0.05
summary(model5) #kalau well-specified semua ada star including intercept
#exclude IV1 to obtain model3
vif(model5)



#MODEL 6
model6 = lm(DV~IV2+IV5+lag1DV+lag1IV2) #Add lag1IV2
#P-value is not satisfied, therefore reject H0 and IV2 is not significant - model not suitable
summary(model6)
library(car)
?vif
vif(model6)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model6 = model6$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model6) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model6) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model6)
library(lmtest)
dwtest(model6) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model6) #reject H0 because the p-value < 0.05
summary(model6) #kalau well-specified semua ada star including intercept
#exclude IV1 to obtain model3
vif(model6)


#MODEL 7
model7 = lm(DV~IV2+IV5+lag1DV+lag1IV2+lag1IV5) #Add lag1IV5
#P-value is not satisfied, therefore reject H0 and IV2 and IV5 is not significant - model not suitable
summary(model7)
library(car)
?vif
vif(model7)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model7 = model7$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model7) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model7) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model7)
library(lmtest)
dwtest(model7) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model7) #reject H0 because the p-value < 0.05
summary(model7) #kalau well-specified semua ada star including intercept
vif(model7)



#MODEL 8
model8 = lm(DV~IV2+IV5+lag2DV+lag1IV2+lag1IV5) #Add lag to DV and change it to lag2DV
#P-value satisfied, but the estimated symbol for IV5 is incorrect - model not suitable
summary(model8)
library(car)
?vif
vif(model8)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model8 = model8$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model8) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model8) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model8)
library(lmtest)
dwtest(model8) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model8) #reject H0 because the p-value < 0.05
summary(model8) #kalau well-specified semua ada star including intercept
vif(model8)


#MODEL 9
model9 = lm(DV~IV2+lag1DV) #Remove IV5 and lag1IV5. Add lag1DV only
#P-value not satisfied - model not suitable
summary(model9)
library(car)
?vif
vif(model9)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model9 = model9$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model9) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model9) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model9)
library(lmtest)
dwtest(model9) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model9) #reject H0 because the p-value < 0.05
summary(model9) #kalau well-specified semua ada star including intercept
vif(model9)



#MODEL 10
model10 = lm(DV~IV2+lag1DV+lag1IV2) #Add lag1IV2
#P-value not satisfied - model not suitable
summary(model10)
library(car)
?vif
vif(model10)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model10 = model10$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model10) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model10) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model10)
library(lmtest)
dwtest(model10) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model10) #reject H0 because the p-value < 0.05
summary(model10) #kalau well-specified semua ada star including intercept
vif(model10)


#MODEL 11
model11 = lm(DV~IV2+lag1DV+lag2IV2)
#Adding lag to IV2 until lag helps to increase the p-value of Anderson Normality
#P-value not satisfied - model not well specified
summary(model11)
library(car)
?vif
vif(model11)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model11 = model11$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model11) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model11) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model11)
library(lmtest)
dwtest(model11) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model11) #reject H0 because the p-value < 0.05
summary(model11) #kalau well-specified semua ada star including intercept
vif(model11)


#MODEL 12
model12 = lm(DV~IV2+lag1DV+lag3IV2)
#Adding lag to DV reduces the p-value of Durbin watson
#Adding lag to IV2 until lag 3 helps to increase the p-value of Anderson Normality
#P-value satisfied, strong significance, correct estimated value, vif value below 10 - model well specified
summary(model12)
library(car)
?vif
vif(model12)
#indications multicollinearity value is more than 10

#Assumptions for the residuals: normality, independence, homoskedasticity, serial correlation
res_model12 = model12$residuals #kita attach error term di bahagian belakang 

#t-test dan f-test nak p-value kecil
#tapi residuals nak p-value besar
#Quantitative diagnostics:

#Anderson Darling Test
#normality
library(nortest)
#H0: The residuals are normally distributed
ad.test(res_model12) #result: not normal; reject the H0 sebab p-value is 0.02 < 0.05

#Ljung-Box Test
#independence
library(stats)
#H0: The residuals is independently distributed
Box.test(res_model12) #result: dependent; reject the H0 sebab p-value  <0.05

#Durbin-Watson test
#serial correlation/auto correlation
library(car)
#H0: No serial correlation
durbinWatsonTest(res_model12)
library(lmtest)
dwtest(model12) #result: reject H0 so ada serial corr sebab p-value < 0.05

#Breush-Pagan test
#homoskedasticity
library(lmtest)
#H0: Homoscedasticity
bptest(model12) #reject H0 because the p-value < 0.05
summary(model12) #kalau well-specified semua ada star including intercept
vif(model12)



#Plotting
#<---------------------------------------------------------------------------------------------------------

#Normality
qqnorm(res_model12)
qqline(res_model12, col=2,lwd=2,lty=2)

#Independence
#Residual lag plot
plot(res_model12,ylab = "Residuals")
plot(res_model12[1:(length(res_model12)-1)],res_model12[2:length(res_model12)])

#Serial Correlation
acf(res_model12, main="Series DV")

#Homoskedaticity
STD_res_model12=res_model12/sd(res_model12)
plot(model12$fitted.values,STD_res_model12,xlab="Fitted values",ylab="Standartized Residuals")
#At horizontal line 0
abline(h=0,col=2)

#Accuracy
library(forecast)
accuracy(model1)
accuracy(model2)
accuracy(model3)
accuracy(model4)
accuracy(model5)
accuracy(model6)
accuracy(model7)
accuracy(model8)
accuracy(model9)
accuracy(model10)
accuracy(model11)
accuracy(model12)
plot.ts(DV)
lines(c(NA,model11$fitted.values),col="red")

#lag in
plot.ts(LE.ts, xlab="Time (Year)" , ylab=" Life Expectancy")
fitted.model12=c(NA,model12$fitted.values)
fitted.model12.ts=ts(fitted.model12, frequency = 1, start="2000")
lines(fitted.model12.ts, type="l", lty=2,�col="red")


install.packages('modelsummary')
install.packages('gt')
install.packages('huxtable')
install.packages('DT')
#summary table of results
library(modelsummary)
model_list = list(model1, model2, model3, model4, model5,model6,model7,model8,model9,model10,model11,model12)

# Create a summary table including t-tests and F-tests
library(huxtable)
table = modelsummary(model_list, stars = TRUE, test = TRUE, output = "huxtable")
table

library(dplyr) 
table %>% 
  set_caption("Fitted Coefficients for Life Expectancy")

library(broom)
extract_t_tests = function(model) {tidy(model)}

t_test_results = lapply(model_list, extract_t_tests)
t_test_results
