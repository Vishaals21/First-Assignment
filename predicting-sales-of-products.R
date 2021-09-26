library(dplyr)
library(fpp2)
library(ggplot2)
library(xlsx)
setwd("/Users/vishalsharma/Downloads/Data Analytics/Excel Files for R")
exceldata = read.xlsx("Product_wise_R_v2.xlsx", sheetIndex = 1)
exceldata
Y <- exceldata %>% filter(Material.Code=="FEF1-DIN70(ISS)" & Zone =="West")
Y
X <-ts(Y[,4], start = c(2019,4), frequency=12)
ggseasonplot(X)

#Trying ETS Model

fit_ets <- ets(X)
print(summary(fit_ets))
checkresiduals(fit_ets)

#Trying ARIMA Model
fit_arima <- auto.arima(X, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
