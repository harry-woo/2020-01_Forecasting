library(tseries)
library(forecast)

## 데이터 읽기 ##

gdp_d  <- read.csv("gdpq.csv", header = TRUE)
gdp_sa <- ts(gdp_d[,1]/1000, start=1982, frequency=4)
gdp_sa_p <- ts(gdp_d[,2], start=1982, frequency=4)
#gdp_sa_p = window(gdp_sa_p1, 2000, 2020)

plot(gdp_sa_p, ylab="GDP", xlab="", col="steelblue")

par(mfrow=c(1,2))
acf(gdp_sa_p, main="" )
pacf(gdp_sa_p, main="" )

gdpp_fit = arima(gdp_sa_p, order=c(3,0,0))
gdpp_fit

auto.arima(gdp_sa_p)
##########################################

#과대적합
gdpp_fit = arima(gdp_sa_p, order=c(3,0,1))
gdpp_fit
gdpp_fit = arima(gdp_sa_p, order=c(4,0,0))
gdpp_fit

#잔차
gdpp_fit = arima(gdp_sa_p, order=c(3,0,0))
tsdiag(gdpp_fit)

#예측
par(mfrow=c(1,1))
plot(forecast(gdpp_fit, h=4), main="")
forecast(gdpp_fit, h=4)
########################################
