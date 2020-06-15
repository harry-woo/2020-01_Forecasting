library(tseries)
library(forecast)

# https://blogs.oracle.com/datascience/introduction-to-forecasting-with-arima-in-r
# https://rstudio-pubs-static.s3.amazonaws.com/303786_f1b99d6b7e9346c4b1488a174bab839a.html
# https://rc2e.com/timeseriesanalysis
# https://otexts.com/fppkr/arima-r.html

## 데이터 읽기 ##

gdp_d  <- read.csv("gdpq.csv", header = TRUE)
gdp_sa <- ts(gdp_d[,1]/1000, start=1982, frequency=4)
gdp_sa_p <- ts(gdp_d[,2], start=1982, frequency=4)

# subsetting?
# gdp_sa_p = window(gdp_sa_p1, 2000, 2020)

library(zoo)
gr <- as.zoo(gdp_sa_p)
index(gr)

forecast::ggtsdisplay(gdp_sa_p)

ggAcf(gdp_sa_p)
ggPacf(gdp_sa_p)

# 성장률은 로그변환 후 차분과 근사함. 일종의 차분을 통하여 추세가 제거됨.
# gghistogram(gdp_sa_p, add.kde = TRUE)
# shapiro.test(gdp_sa_p)

# Stationarity 
adf.test(gdp_sa_p, alternative = "stationary", k = 0) #안정시계열
plot(gdp_sa_p, ylab="GDP Growth Rate", xlab="", col="steelblue")
abline(h = 0, lty = 2, col = "gray")

par(mfrow=c(1,2))
acf(gdp_sa_p, main="" ) 
# 모든 자기상관도표는 시차 0에서 acf=1
# 점선이 유의수준에 따른 기각역

pacf(gdp_sa_p, main="" )

#륭-박스 검정은 m차까지 자기상관관계가 존재하지 않는다는 귀무가설

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
