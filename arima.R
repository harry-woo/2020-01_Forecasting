library(tseries)
library(forecast)
library(gridExtra)

# https://blogs.oracle.com/datascience/introduction-to-forecasting-with-arima-in-r
# https://rstudio-pubs-static.s3.amazonaws.com/303786_f1b99d6b7e9346c4b1488a174bab839a.html
# https://rc2e.com/timeseriesanalysis
# https://otexts.com/fppkr/arima-r.html

## 데이터 읽기 ##

gdp_d  <- read.csv("gdpq.csv", header = TRUE)
# gdp_sa <- ts(gdp_d[,1]/1000, start=1982, frequency=4)
gdp_sa_p <- ts(gdp_d[,2], start=1982, frequency=4)

# subsetting?
# gdp_sa_p = window(gdp_sa_p1, 2000, 2020)

library(zoo)
gr <- as.zoo(gdp_sa_p)
index(gr)

?ggtsdisplay

# 성장률은 로그변환 후 차분과 근사함. 일종의 차분을 통하여 추세가 제거됨.
# gghistogram(gdp_sa_p, add.kde = TRUE)
# shapiro.test(gdp_sa_p)

# Stationarity 
adf.test(gdp_sa_p, alternative = "stationary", k = 0) # 단위근이 있다는 귀무가설을 기각. 단위근이 없는 I(0) 적분계열.
autoplot(gdp_sa_p, lwd = 1.2, col = "royalblue") + xlab("Time") + ylab("GDP Growth Rate") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold")) + 
  ggtitle(label = "전기대비 GDP 성장율 (계절조정계열, 분기별, 실질)") +
  geom_hline(yintercept = 0, lty = 2, col = "black")

acf <- ggAcf(gdp_sa_p) + ggtitle("상관도표") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
pacf <- ggPacf(gdp_sa_p) + ggtitle("부분상관도표") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
grid.arrange(acf, pacf, ncol = 2)

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

# 모형 간결의 원칙

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
