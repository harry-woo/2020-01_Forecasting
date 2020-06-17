
# https://blogs.oracle.com/datascience/introduction-to-forecasting-with-arima-in-r
# https://rstudio-pubs-static.s3.amazonaws.com/303786_f1b99d6b7e9346c4b1488a174bab839a.html
# https://rc2e.com/timeseriesanalysis
# https://otexts.com/fppkr/arima-r.html



rm(list = ls())
library(tseries)
library(forecast)
library(gridExtra)
library(ggfortify)
library(zoo)
library(dplyr)

#
gdp <- read.csv("gdpq.csv", header = TRUE)
gdp_gr <- ts(gdp[,2], start=1982, frequency=4)

# subsetting?
# gdp_gr2 <- window(gdp_gr, 2000, 2020)

# library(zoo)
# gr <- as.zoo(gdp_gr)
# index(gr)

# 성장률은 로그변환 후 차분과 근사함. 일종의 차분을 통하여 추세가 제거됨.
# gghistogram(gdp_gr, add.kde = TRUE)
# shapiro.test(gdp_gr)

# Stationarity
# 추세가 보임
autoplot(gdp_gr) + xlab("Time") + ylab("GDP Growth Rate") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold")) + 
  ggtitle(label = "전기대비 GDP 성장율 (계절조정계열, 분기별, 실질)") +
  geom_hline(yintercept = 0, lty = 2, col = "black") +
  geom_smooth(aes(x = as.Date(time(gdp_gr)), y = gdp_gr),
              se = FALSE, method = "lm", lty = 1, col = "blue")

# 차분 후 확인
autoplot(diff(gdp_gr)) + xlab("Time") + ylab("GDP Growth Rate") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold")) + 
  ggtitle(label = "전기대비 GDP 성장율 (계절조정계열, 분기별, 실질)") +
  geom_hline(yintercept = 0, lty = 2, col = "black") +
  geom_smooth(aes(x = as.Date(time(diff(gdp_gr))), y = diff(gdp_gr)),
              se = FALSE, method = "lm", lty = 1, col = "blue")

adf.test(gdp_gr, alternative = "stationary", k = 0) # 단위근이 있다는 귀무가설을 기각. 단위근이 없는 I(0) 적분계열.
adf.test(diff(gdp_gr), alternative = "stationary", k = 0) # 단위근이 있다는 귀무가설을 기각. 단위근이 없는 I(0) 적분계열.

acf1 <- ggAcf(gdp_gr) + ggtitle("상관도표") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
pacf1 <- ggPacf(gdp_gr) + ggtitle("부분상관도표") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
grid.arrange(acf1, pacf1, ncol = 2)

<<<<<<< HEAD
acf <- ggAcf(diff(gdp_sa_p)) + ggtitle("상관도표") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
pacf <- ggPacf(diff(gdp_sa_p)) + ggtitle("부분상관도표") +
=======
acf2 <- ggAcf(diff(gdp_gr)) + ggtitle("상관도표") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
pacf2 <- ggPacf(diff(gdp_gr)) + ggtitle("부분상관도표") +
>>>>>>> 1109d53006c8d864a89aa004c96fea1eb08d6986
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))
grid.arrange(acf2, pacf2, ncol = 2)


ggseasonplot(gdp_gr, year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(diff(gdp_gr), year.labels = TRUE, year.labels.left = TRUE)

# 모든 자기상관도표는 시차 0에서 acf=1
# 점선이 유의수준에 따른 기각역
# 륭-박스 검정은 m차까지 자기상관관계가 존재하지 않는다는 귀무가설

spectrum(gdp_sa)
spectrum(gdp_gr)

diff(diff(gdp_sa),4)
diff(gdp_sa, 4)

gdpp_fit = Arima(gdp_gr, order=c(1,1,1))
gdpp_fit

auto.arima(gdp_gr, seasonal = TRUE)
auto.arima(gdp_gr, seasonal = FALSE)

auto.arima(gdp_gr2, seasonal = TRUE)

# ggtsdisplay(gdp_gr)

?auto.arima()

df <- data.frame(p = double(), d = double(), q = double(), 
                 aic = double(), aicc = double(), bic = double())

for(p in 0:4){
  for(d in 0:1){
    for(q in 0:4){
      df <- rbind(df, data.frame(p = p, d = d, q = q, 
                                 Arima(gdp_gr, order = c(p, d, q))[c("aic", "aicc", "bic")]))
    }
  }
}

df[df$aic == min(df$aic),]
df[df$aicc == min(df$aicc),]
df[df$bic == min(df$bic),]

df
# 모형 간결의 원칙

##########################################

#과대적합

gdpp_fit = arima(gdp_gr, order=c(3,0,1))
gdpp_fit
gdpp_fit = arima(gdp_gr, order=c(4,0,0))
gdpp_fit

#잔차
gdpp_fit = arima(gdp_gr, order=c(3,0,0))
tsdiag(gdpp_fit)

#예측
par(mfrow=c(1,1))
plot(forecast(gdpp_fit, h=4), main="")
forecast(gdpp_fit, h=4)

########################################


gdp_gr2 <- window(gdp_gr, 2000, 2020)
auto.arima(gdp_gr2)
gdpp_fit2 = Arima(gdp_gr2, order = c(0, 1, 1))
plot(forecast(gdpp_fit2, h=12), main="")
forecast(gdpp_fit2, h=12)

gdp_gr3 <- window(gdp_gr, 2010, 2020)
auto.arima(gdp_gr3)
gdpp_fit3 = Arima(gdp_gr3, order = c(1, 1, 1))
plot(forecast(gdpp_fit3, h=12), main="")
forecast(gdpp_fit2, h=12)