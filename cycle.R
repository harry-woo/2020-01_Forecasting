library(tseries)
library(mFilter)
library(ggfortify)
library(forecast)

# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
# https://community.rstudio.com/t/repeated-troubles-using-autoplot-with-time-series-data-and-forecast-objects/35965/2

rm(list = ls())

## 데이터 읽기 ##

gdp  <- read.csv("gdpq.csv", header = TRUE)
gdp_sa <- ts(gdp[,1]/1000, start = 1982, frequency = 4)

autoplot(gdp_sa, series = "계절조정") +
  xlab("Time") + ylab("GDP") + ggtitle(label = "분기별 실질 GDP (계절조정)") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))

plot(gdp_sa, ylab = "GDP", xlab = "Time", col = "steelblue")

# GDP 변동요인 분해 (계절요인은 원 자료에서 이미 조정됨)

# 추세변동 추출1 : 로그변환한 계절조정계열에 HP 필터를 적용
lgdp_sa <- log(gdp_sa)
lgdp_hp <- mFilter(lgdp_sa, filter = "HP")

# 추세변동 추출2 : 지수함수를 이용하여 GDP 추세계열로 전환
gdp_trend <- exp(lgdp_hp$trend)

# 계절조정 vs. 추세계열
forecast::autoplot(gdp_sa, series = "계절조정", lwd = 1.2) +
  autolayer(gdp_trend, series = "추세계열", lwd = 0.8, lty = 1) +
  xlab("Time") + ylab("GDP") + ggtitle(label = "실질 국내총생산 (분기별, 계절조정 vs 추세계열)") +
  scale_colour_manual(values = c("계절조정" = "steelblue", "추세계열" = "red")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"))

# 이동평균(3분기)을 통해 불규칙 변동을 제거, 지수함수로 재변환
gdp_sam <- exp((lgdp_sa + lag(lgdp_sa, -1)+ lag(lgdp_sa, 1))/3)

gdp_irregular <- gdp_sa / gdp_sam * 100

# 100을 기준으로 순환변동치 계산 (100 이상 호황, 이하 불황)
gdp_cycle = gdp_sam / gdp_trend * 100

# 순환변동
autoplot(gdp_cycle, lwd = 1, col = "steelblue") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red")


plot(gdp_cycle, main="순환변동요인", col="steelblue", ylab="", xlab="")
  abline(h = 100, col = "red", lty = 2)
  