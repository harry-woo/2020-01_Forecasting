library(tseries)
library(mFilter)

## 데이터 읽기 ##

gdp_d  <- read.csv("gdpq.csv", header = TRUE)
gdp_sa <- ts(gdp_d[,1]/1000, start=1982, frequency=4)
gdp_sa_p <- ts(gdp_d[,2], start=1982, frequency=4)

plot(gdp_sa, ylab="GDP", xlab="", col="steelblue")

# GDP 변동요인 분해 
lgdp.hp = mFilter(log(gdp_sa),filter="HP")   # Hodrick-Prescott filter
gdp_t = exp(lgdp.hp$trend)
gdpsam = exp((log(gdp_sa)+lag(log(gdp_sa),-1)+lag(log(gdp_sa),1))/3)
gdp_i = gdp_sa/gdpsam*100
gdp_c = gdpsam/gdp_t*100
par(mfrow=c(1,1))
plot(gdp_t, main="추세변동요인", col="steelblue", ylab="", xlab="")
 lines(gdp_sa, col=2)
plot(gdp_c, main="순환변동요인", col="steelblue", ylab="", xlab="")
