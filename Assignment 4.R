install.packages("astsa")
library(astsa)
arma_sim=arima.sim(list(order=c(1,0,1), ar=c(0.9,.9), ma=c(-0.4,-0.4), n=200)
plot(arma_sim)
summary(arma_sim)
acf(arma_sim)
pacf(arma_sim)

library(fredr)
library(forecast)
fredr_set_key('0c099d2b1a8122207215330c85f8198e')

data=fredr(
  series_id = "JTSJOL",
  observation_start = as.Date("2000-01-01"))

jobs = ts(data$value,frequency = 12, start=c(2000,01))

jobs_fit <- Arima(jobs, order = c(1,1,0))
summary(jobs_fit)
checkresiduals(jobs_fit)
jobs_forecast=forecast(jobs_fit,h=3)
plot(unemployment_forecast)
