install.packages("quantmod")
install.packages("forecast")
library(quantmod)
library(forecast)

getSymbols("TSLA" , from = "2022-09-01")
tesla_cls = TSLA$TSLA.Close
plot(tesla_cls, main = "Tesla Closing Price")

tesla_mm = meanf(tesla_cls,h=6)
plot(tesla_mm)

tesla_drift = rwf(tesla_cls,h=6, drift = TRUE)
plot(tesla_drift)

#Confidence Bands
tesla_mm
tesla_drift

#Residuals and ACF
tesla_mm$fitted
tesla_mm$residuals
checkresiduals(tesla_mm)

tesla_drift$fitted
tesla_drift$residuals
checkresiduals(tesla_drift)

#RSME
tesla_test=tesla_cls[194:199]
tesla_test$TSLA.Close

forecast_error=(tesla_test-tesla_mm$mean)
rmse_mean =sqrt(sum(forecast_error^2)/length(forecast_error))
rmse_mean

forecast_error=(tesla_test-tesla_drift$mean)
rmse_drift=sqrt(sum(forecast_error^2)/length(forecast_error))
rmse_drift


