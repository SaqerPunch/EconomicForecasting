install.packages("tseries")
install.packages("forecast")
install.packages("fredr")
install.packages("ISLR")

library(tseries)
library(forecast)
library(fredr)
library(ISLR)

fredr_set_key('f6ec14e9344ff200b0e9ac56cf5172e1')

house_price = fredr(series_id = "ASPUS",  observation_start = as.Date("1980-01-01"), observation_end = as.Date("2022-01-01"))
HP_ts = ts(house_price$value, frequency = 4, start = c(1980,01))
plot(HP_ts)

linearmodel_HP = tslm(HP_ts~trend+season)
summary(linearmodel_HP)
checkresiduals(linearmodel_HP)

rgdp= fredr(series_id = "GDPC1",  observation_start = as.Date("1980-01-01"), observation_end = as.Date("2022-01-01"))
rgdp_ts = ts(rgdp$value, frequency = 4, start = c(1980,01))
plot(rgdp_ts)

linearmodel_HP_RGDP = tslm(HP_ts~trend+rgdp_ts)
summary(linearmodel_HP_RGDP)
checkresiduals(linearmodel_HP_RGDP)
head(rgdp_ts)

predict(linearmodel_HP_RGDP, data.frame(trend = c(169,170,171), rgdp_ts=c(21000,22000,23000), interval = "confidence"))
plot(rgdp_ts,residuals(linearmodel_HP_RGDP))
plot(fitted(linearmodel_HP_RGDP), residuals(linearmodel_HP_RGDP))


data_rgdp_hp=data.frame(rgdp_ts, HP_ts)
sample_rows<-sample(nrow(data_rgdp_hp), 0.75*nrow(data_rgdp_hp))
data_train<-data_rgdp_hp[sample_rows,]
data_test<-data_rgdp_hp[-sample_rows,]

model_train=lm(HP_ts~rgdp_ts,data=data_train)
data_test$pred<-predict(model_train, data_test)
data_test
