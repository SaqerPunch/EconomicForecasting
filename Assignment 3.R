install.packages("fredr")
library(fredr)
fredr_set_key('0c7ff83034c0d6c213a7c0faa98d633a')

#Question 1
data=fredr(
  series_id = "LNU01375379", 
  observation_start = as.Date("2010-01-01"),
  observation_end = as.Date("2023-06-012"))

data
LFPR =ts(data$value, frequency=12, start=c(2010,01))
plot(LFPR)
acf(LFPR)

#Question 2
decom_LFPR = decompose(LFPR, type = "multiplicative")
plot(decom_LFPR)
trnd = decom_LFPR$trend
season = decom_LFPR$seasonal
rmdr = decom_LFPR$random
trnd
season
rmdr

#Question 4
stl_LFPR = stl(LFPR, s.window = 1000, t.window = 1000)
plot(stl_LFPR)
