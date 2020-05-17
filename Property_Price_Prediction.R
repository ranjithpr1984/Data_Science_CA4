opar <- par(no.readonly = TRUE)
# Read Montly average new property price to R data frame
prop_price_NMS <- read.csv("Monthly_Averaage_New_Property_price.csv")

# Read Montly average new property price to R data frame
prop_price_SMS <- read.csv("Monthly_Averaage_Second_hand_Property_price.csv")

# Convert house price fields to time series type
convert_to_ts <- function(df) {
  min_month <- min(df[,1])
  ts_start <- c(as.numeric(substr(as.character(min_month),1,4)),
                as.numeric(substr(as.character(min_month),5,6)))
  for(j in 2:ncol(df)) {
    df[,j] <- ts(data = round(df[,j]/1000),
                 start = ts_start,
                 frequency = 12)
  }
  return(df)
}

#prop_price_ts_new <- convert_to_ts(prop_price_NMS)
#prop_price_ts_second <- convert_to_ts(prop_price_SMS)

round(prop_price_NMS$Carlow/1000)

prop_price_ts_new <- convert_to_ts(subset(prop_price_NMS,Sale_YearMonth >= "201501"))
prop_price_ts_second <- convert_to_ts(subset(prop_price_SMS,Sale_YearMonth >= "201501"))

plot_counties <- function(df) {
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2))
  par(mar=c(2,1,1,1))
  
  for(j in 2:ncol(df)) {
    plot(df[,j]
         ,main = paste(names(df)[j]," price trend")
         ,xlab = "Year"
         ,ylab = "House price")
    abline(reg=lm(df[,j] ~ time(df[,j])))
    seasonplot(df[,j],
               12,
               col = c(1:6),
               year.labels = TRUE,
               main = paste(names(df)[j]," Seasonal plot")
    )
  }
  
  par(lopar)
}

plot_counties(prop_price_ts_new)
plot_counties(prop_price_ts_second)
par(opar)

seasonplot_counties <- function(df) {
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(3, 3))
  par(mar=c(2,1,1,1))
  
  for(j in 2:ncol(df)) {
    seasonplot(df[,j],
               12,
               col = c(1:6),
               year.labels = TRUE,
               main = paste(names(df)[j]," Seasonal plot")
               )
  }
  
  par(lopar)
}

seasonplot_counties(prop_price_ts_new)

seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels
           = TRUE, main = "Seasonal plot: Airpassengers")

plot(prop_price_ts_second$Dublin)
plot(log(prop_price_ts_second$Dublin))

install.packages("tseries")
library(tseries)

adf.test(prop_price_ts_second$Dublin)
?abline

?stl
ts_decompose <- stl(prop_price_ts_second$Dublin, "periodic")
ts_decompose

ts_seasonal_adjust <- seasadj(ts_decompose)

plot(prop_price_ts_second$Dublin, type = "l")
plot(ts_seasonal_adjust, type = "l")

seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels
           = TRUE, main = "Seasonal plot: Airpassengers")

kpss.test(prop_price_ts_new$Dublin)
nsdiffs(prop_price_ts_new$Dublin)
ndiffs(prop_price_ts_new$Dublin)
stationaryTS <- diff(prop_price_ts_new$Dublin, differences = 1)
plot(stationaryTS, type = "l", main = "Differenced and Stationary") 

fit <- auto.arima(prop_price_ts_second$Dublin)
fit
accuracy(fit)

fit <- Arima(prop_price_ts_new$Dublin, order = c(0, 1, 1))
fit
accuracy(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)

forecast(fit, 3)


Box.test(fit$residuals, type = "Ljung-Box")

plot(window(prop_price_ts_new$Carlow,start=2019))

install.packages("forecast")
library(forecast)

pacf(log(window(prop_price_ts_new$Carlow,start=2015)))
?stl
ts_decompose <- stl(AirPassengers, "periodic")


trained_model <- lm(JohnsonJohnson ~ c(1:length(JohnsonJohnson)))

  as.numeric(substr(as.character(min(prop_price_NMS[,1])),5,6))

substr("abcdef", 2, 4)

str(prop_price_ts_new)
cycle(prop_price_ts_new$Carlow)
cycle(window(prop_price_ts_new$Carlow,start=2015))


tmp <- ts(data = c(1,2,3,6,8,10,13,16,19),start = 2010 , frequency = 1)
adf.test(tmp)
plot(tmp)
