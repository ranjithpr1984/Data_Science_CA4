#install.packages("tseries")
library(tseries)
library(forecast)

opar <- par(no.readonly = TRUE)

# Read CSV monthly mean new house price CSV to R dataframe
prop_price_NMS <- read.csv("Monthly_Averaage_New_Property_price.csv")

# Read CSV monthly mean second-house price CSV to R dataframe
prop_price_SMS <- read.csv("Monthly_Averaage_Second_hand_Property_price.csv")

# Function to convert house price to 1000s
# to consider only significant differences
convert_price_to_1000s <- function(df) {
  for(j in 2:ncol(df)) df[,j] <- round(df[,j]/1000)
  return(df)
}

# Convert new house price to 1000s
prop_price_NMS <- convert_price_to_1000s(prop_price_NMS)

# Convert second-house price to 1000s
prop_price_SMS <- convert_price_to_1000s(prop_price_SMS)

# Function to Convert house price fields to time series type
convert_to_ts <- function(df) {
  
  # Find year and month from data frame
  start_year <- as.numeric(substr(as.character(min(df$Sale_YearMonth)),1,4))
  start_month <- as.numeric(substr(as.character(min(df$Sale_YearMonth)),5,6))
 
  for(j in 2:ncol(df)) {
    df[,j] <- ts(data = df[,j],
                 start = c(start_year, start_month),
                 frequency = 12)
  }
  return(df)
}

# Convert monthly mean new house price to time series
prop_price_ts_new <- convert_to_ts(prop_price_NMS)

# Convert monthly mean second-hand house price to time series
prop_price_ts_second <- convert_to_ts(prop_price_SMS)

str(prop_price_ts_new$Carlow)
cycle(prop_price_ts_new$Carlow)

par(mfrow = c(1, 2))
tmp <- prop_price_ts_new$Roscommon
plot(tmp, 
     main = "Roscommon new house price trend(2010-2020)",
     xlab = "Year",
     ylab = "House price(in 1000s)")
abline(reg=lm(tmp ~ time(tmp)))

tmp <- window(tmp, start = 2015)
plot(tmp, 
     main = "Roscommon new house price trend(2015-2020)",
     xlab = "Year",
     ylab = "House price(in 1000s)")
abline(reg=lm(tmp ~ time(tmp)))
par(opar)

# Filter data since 2015 and convert to time series
prop_price_ts_new <- convert_to_ts(subset(prop_price_NMS,
                                          Sale_YearMonth >= "201501"))
prop_price_ts_second <- convert_to_ts(subset(prop_price_SMS,
                                             Sale_YearMonth >= "201501"))

# Function plot house price trend and season trend
plot_counties <- function(df, house_type) {
  lopar <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2))
  par(mar=c(2,1,1,1))
  
  for(j in 2:ncol(df)) {
    plot(df[,j]
         ,main = paste(names(df)[j], house_type,"house price trend")
         ,xlab = "Year" ,ylab = "House price(in 1000s)")
    abline(reg=lm(df[,j] ~ time(df[,j])))
    
    seasonplot(df[,j],
               12,
               col = c(1:6),
               year.labels = TRUE,
               main = paste(names(df)[j],house_type," house price Seasonal trend"),
               xlab = "Month", ylab = "House price(in 1000s)")
    #    boxplot(df[,j] ~ cycle(df[,j]),
    #            xlab="Date", 
    #            ylab = "House price (1000's)" ,
    #            main ="Monthly Houeprice Boxplot from 2010 to 2020")
    #    
    #    seasonal_decomposition <- stl(df[,j], s.window="period")
    #    plot(seasonal_decomposition)
  }
  
  par(lopar)
}

# Plot house price trend of new houses
plot_counties(prop_price_ts_new, "new")

# Plot house price trend of seond-hand houses
plot_counties(prop_price_ts_second, "second-hand")

par(opar)

# Function plot season decomposition of house price trend
seasonal_decomp_counties <- function(df, house_type) {
  for(j in 2:ncol(df)) {
    #jpeg(paste(names(df)[j],"_",house_type,"_house_season_decomp.jpeg",sep = ""),width = 850, height = 435)
    seasonal_decomposition <- stl(df[,j], s.window="period")
    plot(seasonal_decomposition,
         main = paste(names(df)[j], house_type,"house price seasonal decomposition"))
    #dev.off()
  }
}

# Plot seasonal decomposition of new houses price
seasonal_decomp_counties(prop_price_ts_new, "new")

# Plot seasonal decomposition of seond-hand houses price
seasonal_decomp_counties(prop_price_ts_second, "second-hand")

# Create data frame to store statistics of each county
county_stats_new <- data.frame(data.frame(names(prop_price_ts_new)[2:27]))
names(county_stats_new)[1] <- "county"
county_stats_second <- county_stats_new

# Find Augmented Dickey-Fuller p-value all counties
find_adf_pvalues <- function(df, county) {
  adf_pvalue <- numeric(length(county))
  for(i in 1:length(county)) {
    col_indx <- which(colnames(df)==county[i])
    adf_pvalue[i] <- adf.test(df[,col_indx], k = 12)$p.value
  }
  return(adf_pvalue)
}

county_stats_new$adf_pvalue_k12 <- find_adf_pvalues(prop_price_ts_new,
                                                    county_stats_new$county)
county_stats_second$adf_pvalue_k12 <- find_adf_pvalues(prop_price_ts_second,
                                                       county_stats_second$county)
View(county_stats_new)
View(county_stats_second)

# Function Compute accuracy of auto ARIMA model and TSLM model 
# for all counties also plot 3 years forecast using the model
compare_ts_models <- function(df, house_type) {
  lopar <- par(no.readonly = TRUE)
  par(mfcol = c(1, 2))
  par(mar=c(2,1,1,1))
  county_accuracy <- data.frame(names(df[,2:ncol(df)]),
                                numeric(ncol(df) - 1),
                                numeric(ncol(df) - 1),
                                numeric(ncol(df) - 1),
                                numeric(ncol(df) - 1),
                                numeric(ncol(df) - 1),
                                numeric(ncol(df) - 1)
                                )
  names(county_accuracy) <- c("county","ARIMA_AIC","TSLM_AIC",
                              "ARIMA_BIC","TSLM_BIC",
                              "ARIMA_MAPE","TSLM_MAPE")
  county_accuracy$Model_selected <- as.character("")

  for(j in 2:ncol(df)) {

    time_series <- df[,j]
    county <- names(df)[j]
    county_accuracy[j-1,]$county <- county
    
    arima_fit <- auto.arima(time_series)
    county_accuracy[j-1,]$ARIMA_AIC <- AIC(arima_fit)
    county_accuracy[j-1,]$ARIMA_BIC <- BIC(arima_fit)
    county_accuracy[j-1,]$ARIMA_MAPE <- accuracy(arima_fit)[1,5]
    
    
    tslm_fit <- tslm(time_series ~ trend + season )

    county_accuracy[j-1,]$TSLM_AIC <- AIC(tslm_fit)
    county_accuracy[j-1,]$TSLM_BIC <- BIC(tslm_fit)
    county_accuracy[j-1,]$TSLM_MAPE <- accuracy(tslm_fit)[1,5]

    if(county_accuracy[j-1,]$TSLM_AIC < county_accuracy[j-1,]$ARIMA_AIC  &
       county_accuracy[j-1,]$TSLM_BIC < county_accuracy[j-1,]$ARIMA_BIC )
      county_accuracy[j-1,]$Model_selected <- "TSLM"
    else
      county_accuracy[j-1,]$Model_selected <- "ARIMA"
    
    jpeg(paste(names(df)[j],"_",house_type,"_model_comparison.jpeg",sep = ""),width = 850, height = 435)
    par(mfcol = c(1, 2))
    par(mar=c(2,1,1,1))
    
    autoplot(forecast(arima_fit, level = c(90), h = 36),
         main = paste(names(df)[j], house_type, "Auto ARIMA forcast"))
    autoplot(forecast(tslm_fit, level = c(90), h = 36),
         main = paste(names(df)[j], house_type, "TSLM forcast"))

    dev.off()
  }
  par(lopar)
  return(county_accuracy)
}

# Plot 3 years forcast for new houses
prop_new_models <- compare_ts_models(prop_price_ts_new, "new")

# Plot 3 years forcast for seond-hand houses price
prop_second_models <- compare_ts_models(prop_price_ts_second, "second-hand")

#tmp <- prop_price_ts_new$Leitrim
#ts_decompose <- stl(tmp, "periodic")
#ts_decompose
#ts_seasonal_adjust <- seasadj(ts_decompose)
#plot(tmp, type = "l")
#plot(ts_seasonal_adjust, type = "l")


#
#seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels
#           = TRUE, main = "Seasonal plot: Airpassengers")
#
#kpss.test(prop_price_ts_new$Dublin)
#nsdiffs(prop_price_ts_new$Dublin)
#ndiffs(prop_price_ts_new$Dublin)
#stationaryTS <- diff(prop_price_ts_new$Dublin, differences = 1)
#plot(stationaryTS, type = "l", main = "Differenced and Stationary") 
#
#fit <- auto.arima(prop_price_ts_second$Dublin)
#fit
#accuracy(fit)
#
#fit <- Arima(prop_price_ts_new$Dublin, order = c(0, 1, 1))
#fit
#accuracy(fit)
#
#qqnorm(fit$residuals)
#qqline(fit$residuals)
#
#forecast(fit, 3)
#
#
#Box.test(fit$residuals, type = "Ljung-Box")
#
#plot(window(prop_price_ts_new$Carlow,start=2019))
#
#install.packages("forecast")
#library(forecast)
#
#pacf(log(window(prop_price_ts_new$Carlow,start=2015)))
#?stl
#ts_decompose <- stl(AirPassengers, "periodic")
#
#
#trained_model <- lm(JohnsonJohnson ~ c(1:length(JohnsonJohnson)))
#
#  as.numeric(substr(as.character(min(prop_price_NMS[,1])),5,6))
#
#substr("abcdef", 2, 4)
#
#str(prop_price_ts_new)
#cycle(prop_price_ts_new$Carlow)
#cycle(window(prop_price_ts_new$Carlow,start=2015))
#
#
#tmp <- ts(data = c(1,2,3,6,8,10,13,16,19),start = 2010 , frequency = 1)
#adf.test(tmp)
#plot(tmp)
