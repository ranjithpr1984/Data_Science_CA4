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
    
    #jpeg(paste(names(df)[j],"_",house_type,"_model_comparison.jpeg",sep = ""),width = 850, height = 435)
    #par(mfcol = c(1, 2))
    #par(mar=c(2,1,1,1))
    
    plot(forecast(arima_fit, level = c(90), h = 36),
         main = paste(names(df)[j], house_type, "house price Auto ARIMA forcast"))
    plot(forecast(tslm_fit, level = c(90), h = 36),
         main = paste(names(df)[j], house_type, "house price TSLM forcast"))

    #dev.off()
  }
  par(lopar)
  return(county_accuracy)
}

# Compare predictive models for new house price
prop_new_models <- compare_ts_models(prop_price_ts_new, "new")

# Compare predictive models for second-hand house price
prop_second_models <- compare_ts_models(prop_price_ts_second, "second-hand")

# Function to build and evaluate ARIMA model for all counties
evaluate_arima_model <- function(df, house_type) {
  lopar <- par(no.readonly = TRUE)
  Ljung_Box_pvalue <- numeric(ncol(df)-1)
  for(j in 2:ncol(df)) {
    fit <- auto.arima(df[,j])
    Ljung_Box_pvalue[j-1] <- Box.test(fit$residuals, type = "Ljung-Box")$p.value
    
    #jpeg(paste(names(df)[j],"_",house_type,"_ARIMA_fit.jpeg",sep = ""),width = 850, height = 435)
    qqnorm(fit$residuals,
           main = paste(names(df)[j], house_type, "house price ARIMA fit"))
    qqline(fit$residuals)
    #dev.off()
  }
  par(lopar)
  return(Ljung_Box_pvalue)
}


county_stats_new$Ljung_Box_pvalue <- evaluate_arima_model(prop_price_ts_new, 
                                                          "new")
county_stats_second$Ljung_Box_pvalue <- evaluate_arima_model(prop_price_ts_second, 
                                                          "second-hand")
county_stats_new$Ljung_Box_pvalue
county_stats_second$Ljung_Box_pvalue

# Funtion Validate arima model for all the counties.
# This function find correlation between actual values and prediction.
# Also plot prediction and actual value to visualise how well the prediction
# fits actual value
validate_arima_model <- function(df, house_type) {
  #lopar <- par(no.readonly = TRUE)
  n_county <- ncol(df) - 1;
  corr_accuracy <- data.frame(names(df[,2:ncol(df)]),
                              numeric(n_county),
                              numeric(n_county),
                              numeric(n_county),
                              numeric(n_county),
                              numeric(n_county))

  names(corr_accuracy) <- c("county","predicted_point",
                            "predicted_Lo_80","predicted_Hi_80",
                            "predicted_Lo_95","predicted_Hi_95")
  corr_accuracy$no_trend <- FALSE
    
  for(j in 2:ncol(df)) {
    ts_train <- window(df[,j], 
                       start = c(2015,1),
                       end = c(2018,12))
    ts_test <- window(df[,j], 
                       start = c(2019,1))
    fit <- auto.arima(ts_train)
    
    ts_predict <- forecast(fit,16)
    actl_pred <- cbind(actuals = ts_test, predicted = ts_predict)
    
    act_pred_cor <- cor(actl_pred)[1,]
    
    corr_accuracy[j-1,]$predicted_point <- act_pred_cor["predicted.Point Forecast"]
    corr_accuracy[j-1,]$predicted_Lo_80 <- act_pred_cor["predicted.Lo 80"]
    corr_accuracy[j-1,]$predicted_Hi_80 <- act_pred_cor["predicted.Hi 80"]
    corr_accuracy[j-1,]$predicted_Lo_95 <- act_pred_cor["predicted.Lo 95"]
    corr_accuracy[j-1,]$predicted_Hi_95 <- act_pred_cor["predicted.Hi 95"]
    corr_accuracy[j-1,]$no_trend <- (min(ts_predict[["mean"]]) == max(ts_predict[["mean"]]))
    
    #jpeg(paste(names(df)[j],"_",house_type,"_ARIMA_PREDICT_ACCURACY.jpeg",sep = ""),width = 850, height = 435)
    plot(ts_predict,
         include = 1,
         main = paste(names(df)[j], house_type, "house price Prediction accuracy"))
    lines(ts_test, col = "black",lwd = 2)
    #dev.off()
  }
  par(lopar)
  return(corr_accuracy)
}

corr_accuracy_new <- validate_arima_model(prop_price_ts_new, "new")
corr_accuracy_second <- validate_arima_model(prop_price_ts_second,"second-hand")

View(corr_accuracy_new)
View(corr_accuracy_second)

forecast_one_year <- function(df,county, house_type) {
  county_price_rise <- data.frame(county)
  county_price_rise$price_rise_k <- 0
  
  for(i in 1:length(county)) {
    col_indx <- which(colnames(df)==county[i])
    fit <- auto.arima(df[,col_indx])
    ts_predict <- forecast(fit,12)
    min_price <- round(min(ts_predict[["mean"]]))
    max_price <- round(max(ts_predict[["mean"]]))
    
    #jpeg(paste(county[i],"_",house_type,"house_price_forecast.jpeg",sep = ""),width = 850, height = 435)
    plot(ts_predict, include = 1,
         main = paste(house_type, "house price one year forcast for", county[i]),
         xlab = "Year",
         ylab = "House price (in 1000s)")
    abline(h = min_price, col = 3)
    abline(h = max_price, col = 3)
    axis(side = 4, at = c(min_price,max_price))
    text(2020.4,
         mean(c(min_price,max_price)),
         paste("Expected rise in price : ", 
               max_price-min_price, "k", sep = ""),
         adj = 0,
         col = 2)
    county_price_rise[i,]$county <- county[i]
    county_price_rise[i,]$price_rise_k <- max_price-min_price
    #dev.off()
  }
  return(county_price_rise)
}

price_rise_new <- forecast_one_year(prop_price_ts_new,
                                    c("Cork","Kildare", "Limerick",
                                      "Louth", "Waterford"),
                                    "New")
price_rise_second <- forecast_one_year(prop_price_ts_second,
                                       c("Kildare", "Meath", "Wicklow","Westmeath",
                                         "Offaly", "Monaghan", "Tipperary", "Clare",
                                         "Laois", "Galway", "Waterford", "Kerry",
                                         "Cork"),
                                       "Second-hand")
View(price_rise_new)
View(price_rise_second)
