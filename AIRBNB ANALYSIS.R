library(tseries)
library(tidyverse)
library(ggplot2)
library(stats)
library(forecast)
library(TSA)
library(urca)
library(FinTS)
library(rugarch)

df = read_csv("C:/Users/sharv/Desktop/Time Series Project/Non Seasonal/ABNB.csv",show_col_types = FALSE)
df = select(df, Date, Close)
df$Date <- as.Date(df$Date, "%Y-%m-%d")

# Create a time series with daily data from 2020-12-10 to 2023-11-03
daily_data <- ts(df$Close, start = c(2021,1), end = c(2023,11), frequency = 365)

plot(daily_data, main = 'original_dataset')

#####################################################################

## Performing dickey Fuller Test

adf_test = adf.test(daily_data)
print(adf_test)

## Since p value > 0.05: fail to reject H0: it means non-stationary

###################################################################

## differencing

close_diff = diff(daily_data)

adf_test = adf.test(close_diff)

print(adf_test)

## Means the data has become stationary

## acf and pacf plots

acf(close_diff, lag.max = 30, main = 'ACF of differenced data')
pacf(close_diff, lag.max = 30, main = 'PACF of differenced data')

eacf(close_diff)

####################################################################################################################

# object to store ARIMA summary in
model_summary <- data.frame()

# loop to store
for(p in 0:10){
  for(q in 0:10){
    for (d in 1:1) {
      fit <- Arima(close_diff, order = c(p,d,q))
      
      s <- shapiro.test(rstandard(fit))
      
      # H0: The model does not show lack of fit
      # H1: not H0
      lb <- LB.test(fit, lag = 50)
      
      # gather everything into a single data frame
      # AIC, BIC, SHAPIRO OF RESIDUALS, LB TEST
      acc_ext <- data.frame(# arima order
        p,
        d,
        q,
        # goodness of fit
        LJUNG = lb$p.value,
        SHAPIRO = s$p.value,
        AIC = AIC(fit),
        BIC = BIC(fit)
      )
      
      # add ARIMA summary
      model_summary <- rbind(model_summary, acc_ext)
    }
  }
}

# show summary
filter(model_summary[order(model_summary$BIC, decreasing = FALSE),], LJUNG > 0.05)


#################################################################################################################
## WE choose (0,1,4) Model
best_fit <- Arima(daily_data, order = c(0,1,4))

acf(rstandard(fit))
pacf(rstandard(fit))
qqnorm(rstandard(fit))
qqline(rstandard(fit))

plot(forecast(best_fit, h = 60))


#################################################################################################################


#GARCH MODELLING

# we now need to fit either abs(return) or sqaure(return)


#GARCH - square(return)
print(daily_data)

dataset_nd <- daily_data
square_return <- dataset_nd^2



eacf(square_return)


adf.test(square_return)


square_return <- diff(square_return, differences = 1)

adf.test(square_return)


##Now Finding best ARIMA model for square(return)

# object to store ARIMA summary in
model_summary <- data.frame()

# loop to store
for(p in 0:7){
  for(q in 0:10){
    for (d in 1:1) {
      fit <- Arima(square_return, order=c(p,d,q), method="ML")
      
      s <- shapiro.test(rstandard(fit))
      
      # H0: The model does not show lack of fit
      # H1: not H0
      lb <- LB.test(fit, lag = 50)
      
      # gather everything into a single data frame
      # AIC, BIC, SHAPIRO OF RESIDUALS, LB TEST
      acc_ext <- data.frame(# arima order
        p,
        d,
        q,
        # goodness of fit
        LJUNG = lb$p.value,
        SHAPIRO = s$p.value,
        AIC = AIC(fit),
        BIC = BIC(fit)
      )
      
      # add ARIMA summary
      model_summary <- rbind(model_summary, acc_ext)
    }
  }
}

# show summary
filter(model_summary[order(model_summary$BIC, decreasing = FALSE),], LJUNG > 0.05)


tail(dataset_nd)

## Best forecast is 3,1,2
##FORECASTING

garch_model <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(3, 2)), distribution.model = "norm")
garch_fit <- ugarchfit(garch_model, data = daily_data)
y_pred <- ugarchforecast(garch_fit, n.ahead = 365)

sfinal <- garch_model
setfixed(sfinal) <- as.list(coef(garch_fit))
sim <- ugarchpath(spec = sfinal,
                  m.sim = 1,
                  n.sim = 1*60,
                  rseed = 16)

last_value = tail(dataset_nd)[1]
print(last_value)


p <- ts(fitted(sim), frequency = 365, start = c(2023, 11, 4))
print(p)
p_1 = last_value - p[1]



#print(c(dataset_nd, original_form))

#par(mfrow=c(2,1))
plot(dataset_nd, type = "l")
lines(p+p_1+p_1, col='red')

