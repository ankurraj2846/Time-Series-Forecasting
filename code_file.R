data <- read.csv("/Users/AR/Desktop/Fractal_test/train.csv")

install.packages("forecast")
install.packages("tseries")

library(forecast)
library(tseries)

attach(data)

id <- unique(Item_ID)

## fresh_data data frame will store the forecasted values
fresh_data <- data.frame(ID = numeric(), Sales = numeric(), price = numeric())

n <- length(unique(Item_ID))

for (i in 1:n){
  
  k <- id[i] 
  
  new_data <- subset(data, Item_ID==k, select=c(Item_ID, Datetime, Price, Number_Of_Sales))
  
  new_data$Datetime = as.Date(new_data$Datetime)
  
  p_p <- ts(new_data$Price, frequency = 12, start = min(new_data$Datetime), end = max(new_data$Datetime)) 
  
  ## STL decomposition and Seasonal adjustment
  q_p <- seasadj(stl(p_p, s.window="periodic"))
  
  fit_p = Arima(q_p, order=c(2,1,0), include.drift = TRUE)
  
  fcast_p <- forecast(fit_p, method = naive, h=180)
  
  r_p <- as.vector(fcast_p$mean)
  
  ## Below is the same procedure repeated for Sales 
  
  p_s <- ts(new_data$Number_Of_Sales, frequency = 12, start = min(new_data$Datetime), end = max(new_data$Datetime)) 
  
  q_s <- seasadj(stl(p_s, s.window="periodic"))
  
  fit_s = Arima(q_s, order=c(2,1,0), include.drift = TRUE)
  
  fcast_s <- forecast(fit_s, method = naive, h=180)
  
  r_s <- as.vector(fcast_s$mean)
  
  ## Now we will append the rows in fresh_data with the outcome we just derived for sales and price
  fresh_data <- rbind(fresh_data, data.frame(Id=rep(k,180), Sales=r_s, price=r_p))
}

colnames(fresh_data) <- c("ID", "Sales", "price")
