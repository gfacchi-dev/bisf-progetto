library(tseries)
library(zoo)
library(forecast)

generateForecastArima <- function(stockName, n, m, l) {
  dataset.z <- get.hist.quote( instrument=stockName, start="2007-01-01", quote="AdjClose", provider="yahoo", origin="1970-01-01", compression="month")
  dataset.z <- na.omit(dataset.z)
  index(dataset.z) <- as.yearmon(index(dataset.z))
  colnames(dataset.z) <- stockName
  dataset.z <- aggregate(dataset.z, index(dataset.z), tail, 1)
  index(dataset.z) <- as.yearmon(index(dataset.z))
  head(dataset.z)
  returns <- diff( log(dataset.z[,1]) )
  
  fitRet <- stl(returns[,1], s.window="period")
  head(fitRet$time.series,10)
  plot(fitRet, main=paste("Seasonal Decomposition of, ", stockName ," CC Returns Time Series"))
  
  
  returnsTrain <- returns[(length(returns) - (n + m + l)):(length(returns) - (m + l + 1))]
  returnsTest <- returns[(length(returns)- (m + l)):(length(returns) - (l + 1))]  
  
  for(ar in 1:10){
    for(ma in 1:10){
      try (fit <- arima(returnsTrain, order = c(ar, 0, ma)))
      
      if(!is.null(fit)){
        arma.predictions <- predict(fit, n.ahead = ((length(returns) - l) - (length(returns)-(m + l + 1))))$pred
        arma.forecast <- forecast(fit, h = length(returnsTest),level = c(95, 80))
        plot(arma.forecast, main = paste0("ARMA (ar=",ar,", ma=",ma,") forecasts for ", stockName, " returns"), xlab = "Data", ylab = "Ritorno (%)")
        grid()
        lines(returnsTest)
        rmseColl <<- rbind(rmseColl, c(stockName, accuracy(arma.predictions, returnsTest)[2], ar, 0, ma))
        lines(returnsTest)
      }
    }
  }
}