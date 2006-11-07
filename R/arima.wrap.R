arima.wrap <- function(formula, order, x, xreg=NULL, data, M, ...){
  arima(x=x, xreg=xreg, order=order, ...)
} 
