
#' Compute profit from almond yield anomaly given climate conditions
#'
#' Profit based on almond yield anomaly, 
#' price set at $3,520/ton (based on $1.76/lb from 
#' California Agricultural Statistics Review 2021-2022)
#'
#' @param min_temp Minimum temperature in February (°C)
#' @param P Precipitation in January (mm)
#' @param year Year(s) of yield observation
#' @return Data frame with estimate of change in profit
compute_profit_fromyield <- function(min_temp, P, year) {
  
  # fixed almond price in $/ton (based on $1.76/lb)
  price <- 3520
  
  # compute yield anomaly directly using formula
  yield_anomaly <- (-0.015 * min_temp) - 
    (0.0046 * (min_temp^2)) - 
    (0.07 * P) + 
    (0.0043 * (P^2)) + 
    0.28
  
  # create the profit data frame without scenario number
  yearprofit <- data.frame(
    year = year,
    min_temp = min_temp,
    P = P,
    yield_anomaly = yield_anomaly
  )
  
  # calculate net profit
  yearprofit$net <- yearprofit$yield_anomaly * price
  
  return(yearprofit)
}
