
#' Profit based on almond yield anomaly, 
#' price set at $3,520/ton (based on $1.76/lb from 
#' California Agricultural Statistics Review 2021-2022)
#'
#' @param min_temp Minimum temperature in February (°C)
#' @param P Precipitation in January (mm)
#' @param year Year(s) of yield observation
#' @param discount Discount rate (default = 0.05)
#' @return Data frame with estimate of change in profit
compute_profit_fromyield <- function(min_temp, P, year, discount = 0.05) {
  
  # fixed almond price in $/ton (based on $1.76/lb)
  price <- 3520
  
  # compute yield anomaly using almond_yield function
  yield_anomaly <- almond_yield(min_temp, P)
  
  # generate a unique identifier or scenario number
  scen <- seq_along(yield_anomaly)
  
  # create the profit data frame
  yearprofit <- data.frame(
    scen = scen,
    year = year,
    min_temp = min_temp,
    P = P,
    yield_anomaly = yield_anomaly
  )
  
  # calculate net profit
  yearprofit$net <- yearprofit$yield_anomaly * price
  
  # compute discounted net profit
  yearprofit <- yearprofit |>
    mutate(netpre = compute_NPV_04202025(
      value = net,
      time = year - min(year),  # normalize to first year
      discount = discount
    ))
  
  return(yearprofit)
}
