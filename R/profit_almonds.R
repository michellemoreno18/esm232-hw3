
#' Profit based on almond yield anomaly
#' @param  price ($/ton)
#' @param  yield_anomaly (tons/acre)
#' @param  year (when yield was obtained)
#' @param discount rate (default 0.05)
#' @return data frame with estimate of change in profit
compute_profit_fromyield <- function(yield_anomaly, year, price, discount = 0.05) {
  
  # generate a unique identifier or scenario number
  scen <- seq(from = 1, to = length(yield_anomaly))
  yearprofit <- data.frame(scen = scen, 
                           yield_anomaly = yield_anomaly, year = year)
  yearprofit$net <- yearprofit$yield_anomaly * price
  
  # note how discount is passed through to this function
  # remember to normalize the year to the first year
  yearprofit <- yearprofit |>
    mutate(netpre = compute_NPV_04202025(value = net, 
                                         time = year - year[1], 
                                         discount = discount))
  
  return(yearprofit)
}
