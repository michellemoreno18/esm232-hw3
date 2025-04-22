
#' Almond yield anomaly in response to climate conditions
#' 
#' This function computes yield anomaly (tons/acre) based on temperature and 
#' precipitation
#' 
#' @param min_temp Minimum temperature (degrees C) during the month of February
#' @param P Precipitation (mm) during the month of January
#' @return Almond yield anomaly (tons/acre)
#' function definition
almond_yield = function(min_temp, P) {
  Y = (-0.015 * min_temp) - (0.0046 * (min_temp^2)) - (0.07 * P) + 
    (0.0043 * (P^2)) + 0.28
  return(Y)  
}
