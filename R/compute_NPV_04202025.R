
#' compute_NPV
#'
#' Compute net present value
#' @param value ($)
#' @param time in the future that cost/value occurs (years)
#' @param discount rate
#' @return value in $

compute_NPV <- function(value, time, discount = 0.05) {
  result <- value / (1 + discount)^time
  return(result)
}
