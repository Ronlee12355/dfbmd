#' Standard Error
#' @description This function computes the standard error of the values in x. If na.rm is TRUE then missing values are
#' removed before computation proceeds.
#' @param x a numeric vector or an R object which is coercible to one by as.double(x)
#' @param na.rm logical. Should missing values be removed?
#'
#' @return value of standard error
#' @seealso \code{\link{sd}}
#' @examples standard_error(1:7)
standard_error <- function(x, na.rm = FALSE){
  x <- as.double(x)
  return(var(x, na.rm = na.rm)/length(na.omit(x)))
}
