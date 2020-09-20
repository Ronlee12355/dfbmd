#' Linear regression using ordinary least square
#'
#' @param y Response variable
#' @param x Independent variables
#'
#' @return A dataframe contains coefficients
#' @export
#' @seealso \code{\link{lm}}
#' @examples
#' library(tidyverse)
#' ols(y=diamonds$price,x=diamonds %>% select(-price) %>% select_if(is.numeric)) %>% print()
ols <- function(y, x){
  x <- as.matrix(x)
  x <- cbind(intercept=1,x)
  result <- solve(t(x) %*% x) %*% t(x) %*% y
  result <- as.data.frame(result)
  result[["V2"]] <- rownames(result)
  colnames(result) <- c("coefficients", "variables")
  return(result)
}
