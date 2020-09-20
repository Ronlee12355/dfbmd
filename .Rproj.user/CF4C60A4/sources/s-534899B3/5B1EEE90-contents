#' Make the element of a matrix's diagonal line into zero
#'
#' @param input A matrix has the same size of columns and rows
#'
#' @return A matrix with all element of diagonal line  zero
#' @export
#'
#' @examples \dontrun{
#' A <- matrix(1:100, nrow=10)
#' A <- diagonal_zero(A)
#' }
diagonal_zero <- function(input = matrix()){
  if(nrow(input) == ncol(input)){
    stop("The input matrix should be in the size size of row and column")
  }

  return(input - diag(diag(input)))
}
