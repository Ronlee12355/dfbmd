#' Using random walk algorithm to predict drug-drug interaction with restart
#'
#' @param similarity_matrix Similarity matrix of N drugs
#' @param adjacent_matrix Adjacent matrix of label (1 = known interaction, 0 = non-interaction)
#' @param probability The probability of jumping back ti initial point
#'
#' @return NxN Matrix of probability
#' @references Zhang el at (2017). Predicting potential drug-drug interactions by integrating chemical, biological, phenotypic and network data. BMC Bioinformatics 18:18

random_walk <- function(similarity_matrix = matrix(), adjacent_matrix = matrix(),
                        probability = 0.85){
  D <- diag(apply(similarity_matrix, 2, sum))
  W <- solve(D) %*% similarity_matrix
  I <- diag(1, nrow = nrow(similarity_matrix))

  Y <- (1-probability) * solve((I - probability * W)) %*% adjacent_matrix
  return(Y)
}
