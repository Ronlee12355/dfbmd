#' Function to convert named matrix to colname-rowname dataframe
#'
#' @description This function helps you to convert your matrix into dataframe if your matrix has colnames and rownames with value in it
#' @param input.matrix Matrix contains names and values
#' @param colnames.first Whether in the return dataframe, the colnames of matrix should be the first col
#' @param new.colnames The colnames of the return dataframe
#'
#' @return A dataframe contains colnames and rownames of the input matrix, as well as the value
#' @export
#'
#' @examples \dontrun{
#' mx <- matrix(1:6, ncol=2)
#' colnames(mx) <- c("v1", "v2")
#' rownames(mx) <- c('r1', 'r2', 'r3')
#' print(comb_matrix_to_df(mx))
#' }
comb_matrix_to_df<-function(input.matrix = matrix(), colnames.first = TRUE,
                            new.colnames = c("c", "r", "value")){
  if(!is.matrix(input.matrix)){
    stop("The input parameter should be a matrix")
  }

  cl.names <- colnames(input.matrix)
  rw.names <- rownames(input.matrix)

  if(colnames.first){
    result <- expand.grid(cl.names, rw.names)
    result[,3]<-NA
    colnames(result) <- c("c", "r", "value")

    for (i in 1:nrow(result)) {
      result$value[i] <- input.matrix[result$r[i], result$c[i]]
    }
  }else{
    result <- expand.grid(rw.names, cl.names)
    result[,3]<-NA
    colnames(result) <- c("r", "c", "value")

    for (i in 1:nrow(result)) {
      result$value[i] <- input.matrix[result$r[i], result$c[i]]
    }
  }

  colnames(result) <- new.colnames
  return(result)
}
