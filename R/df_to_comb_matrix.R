#' Function to convert sepcific columns of a dataframe to a named matrix with value
#'
#' @param input.df The dataframe contains needed columns
#' @param col.margin The index of inputed dataframe's column to be the colname
#' @param row.margin The index of inputed dataframe's column to be the rowname
#' @param value.margin The index of inputed dataframe's column to be the value
#' @param fill The filled in value when no value of a pair is noticed
#'
#' @return A names matrix with values
#' @note The value of fill will be filled in if no value of a pair is noticed
#'
#' @examples \dontrun{
#'   A <- data.frame(x = c('a', 'b', 'c'), y = c('A', 'B', 'c'), v = c(3:5))
#'   df_to_comb_matrix(A, col.margin = 2, row.margin = 1, value.margin = 3)
#' }
#' @export
df_to_comb_matrix <- function(input.df = data.frame(), col.margin = 1,
                              row.margin = 2, value.margin = 3, fill = NA){
  if(identical(col.margin, row.margin) | identical(col.margin, value.margin) | identical(row.margin, value.margin)){
    stop('No dupulicated number is allowed')
  }
  input.df <- input.df[,c(col.margin, row.margin, value.margin)]
  colnames(input.df) <- c('col', 'row', 'value')
  res.mat <- matrix(fill, ncol  = length(input.df[[1]]),
                    nrow = length(input.df[[2]]))
  colnames(res.mat) <- as.character(input.df[[1]])
  rownames(res.mat) <- as.character(input.df[[2]])

  for(i in 1:nrow(input.df)){
    res.mat[input.df$row[i], input.df$col[i]] <- input.df$value[i]
  }

  return(res.mat)
}
