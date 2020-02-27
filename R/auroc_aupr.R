#' Function to fetch score of AUROC and AUPR
#' @description This function computes the value of AUROC and AUPR at the same time
#' @param score A numeric dataset of predicted scores, a vector
#' @param labels A numeric, character, logical, or factor dataset of observed labels
#' @param mode A string that specifies the types of evaluation measures that the evalmod function calculates, roc, pr, both.
#' the default is both.
#'
#' @return value of computed scores of roc or prc.
#' @seealso \code{\link{[precrec]evalmod}}
#' @examples
#' x <- c(0.1, 0.5, 0.6, 0.2)
#' y <- factor(c('yes','no','yes','no'))
#' auroc_aupr(scores = x, labels = y)
auroc_aupr <- function(scores, labels, mode = 'both'){
  if(!(mode %in% c('roc','prc','both'))){
    stop('Iinvaluable parameter for mode')
  }

  x <- precrec::evalmod(scores = scores, labels = labels)
  score <- attr(x, 'auc')$aucs
  if(mode == 'both'){
    names(score) <- c('auroc', 'aupr')
  }else if(mode == 'roc'){
    score <- score[1]
    names(score) <- 'roc'
  }else{
    score <- score[2]
    names(score) <- 'prc'
  }

  return(score)
}
