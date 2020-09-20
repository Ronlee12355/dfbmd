#' Plot multiple ROC curves in one graph
#' @description This function visualize multiple ROC curves and display values
#' of Area Under the Receiver Operating Characteristic curve (AUROC)
#' @param predictors Dataframe of your predicted possiblities of different models.
#' @param label Ideally, labels should be supplied as ordered factor(s), the lower level corresponding to the negative
#' class, the upper level to the positive class
#' @return a list containing multiple ROCR objects
#' @seealso \code{\link[ROCR]{prediction}}, \code{\link[ROCR]{performance}}
#' @importFrom magrittr %>%
#' @importFrom ROCR prediction performance
#' @export
multi_roc <- function(predictors, label){
  auc_obj <- lapply(predictors, function(x){
    pred<-prediction(x, labels = label) %>%
      performance('tpr','fpr')
  })

  auc_values<-sapply(predictors, function(x){
    pred<-prediction(x, labels = label) %>%
      performance(measure = 'auc')
    return(pred@y.values[[1]])
  })
  cat(paste0(auc_values, '\n'))
  return(auc_obj)
}
