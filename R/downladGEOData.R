#' Function to get GEO data easily
#'
#' @param GEO A character string representing a GEO object for download and parsing
#' @param destdir The destination directory for any downloads
#'
#' @description This function writes out two csv files, one contains the data matrix and the other contains the phenotype information of each sample
#' @return An object of the appropriate class (GDS, GPL, GSM, or GSE) is returned
#' @seealso \code{\link[GEOquery]{getGEO}}
#'
#' @examples
#' downloadGEOData(GEO='GSE39582')
downloadGEOData <- function(GEO='', destdir='.'){
  if(!require('GEOquery')){
    stop('Please install package GEOquery first')
  }

  library(GEOquery)
  eSet <- geoGEO(GEO, destdir = destdir, getGPl = F)

  write.csv(exprs(eSet[[1]]), file = paste0(GEO, '_expression_matrix.csv'))
  write.csv(pData(eSet[[1]]), file = paste0(GEO, '_phenotype_info.csv'))
  return(eSet)
}
