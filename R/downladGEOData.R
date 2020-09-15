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
#' \dontrun{
#'  downloadGEOData(GEO='GSE39582')
#' }
#' @importFrom utils write.csv
downloadGEOData <- function(GEO='', destdir='.'){
  if(!requireNamespace('GEOquery')){
    stop('Please install package GEOquery')
  }

  if(!requireNamespace('Biobase')){
    stop('Please install package Biobase')
  }

  eSet <- GEOquery::geoGEO(GEO, destdir = destdir, getGPl = F)

  write.csv(Biobase::exprs(eSet[[1]]), file = paste0(GEO, '_expression_matrix.csv'))
  write.csv(Biobase::pData(eSet[[1]]), file = paste0(GEO, '_phenotype_info.csv'))
  return(eSet)
}
