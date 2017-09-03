#' @title <dependencies>
#'
#' @description <install the required dependencies located out of CRAN repository>
#' @param
#' @export


dependencies<-function() {

  library(devtools)
  install_github("cran/fPortfolio")
  install_github("cran/PerformanceAnalytics")
}
