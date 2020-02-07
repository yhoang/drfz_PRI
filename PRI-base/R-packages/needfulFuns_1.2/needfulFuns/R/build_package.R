#' Build R-packages with devtools
#' @rdname build.package
#' @aliases build.package
#'
#' @details \code{Builds Rd files and updates description and Namespace}.
#' @examples \donttest{# build.package("/Path/to/dir/packagename/R")
#' }
#' @export

build.package=function(directory){
  location=getwd()
  setwd(directory)
  devtools::document()
  setwd(location)
}