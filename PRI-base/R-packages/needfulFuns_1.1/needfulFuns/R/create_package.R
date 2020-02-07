#' Create R-packages with devtools
#' @rdname create.package
#' @aliases create.package
#'
#' @details \code{Creates all necessary files and directories for a package}.
#' @examples \donttest{# create.package("/Path/to/dir/packagename")
#' }
#' @export

create.package=function(directory){
  devtools::create(directory)
}