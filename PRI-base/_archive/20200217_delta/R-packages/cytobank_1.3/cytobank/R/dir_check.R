#' Check if a directory is existing. If not generate it.
#' @rdname dir.check
#' @aliases dir.check
#'
#' @details \code{Generates a directory if it is not existing}.
#' @examples \donttest{# Generate diroctory, returns path:
#' path=dir.check("path/to/store/files")
#' }
#' @export

dir.check=function(path){
  if(dir.exists(path)==T){
    writeLines(paste("Directory",path,"already exists."))
  } else {
    dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0755")
    writeLines(paste("Directory",path,"does not exist and will be generated."))
  }
  path
}