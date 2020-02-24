#' File path helper function -Ensure path ends in "directory separator"
#' @rdname directory_file_join
#' @aliases directory_file_join
#'
#' @details \code{Join a file to a path}.
#' @examples \donttest{directory_file_join(directory_path, file_name)
#' }
#' @export 

directory_file_join=function(directory_path, file_name){
  if (regexpr("[^/\\]$", directory_path) != -1){
    directory_path=file.path(directory_path, file_name)
  } else {
    directory_path=paste(directory_path, file_name, sep="")
  }
}