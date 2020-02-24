#' Convert a parsed list -> Cytobank dataframe
#' @rdname cyto_dataframe
#' @aliases cyto_dataframe
#'
#' @details \code{cyto_dataframe}.
#' @examples \donttest{# Convert parsed list
#' df=cyto_dataframe(parsed.list)
#' }
#' @export
 
cyto_dataframe=function(parsed_list){
  return(
    setNames(
      data.frame(
        # transpose of the matrix
        t(matrix(unlist(rbind(
          # lapply to set NULL -> NA
          lapply(
            lapply(parsed_list, rbind),
            function(x) ifelse(x == "NULL", NA, x))), recursive=FALSE),
          ncol=length(parsed_list))), stringsAsFactors=FALSE),
      labels(parsed_list[[1]]))
  )
}
