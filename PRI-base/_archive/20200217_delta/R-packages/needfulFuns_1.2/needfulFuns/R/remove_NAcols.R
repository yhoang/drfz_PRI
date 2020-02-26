#' Delete columns of a dataframe containing NAs
#' @rdname remove.NAcols
#' @aliases remove.NAcols
#'
#' @details \code{Remove all columns from a dataframe containing NAs (na.omit excludes rows)}.
#' @examples \donttest{# dataframe=remove.NAcols(dataframe)
#' }
#' @export

remove.NAcols=function(dframe){
  dframe=dframe[,!apply(dframe,2,function(x) any(is.na(x)))]
}