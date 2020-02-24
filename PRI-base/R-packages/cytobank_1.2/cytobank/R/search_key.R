#' Search keyword in experiment list
#' @rdname search.key
#' @aliases search.key
#'
#' @details \code{search a keyword}.
#' @examples \donttest{# Search for Spitzer in experiment list
#' result=search.key(experiments, "Spitzer")
#' }
#' @export 

search.key=function(df, keyword="Spitzer"){
  found=grep(keyword,df$Title)
  if(length(found) == 0){
    print(paste(keyword,"keyword not found"))
  } else {
    return(df[found,])
  }
}