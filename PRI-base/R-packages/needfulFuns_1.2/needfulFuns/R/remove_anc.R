#' Remove symbols from a string not allowed for filenames and folders
#' @rdname remove.anc
#' @aliases remove_anc
#'
#' @details \code{Deletes \\ / : * ?  < > |}.
#' @examples \donttest{# string="?a></lex"
#' remove.anc(string)
#' }
#' @export 
remove.anc=function(string){
  symbols=c("\\","/",":","*","?","<",">","|","~","&","%","\"","{","}","#","+",",")
  # print(paste(symbols,sep=","))
  for(i in 1:length(symbols)){
    string=gsub(symbols[i]," ",string,fixed=T)
  }
  string
}