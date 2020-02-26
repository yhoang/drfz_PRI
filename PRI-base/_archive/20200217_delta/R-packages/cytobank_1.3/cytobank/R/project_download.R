#' Download all or several experiments of a Cytobank project
#' @rdname project.download
#' @aliases project.download
#'
#' @details \code{Download a whole project from Cytobank}.
#' @examples \donttest{# Download project Spitzer:
#' community="https://community.cytobank.org/cytobank"
#' experiments=get.experiments(community)
#' ### Filter experiments by keyword
#' spitzer=search.key(experiments,"Spitzer")
#' project.download(community, username, password, spitzer,"path/to/download/file")
#' }
#' @export

project.download=function(site,usr,pwd,df,directory=getwd()){
  dir.check(directory)
  for(i in 1:length(df$ID)){
    cat("\n")
    writeLines(paste("Starting download of experiment ",df$ID[i],": ",df$Title[i],sep=""))
    subdir=dir.check(paste(directory,"/",df$Title[i],sep=""))
    fcs.files=fcs.list(site,usr,pwd,df$ID[i])
    fcs.download(site,usr,pwd,df$ID[i],fcs.files$id,fcs.files$filename,subdir)
  }
  cat("\n")
  writeLines("Project-download finished successfully...")
}