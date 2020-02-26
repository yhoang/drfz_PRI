#' Get FCS-Files list for an experiment
#' @rdname fcs.list
#' @aliases fcs.list
#'
#' @details \code{fcs.list}.
#' @examples \donttest{# Get fcs-files from cytobank community server
#' community="https://community.cytobank.org/cytobank"
#' fcs.files=fcs.list(community, username, password, 60068)
#' }
#' @export
 
fcs.list=function(site,usr,pwd,experiment_id){
  resp=httr::GET(paste(site,"/experiments/",experiment_id,"/fcs_files",sep=""),
           httr::authenticate(usr, pwd)
  )
  parsed=jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector=FALSE)
  df = cyto_dataframe(parsed)
  df
}