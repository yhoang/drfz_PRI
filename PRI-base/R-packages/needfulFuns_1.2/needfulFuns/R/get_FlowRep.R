#' Download one or more Datasets from FlowRepository
#' @rdname getFlowRep
#' @aliases getFlo
#'
#' @details \code{Function needs one or more Experiment IDs as a vector or character and 
#' a directory path as character to save downloaded files}.
#' @examples \donttest{# getFlowRep("FR-FCM-ZY69","/Path/to/store/files")
#' }
#' @export

getFlowRep=function(expIDs,directory=getwd(),verbose=F){ # wrapper function to download 
  options(warn=-1)                                       # one or multiple experiments                                       
  location=getwd()
  setwd(directory)
  message(paste("Destination:",directory))
  for(i in 1:length(expIDs)){
    message(paste("Downloading dataset:",expIDs[i]))
    ds=FlowRepositoryR::flowRep.get(expIDs[i])
    if(verbose) FlowRepositoryR::summary(ds)
    shiny::incProgress(1/length(expIDs),
                       detail=paste("dataset:",
                                    expIDs[i])
                       )
    ds=FlowRepositoryR::download(ds,show.progress=T)
  }
  setwd(location)
  options(warn=0)
  message("DONE")
}