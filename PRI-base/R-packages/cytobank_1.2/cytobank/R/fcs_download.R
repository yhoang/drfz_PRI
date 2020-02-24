#' Download several or all fcs-files of an experiment
#' @rdname fcs.download
#' @aliases fcs.download
#'
#' @details \code{Download fcs-files of interest}.
#' @examples \donttest{# Download fcs-files from community server
#' community="https://community.cytobank.org/cytobank"
#' files=fcs.list(community, username, password, 45682)
#' fcs.download(community, username, password, files$ID[1], 
#' files$id, files$filename, directory="path/to/store/files")
#' }
#' @export 

fcs.download=function(site,usr,pwd,experiment_id,fcs_file_ids,filenames,directory=getwd()){
  if(length(fcs_file_ids) != length(filenames)) {
    stop("vectors differ in length",
         call.=F)
  }
  shiny::withProgress(message="",value=0,{  
    for(i in 1:length(fcs_file_ids)){
      cat("\n")
      writeLines(paste("Downloading file ",i," of ",length(fcs_file_ids)," : ",filenames[i],sep=""))
      
      temp_file = directory_file_join(directory, paste(filenames[i],".part",sep = ""))
      file_name = directory_file_join(directory, filenames[i])
      shiny::incProgress(1/length(fcs_file_ids),
                         detail=paste("Downloading file ",i," of ",length(fcs_file_ids),
                                      " : ",filenames[i],sep=""))
      resp = httr::GET(paste(site, "/experiments/", experiment_id, "/download_fcs_file?fcs_file_id=",
                             fcs_file_ids[i], sep=""),
                       httr::authenticate(usr,pwd),
                       httr::write_disk(temp_file, overwrite=T),
                       httr::progress(type="down")
      )
      file.rename(temp_file, file_name)
    }
    cat("\n")
    writeLines("download finished successfully...")
  })
}