#' Download several or all attachments of an experiment
#' @rdname attachment.download
#' @aliases attachment.download
#'
#' @details \code{Download attachment files of interest from Cytobank}.
#' @examples \donttest{# Download attachments from community server:
#' community="https://community.cytobank.org/cytobank"
#' attachments=attachment.list(community, 45682)
#' attachment.download(community, username, password,
#' attachment$Link, attachment$Filename, directory="path/to/store/files")
#' }
#' @export 

attachment.download=function(site,usr,pwd,link,filename,directory=getwd()){
  for(i in 1:length(link)){
    cat("\n")
    writeLines(paste("Downloading file ",i," of ",length(link)," : ",filename[i],sep=""))
    temp_file = directory_file_join(directory, paste(filename[i],".part",sep=""))
    file_name = directory_file_join(directory, filename[i])
    resp = httr::GET(paste(site,"/",link[i], sep=""),
                      httr::authenticate(usr,pwd),
                      httr::write_disk(temp_file, overwrite=TRUE),
                      httr::progress(type="down")
                    )
    
    file.rename(temp_file, file_name)
  }
  
  cat("\n")
  writeLines("Download finished successfully...")
}
