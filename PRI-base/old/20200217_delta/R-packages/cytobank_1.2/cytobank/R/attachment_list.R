#' List attachments of an experiment
#' @rdname attachment.list
#' @aliases attachment.list
#'
#' @details \code{List attachment files of an experiment from Cytobank}.
#' @examples \donttest{# List attachments of an experiment from community server:
#' community="https://community.cytobank.org/cytobank"
#' attachments=attachment.list(community, username, password, 41956)
#' }
#' @export 

attachment.list=function(site,usr,pwd,experiment_id){
  resp=GET(paste(site,"/experiments/",experiment_id,sep=""),
           authenticate(usr,pwd))
  
  pg=xml2::read_html(resp)
  links=rvest::html_attr(rvest::html_nodes(pg, "a"), "href")
  x=grep(paste("/cytobank/experiments/",experiment_id,"/attachments/\\d+/download$",sep=""),
         links,value=T)
  x=gsub("/cytobank/","",x)
  node.text=rvest::html_nodes(pg,xpath = '//div[@class="filename"]/text()')
  node.title=rvest::html_text(node.text)
  node.title=gsub("\n","",node.title)
  filename=node.title[which(node.title !="")]
  
  if(length(node.title) == 0 || length(x) == 0){
    writeLines(paste("No attachments available for experiment:",experiment_id))
    return(data.frame(Link="Not available",Filename="-"))
  } else {
      return(result=data.frame(Link=x,Filename=filename))
    }
}