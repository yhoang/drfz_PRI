#' Get all public experiments from community or premium cytobank, returns dataframe
#' 
#' @rdname get.experiments
#' @aliases get.experiments
#'
#' @details \code{get.experiments} .
#' @examples \donttest{# Download experiment list
#' community="https://community.cytobank.org/cytobank"
#' experiments=get.experiments(community, username, password)
#' }
#' @export  

get.experiments=function(site,usr,pwd){
  resp=httr::GET(paste(site,"/experiments",sep=""),
                 httr::authenticate(usr, pwd),
                 httr::progress(type="down")
  )
  cat("\n")
  pg=xml2::read_html(resp)
  node.text=rvest::html_nodes(pg,xpath = '//a[@href="/cytobank/experiments?filter=all"]/text()') 
  node.title=rvest::html_text(node.text)
  matches=regmatches(node.title, gregexpr("[[:digit:]]+", node.text))
  no.exp=as.numeric(unlist(matches))
  sel.name=".pr"
  links=rvest::html_attr(rvest::html_nodes(pg, "a"), "href")
  titles=rvest::html_attr(rvest::html_nodes(pg, "a"), "title")
  prires=rvest::html_nodes(pg,sel.name) %>% rvest::html_text()
  x=grep("/cytobank/experiments/\\d+$",links,value=T)
  if(length(x) > no.exp){
    x=x[(length(x)-no.exp+1):length(x)] # problem with already viewed experiments, so they have to be deleted
  }
  idx=which(!is.na(titles))
  titles=titles[idx]
  
  exp_id=gsub("/cytobank/experiments/","", x)
  
  if(length(exp_id) != length(titles)){
    stop("vectors differ in length",
         call.=F)
  }
  options(stringsAsFactors=F)
  experiments=data.frame(ID=type.convert(exp_id),Researcher=prires,Title=titles)
  experiments
}
