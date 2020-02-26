#' Update FlowRepository datasets
#' @rdname updateFlowRep
#' @aliases upflo
#'
#' @details \code{Special function for PRIbase, updates FlowRepository datasets
#' and returns TRUE if new experiments are available else FALSE}.
#' @examples \donttest{# updateFlowRep(T) # displays info in console
#' }
#' @export

updateFlowRep=function(verbose=F){
  db="C:/Users/Rybak/Documents/Masterthesis/App20171024/www/FlowRep_Experiments.sqlite3"
  exps=needfulFuns::get.tables(db)
  idVector=as.vector(exps$Experiment_table_short$ID)
  message("Connecting to FlowRespository")
  dataSets=FlowRepositoryR::flowRep.ls()
  idx=which(dataSets %in% idVector)
  if(!is.null(idx) && length(dataSets) > length(idVector)){
    dataSetsCut=dataSets[-idx]
    message(paste(length(dataSetsCut),"new datasets available"))
    options(warn=-1)
    options(stringsAsFactors=F)
    df=NULL
    for(i in 1:length(dataSetsCut)){
      ds=FlowRepositoryR::flowRep.get(dataSetsCut[i])
      if(length(ds@publications)>0){
        public=gsub("PMID:","",unlist(ds@publications)[1])
      } else {
        public="not applicable"
      }
      if(verbose) print(paste(i,ds@id,ds@primary.researcher,ds@name,sep=" | "))
      df=rbind(df,c(ds@id,ds@primary.researcher,ds@name,public))
    }
    options(warn=0)
    colnames(df)=c("ID","Researcher","Title","PMID")
    df=as.data.frame(df)
    database=RSQLite::dbConnect(SQLite(),db)
    RSQLite::dbWriteTable(database,"Experiment_table_short",df,append=T)
    RSQLite::dbDisconnect(database)
    message("Finished updating datasets")
    return(T)
  } else {
    message("FlowRepository database is up to date.")
    return(F)
  }
}