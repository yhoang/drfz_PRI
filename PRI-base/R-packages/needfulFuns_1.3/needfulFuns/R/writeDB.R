#' Write or update a table into an existing or not existing (then it will be generated)
#' database
#' @rdname writeDB
#' @aliases writeDB
#'
#' @details \code{This Function needs a .sqlite3 databasefile, title of the table to 
#' write and a dataframe to store. It is possible to append data to an existing table
#' and to overwrite it. The verbose flag is used to display work progress to console}.
#' @examples \donttest{# writeDB("FlowRepExperiments.sqlite3",
#' "Experiment_table_short",experiments,verbose=T)
#' }
#' @export

writeDB=function(file,table,df,add=F,replace=T,verbose=F){
  if(file.exists(file)){
    db=RSQLite::dbConnect(RSQLite::SQLite(),file)
    if(verbose) writeLines("Connected to existing database")
    RSQLite::dbWriteTable(db,table,df,append=add,overwrite=replace)
    RSQLite::dbDisconnect(db)
    if(verbose) {
      writeLines("Table updated successful")
      writeLines("Database disconnected")
    }
  } else {
    if(verbose) writeLines("Database file does not exist and will be generated")
    db=RSQLite::dbConnect(RSQLite::SQLite(),file)
    if(verbose) writeLines("Database connected")
    RSQLite::dbWriteTable(db,table,df,append=add,overwrite=replace)
    RSQLite::dbDisconnect(db)
    if(verbose) {
      writeLines("Table generated successful")
      writeLines("Database disconnected")
    }
  }
}