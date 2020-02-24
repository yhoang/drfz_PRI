#' Get tables of sqlite3 databases
#' @rdname get.tables
#' @aliases get.tables
#'
#' @details \code{Lists all available tables of a databse}.
#' @examples \donttest{# List tables of test.sqlite3
#' tables=get.tables("test.sqlite3")
#' }
#' @export

get.tables=function(data.base){                                            
  tables=list()                                                            
  db=RSQLite::dbConnect(SQLite(),data.base)
  list.tables=RSQLite::dbListTables(db)
  writeLines(paste(data.base,"contains",length(list.tables),"tables:"))
  for(i in 1:length(list.tables)){
    writeLines(paste0(i,". ",list.tables[i]))
    table=RSQLite::dbReadTable(db, list.tables[i])
    tables[[list.tables[i]]]=table
  }
  RSQLite::dbDisconnect(db)
  tables
}