#!/usr/bin/R
# Author: Yen Hoang and Felix Lohrke
# DRFZ 2020

# ---------- # Interaction functions with the sqlite3 database # ---------- #

# database connector call
Main$connectDb <- function(fname){
  Current <- Main
  Current$conn <- dbConnect(SQLite(), dbname = fname)
  print(paste("Database opened:", fname))
}

# database disconnector call
Main$disconnectDb <- function (){
  Current <- Main
  dbDisconnect(this$conn)
  printf("w: do disconnectDb: Database closed: %s", file.path(Current$db.path, Current$db.name))
}

# retrieve all data from table from database
Main$getDFtable <- function (table){
  Current = Main
  table = dbGetQuery(Current$conn, paste("SELECT * FROM ", table))
  return(table)
}

# database metadata retrieval and preparation
Main$getMetaData <- function(connector) {
  Current = Main
  
  # get all tables in database
  Current$all.tables = dbListTables(connector)
  Current$df.num = 2
  Current$dataframes.name <- vector()
  Current$dataframes.name[1] <- "Rectangle_Data_Raw"
  Current$dataframes.name[2] <- "Rectangle_Data_Transformed"
  ## sorting of metadata from tables
  # index for metadata (looks for any of these patterns)
  idx <- grep("markerIdentity|colnameIndex|fileIdentity|fileIndex|UserHistory|Classification|equipmentInfo|fileComments|SPILL", Current$all.tables)

  for (i in idx){
  Current$df.num <- Current$df.num + 1 
    
  nam <- paste("dataframes", Current$df.num, sep="")
  df <- Current$getDFtable(Current$all.tables[i])
  assign(nam, df, envir=Main) 
  Current$dataframes.name[Current$df.num] <- Current$all.tables[i]
  }
  print("end")

}