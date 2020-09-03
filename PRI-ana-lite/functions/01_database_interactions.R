#!/usr/bin/R
# Author: Yen Hoang and Felix Lohrke
# DRFZ 2020

# ---------- # Interaction functions with the sqlite3 database # ---------- #

# retrieve main table names from database and return
Main$returnTablenames <- function(database.path) {
  # get all tablenames from database
  conn = dbConnect(SQLite(), dbname = database.path)
  all_tables = dbListTables(conn)

  # idx to filter out non-main table names
  idx <- grep("markerIdentity|colnameIndex|fileIdentity|fileIndex|UserHistory|Classification|equipmentInfo|fileComments|SPILL", all_tables)
  return(all_tables[-idx])
}

# get marker information from specified table and specified sample ID
Main$getMarkerData <- function(database.path, table, fileID) {

  # get Marker data from specified table and for specified ID (Sample)
  conn = dbConnect(SQLite(), dbname = database.path)
  data = dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE file_ID == '", fileID, "'"))

  return(data)
}


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

  # dont know what these are for -> maybe saving after own rectangle?
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

  # contains absolute num of different subsets/projects
  Current$all.tables = Current$all.tables[-idx]
  Current$current.table = Current$all.tables[1]

  # get indices for filenames and table
  idx.table <- grep(paste0("^", Current$all.tables[1], "_"), Current$dataframes.name)
  idx.file <- grep("fileIdentity|fileIndex", Current$dataframes.name)
  
  # assigning different names to fileid/markerid depending on retrieved data
  if (grepl("Identity", Current$dataframes.name[idx.file[1]])) Current$fileid_name <- "_fileIdentity"
  else Current$fileid_name <- "_fileIndex"
  idx.stain <- grep("markerIdentity|colnameIndex", Current$dataframes.name)[1]
  if (grepl("Identity", Current$dataframes.name[idx.stain])) Current$markerid_name <- "_markerIdentity"
  else Current$markerid_name <- "_colnameIndex"

  print("Metadata prepared")
}