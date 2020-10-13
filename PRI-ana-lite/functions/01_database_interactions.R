#!/usr/bin/R
# Author: Felix Lohrke and Yen Hoang
# DRFZ 2020

# ---------- # Interaction functions with the sqlite3 database # ---------- #

# retrieve main table names from database and return
Main$returnTableNames <- function(database.path) {
  # get all tablenames from database
  conn = dbConnect(SQLite(), dbname = database.path)
  all_tables = dbListTables(conn)
  dbDisconnect(conn)

  # idx to filter out non-main table names
  idx <- grep("markerIdentity|colnameIndex|fileIdentity|fileIndex|UserHistory|Classification|equipmentInfo|fileComments|SPILL", all_tables)
  return(all_tables[-idx])
}

# get all marker values from specified table and specified sample ID from table NAME_markerIdentity
# optional input of vector with markerindices ("colNUMBER,") to select only for specific markers
Main$getMarkerData <- function(database.path, table, fileID, markerindex = "all") {

  print("do: getMarkerData")

  # get Marker data from specified table and for specified ID (Sample)
  conn = dbConnect(SQLite(), dbname = database.path)
  if (markerindex == "all") {
    print(paste0("Retrieving all markervalues for project ", table, " and sample with ID ", fileID))
    data = dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE file_ID == '", fileID, "'"))
  } else {
    print(paste0("Retrieving selected markervalues for project ", table, " and sample with ID ", fileID))
    data = dbGetQuery(conn, paste0("SELECT ", markerindex , " FROM ", table, " WHERE file_ID == '", fileID, "'"))
  }
  
  dbDisconnect(conn)

  # data prep
  cofactor = 1
  data = asinh(data/cofactor)
  print("done")
  return(data)
}


# get sample names with position corresponding to their ID from table NAME_fileIdentity
Main$getSampleNames <- function(database.path, table) {

  print("do: getSampleNames")

  # get sample names which are identified through FileID in other tables
  conn = dbConnect(SQLite(), dbname = database.path)
  sample_names = dbGetQuery(conn, paste0("SELECT filename FROM ", table))
  dbDisconnect(conn)

  return(sample_names)
}

# get unique marker names from table NAME_markerIdentity
Main$getMarkerNames <- function(database.path, table, fileID) {

  print("do: getMarkerNames")
  
  # get sample names which are identified through FileID in other tables
  conn = dbConnect(SQLite(), dbname = database.path)
  marker_names = unique(dbGetQuery(conn, paste0("SELECT shortname FROM ", table)))
  dbDisconnect(conn)

  return(marker_names)
}

# get pre saved cutoffs from database
Main$getSavedCutoffs <- function(database.path, table, fileID) {

  print("do: getSavedCutoffs")
  conn = dbConnect(SQLite(), dbname = database.path)
  cutoffs = dbGetQuery(conn, paste0("SELECT file_savedCutoffs FROM ", table, " WHERE file_ID == '", fileID, "'"))
  dbDisconnect(conn)
  
  return(cutoffs)

}

# save generated cutoffs to database
# selects rows with calculated cutoffs dynamically and moves iteratively through cutoffs vector (position doesnt respond directly to row)
Main$saveCutoffs <- function(database.path, table, fileID, cutoffs, positions) {

  print("do: saveCutoffs")
  conn = dbConnect(SQLite(), dbname = database.path)
  x = 1
  for (row in positions) {
    print(paste0("UPDATE ", paste0(table,"_markerIdentity"), " SET file_savedCutoffs = ", cutoffs[x], " WHERE file_ID == ", fileID, " AND rowid == ", row))
    #dbSendQuery(conn, paste0("UPDATE ", paste0(table,"_markerIdentity"), " SET file_savedCutoffs = ", cutoffs[x], " WHERE file_ID == ", fileID, " AND rowid == ", row))
    x = x + 1
  }
  dbDisconnect(conn)

}
