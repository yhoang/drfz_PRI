#!/usr/bin/R
# Author: Felix Lohrke

# ---------- # Start of PRI-ana-lite # ---------- #

# Create new environment
rm(list = ls())
Main <- new.env()
Parameters <- new.env()
Main$parent.env <- ls()
Main$version <- "v0.1"

# designate work station
work.station = "delta_local"

# Pathing
if (work.station == "delta_local") {
    PRIanalite.path = file.path("","home","flohrke","PRI-Github","drfz_PRI","PRI-ana-lite","functions")
    Lib.path = "/home/flohrke/R/x86_64-pc-linux-gnu-library/3.6"
    aram.path <- file.path("","home","flohrke","PRI-Github","drfz_PRI","PRI-ana")
    Main$db.path=file.path("","data","databases")
    Main$db.name="RB_20191002_Good2018.sqlite3"
}

# loading libraries
library(RSQLite, quietly=TRUE, lib.loc = Lib.path)
library(tcltk2, quietly=TRUE, lib.loc = Lib.path)
library(R.devices, quietly=TRUE, lib.loc = Lib.path)

### Load functions -------------------------------------------------------
# setwd(PRIana.path)
### Source several R scripts
source.files <- list.files(path = PRIanalite.path, pattern = "\\.[Rr]$")
for (nm in 1:length(source.files)) {
 source(file.path(PRIanalite.path, source.files[nm]))
}

# connect to database
if (length(strsplit(Main$db.name, "")[[1]]) >  0){

    # database connection is established at Main$conn
    Main$connectDb(file.path(Main$db.path, Main$db.name))
    
} else {
    file <- tclvalue(tkgetOpenFile(initialdir=fcs$db.path, defaultextension="sqlite3"))
    Main$connectDb(file)
    Main$db.name <- file
}

# retrieve MetaData 
Main$getMetaData(Main$conn)

# Start App
# preselection for opening project selection once at start
Main$preselection = TRUE
Main$GUImain()
