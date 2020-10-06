#!/usr/bin/R
# Author: Felix Lohrke and Yen Hoang

# ---------- # Start of PRI-ana-lite # ---------- #

# Create new environment
rm(list = ls())
Main <- new.env()
Parameters <- new.env()
Main$parent.env <- ls()
Main$version <- "PRI-ANA-LITE v0.3"

# designate work station
work.station = "delta_local"

# Pathing
if (work.station == "delta_local") {
    PRIanalite.path = file.path("","home","flohrke","PRI-Github","drfz_PRI","PRI-ana-lite","functions")
    Lib.path = "/home/flohrke/R/x86_64-pc-linux-gnu-library/3.6"
    aram.path <- file.path("","home","flohrke","PRI-Github","drfz_PRI","PRI-ana")
    #Main$db.path=file>>.path("","data","databases")
    #Main$db.name="RB_20191002_Good2018.sqlite3"
    Main$database = file.path("","data","databases","RB_20191002_Good2018.sqlite3")
}

# loading libraries
library(RSQLite, quietly=TRUE, lib.loc = Lib.path)
library(tcltk2, quietly=TRUE, lib.loc = Lib.path)
library(R.devices, quietly=TRUE, lib.loc = Lib.path)

### Load functions -------------------------------------------------------

### Source several R scripts
source.files <- list.files(path = PRIanalite.path, pattern = "\\.[Rr]$")
for (nm in 1:length(source.files)) {
 source(file.path(PRIanalite.path, source.files[nm]))
}

# Start App
# preselection for opening project selection once at start
Main$preselection = TRUE
Main$GUImain()
