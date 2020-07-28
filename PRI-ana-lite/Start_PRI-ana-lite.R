#!/usr/bin/R
# Author: Felix Lohrke

# Create new environment
rm(list = ls())
pal <- new.env()
param <- new.env()
pal$parent.env <- ls()
pal$version <- "v0.1"

# designate work station
work.station = "delta_local"

# Pathing
if (work.station == "delta_local") {
    PRIanalite.path = file.path("","home","flohrke","PRI-Github","drfz_PRI","PRI-ana-lite","functions")
    Lib.path = "/home/flohrke/R/x86_64-pc-linux-gnu-library/3.6"

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

# Start App
pal$GUImain()