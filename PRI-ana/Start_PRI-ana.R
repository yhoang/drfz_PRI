#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020

# Create new environment
rm(list = ls())
fcs = new.env()
param = new.env()
fcs$parent.env=ls()
fcs$version="v0.39c"

### Chose work.stations --------------------------------------------------
### "office"      : DRFZ Office
### "Ria"         : Ria Laptop
### "delta"       : delta office
### "lenovoz570"  : laptop Lenovo Z570
### "asus-zenbook": ASUS zenbook
### ----------------------------------------------------------------------
work.station="delta"
# work.station="lenovoz570"
# work.station="rev"
# work.station="office"
# work.station="Ria"
# work.station="Praktika"

if (work.station == "asus-zenbook") {
  PRIana.path = file.path("/","scratch","drfz_PRI","functions")
} else if (work.station == "drfz") {
  PRIana.path = file.path("Y:","AG_Baumgrass","AG-PRI","PRIanalyzer","functions")
} else if (work.station == "delta") {
  PRIana.path = file.path("","scratch","drfz","PRI","PRI-ana","functions")
} else if (work.station == "lenovoz570") {
  PRIana.path = file.path("/","scratch","drfz_PRI","functions")
} else if (work.station == "Ria") {
  PRIana.path = file.path("C:","Users","ag-baumgras","Documents","R","functions")
}

### Load functions -------------------------------------------------------
# setwd(PRIana.path)
### Source several R scripts
for (nm in list.files(path = PRIana.path, pattern = "\\.[RrSsQq]$")) {
  source(file.path(PRIana.path, nm))
}

### START APP -------------------------------------------------------------
fcs$GUImain()
