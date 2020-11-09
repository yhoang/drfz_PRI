#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020

# Create new environment
rm(list = ls())
fcs <- new.env()
param <- new.env()
fcs$parent.env <- ls()
fcs$version <- "v0.40a"

### Chose work.stations --------------------------------------------------
### "drfz"          : DRFZ Office
### "Ria"           : Ria Laptop
### "delta"         : delta office
### "lenovoz570"    : Lenovo Z570
### "asus-zenbook"  : ASUS zenbook
### "asus-vividbook": ASUS Vividbook
### ----------------------------------------------------------------------

# work.station <- "drfz"
# work.station <- "lenovoz570"
# work.station <- "rev"
# work.station <- "Ria"
# work.station <- "Praktika"
# work.station <- "asus-zenbook"
#work.station <- "felix"
#work.station <- "asus-vividbook"
work.station <- "delta_local"

if (work.station == "asus-zenbook") {
  PRIana.path <- file.path("", "scratch", "drfz_PRI", "PRI-ana", "functions") 
} else if (work.station == "drfz") {
  PRIana.path <- file.path("Y:", "AG_Baumgrass", "AG-PRI", "github_yhoang", "drfz_PRI", "PRI-ana", "functions")
} else if (work.station == "delta") {
  PRIana.path <- file.path("", "scratch", "drfz", "PRI", "PRI-ana", "functions")
} else if (work.station == "lenovoz570") {
  PRIana.path <- file.path("", "scratch", "drfz_PRI", "functions")
} else if (work.station == "Ria") {
  PRIana.path <- file.path("C:", "Users", "ag-baumgras", "Documents", "R", "functions")
} else if (work.station == "asus-vividbook") {
  PRIana.path <- file.path("D:", "drfz_PRI", "PRI-ana", "functions")
} else if (work.station == "felix") {
 PRIana.path <- file.path("~","Github", "DRFZ-AG-Baumgrass", "AG_Baumgrass","PRI", "drfz_PRI", "PRI-ana", "functions")
} else if (work.station == "delta_local") {
 PRIana.path <- file.path("~", "PRI-Github", "drfz_PRI", "PRI-ana", "functions")
}


### Load functions -------------------------------------------------------
# setwd(PRIana.path)
### Source several R scripts
source.files <- list.files(path = PRIana.path, pattern = "\\.[Rr]$")
# for (nm in 1:5) {
for (nm in 1:length(source.files)) {
 source(file.path(PRIana.path, source.files[nm]))
}

### START APP -------------------------------------------------------------
fcs$GUImain()

