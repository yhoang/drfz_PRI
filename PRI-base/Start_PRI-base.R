#usr/bin/R
rm(list=ls())
options(shiny.error=browser)

### chose your workstation ---------------------------------
# work.station = "asus-zenbook"
# work.station = "delta"
# work.station = "drfz"
work.station = "felix"


### load paths - DO NOT TOUCH! ----------------------------
if (work.station == "asus-zenbook") {
  ### ASUS Zenbook
  PRIbase.path = "/scratch/drfz_PRI/PRI-base"
  ####### Set directory
  setwd(PRIbase.path)
  
  # Loads all needed packages.
  Lib.path = "/opt/R-3.5.1/library"
  source("www/packages.R")
  
  # set paths for app and databases
  App.path="/scratch/drfz_PRI/PRI-base/"
  DB.path="/data/databases/"
  FlowRepDb.path="www/FlowRep_Experiments.sqlite3"
  FCS.download.path="/data/databases/FCS"
  
} else if (work.station == "drfz") {
  ### DRFZ
  PRIbase.path = "Y:/AG_Baumgrass/AG-PRI/PRI-base"
  
  ####### Set directory
  setwd(PRIbase.path)
  
  # Loads all needed packages.
  Lib.path = "Y:/AG_Baumgrass/AG-PRI/R/R-3.6.1/library"
  source("www/packages.R")
  
  # set paths for app and databases
  App.path="Y:/AG_Baumgrass/AG-PRI/PRI-base/"
  DB.path="Y:/AG_Baumgrass/AG-PRI/PRI-base/DB/"
  FlowRepDb.path="Y:/AG_Baumgrass/AG-PRI/PRI-base/www/FlowRep_Experiments.sqlite3"
  FCS.download.path="Y:/AG_Baumgrass/AG-PRI/PRI-base/FCS"
  
} else if (work.station == "delta") {
  ### DELTA
  PRIbase.path = "/scratch/drfz/PRI/PRI-base"
  
  ####### Set directory
  setwd(PRIbase.path)
  
  # Loads all needed packages.
  Lib.path = "/usr/local/lib/R/site-library"
  source("www/packages.R")
  
  # set paths for app and databases
  App.path="/scratch/drfz/PRI/PRI-base/"
  DB.path="/data/databases/"
  FlowRepDb.path="www/FlowRep_Experiments.sqlite3"
  FCS.download.path="/data/databases/FCS"
  
} else if (work.station == "felix") {
  ### DELTA
  PRIbase.path = "~/Github/DRFZ-AG-Baumgrass/AG_Baumgrass/PRI/drfz_PRI/PRI-base"
  
  ####### Set directory
  setwd(PRIbase.path)
  
  # Loads all needed packages.
  Lib.path = "/home/felix/R/x86_64-pc-linux-gnu-library/3.6"
  source("www/packages.R")
  
  # set paths for app and databases
  App.path="~/Github/DRFZ-AG-Baumgrass/AG_Baumgrass/PRI/drfz_PRI/PRI-base/"
  DB.path="/data/databases/"
  FlowRepDb.path="www/FlowRep_Experiments.sqlite3"
  FCS.download.path="/data/databases/FCS"
  
}

### starts app. ---------------------------------------------------
runApp()
