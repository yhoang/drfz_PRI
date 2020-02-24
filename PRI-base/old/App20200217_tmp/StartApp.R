options(shiny.error=browser)

####### Start im DRFZ
setwd("/scratch/drfz_PRI/PRI-base")

# Loads all needed packages.
source("App20200217/www/packages.R")

# set paths for app and databases
App.path="/scratch/drfz_PRI/PRI-base/App20200217/"
DB.path="/data/databases/"
FlowRepDb.path="App20200217/www/FlowRep_Experiments.sqlite3"
FCS.download.path="/data/databases/FCS"

# Loads all needed packages.
source("App20200217/www/packages.R")

# Starts App.
runApp("App20200217/")
