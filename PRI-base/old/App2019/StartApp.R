options(shiny.error=browser)

####### Start im DRFZ
setwd("/scratch/drfz_PRI/PRI-base")

# Loads all needed packages.
source("www/packages.R")

App.path="/scratch/drfz_PRI/PRI-base/"
DB.path="/data/databases/"

# Loads all needed packages.
source("www/packages.R")

# Starts App.
runApp()

