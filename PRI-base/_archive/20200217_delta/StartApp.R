# add library Path
.libPaths(c(.libPaths(),"Y:/AG_Baumgrass/Alexander.Rybak/win-library/3.4"))
options(shiny.error=browser)
# set working directory
setwd("Y:/AG_Baumgrass/AG-PRI/PRIbase")
# set paths for app and databases
App.path="Y:/AG_Baumgrass/AG-PRI/PRIbase/App20170929"
DB.path="Y:/AG_Baumgrass/Alexander.Rybak/win-library/"
# Loads all needed packages.
source("App20170929/www/packages.R")
# Starts App.
runApp("App20170929/")
