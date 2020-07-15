#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020

### load public libraries - DO NOT TOUCH!

### set paths and load libraries - DO NOT TOUCH! -----------------------------
### set paths
if (work.station  ==  "asus-zenbook") {
  ### ASUS Zenbook
  Lib.path <- "/opt/R-3.5.1/library"

  setwd(file.path("", "scratch", "drfz_PRI", "PRI-ana"))
  fcs$db.path <- file.path("", "data", "databases")
  fcs$db.name <- "EM_20200210_EME002_WTCD4.sqlite3"

  fcs$table.dir <- "/scratch/drfz_PRI/PRI-ana/"
  fcs$working <- TRUE
  #fcs$working <- FALSE

  ### set template location
  fcs$png.file <- file.path("tcl", "tmp.png")
  fcs$template.file <- file.path("tcl", "template.png")
} else if (work.station == "asus-vividbook") {
  Lib.path <- file.path("C:", "Program\ Files", "R", "R-3.5.1", "library")

  setwd(file.path("D:", "drfz_PRI", "PRI-ana"))
  fcs$db.path <- file.path("D:", "DB")
  fcs$db.name <- "SGJP_20180524_NZBxWIL21+Cytokines.sqlite3"
  fcs$working <- FALSE
  fcs$working <- TRUE

  ### set template location
  fcs$png.file <- file.path("tcl", "tmp.png")
  fcs$template.file <- file.path("tcl", "template.png")
} else if (work.station == "drfz") {
  Lib.path <- "Y:/AG_Baumgrass/AG-PRI/R/R-3.6.1/library"

  setwd(file.path("Y:", "AG_Baumgrass", "AG-PRI", "PRIanalyzer"))
  fcs$db.path <- file.path("Y:", "AG_Baumgrass", "AG-PRI", "DB")
  fcs$db.name <- ""
  fcs$working <- FALSE
  #fcs$working <- TRUE

  ### set template location
  fcs$png.file <- file.path("tcl", "tmp.png")
  fcs$template.file <- file.path("tcl", "template.png")
} else if (work.station == "lenovoz570") {
  setwd(file.path("/", "scratch", "drfz_PRI"))
  fcs$db.path <- file.path("", "data", "databases")
  fcs$db.name <- ""
  fcs$working <- FALSE

  ### set template location
  fcs$png.file <- file.path("tcl", "tmp.png")
  fcs$template.file <- file.path("tcl", "template.png")
} else if (work.station == "Ria") {
  setwd("C:/Users/ag-baumgras/Documents/R")
  fcs$db.path <- file.path("data")
  fcs$db.name <- ""
  fcs$working <- FALSE
  ### set template location
  fcs$png.file <- file.path("tcl", "tmp.png")
  fcs$template.file <- file.path("tcl", "template.png")
} else if (work.station == "delta") {
  Lib.path <- "/usr/local/lib/R/site-library"

  setwd(file.path("", "scratch", "drfz", "PRI", "PRI-ana"))
  fcs$db.path <- file.path("", "data", "databases")
  # fcs$db.name <- "RB_20191002_Good2018.sqlite3"
  fcs$db.name <- "YH_20190524_Tordesillas2016.sqlite3"

  fcs$table.dir <- "/scratch/drfz/Spitzer2017/glmnet_bio_keep/"
  fcs$working <- TRUE
  #fcs$working <- FALSE

  ### set template location
  fcs$png.file <- file.path("tcl", "tmp.png")
  fcs$template.file <- file.path("tcl", "template.png")
} else if (work.station == "rev") {
  setwd(file.path("", "scratch", "drfz", "R"))
  fcs$db.path <- file.path("", "scratch", "db")
  fcs$db.name <- "SG_20161030_HumanCytokines_CD4mem.sqlite3"
  fcs$working <- TRUE
  density <- TRUE
}
library(RSQLite, quietly=TRUE, lib.loc = Lib.path)
library(tcltk2, quietly=TRUE, lib.loc = Lib.path)
library(R.devices, quietly=TRUE, lib.loc = Lib.path)

### load private library
source(file.path("tcl", "libbwidget.r"))


### global variables - DO NOT TOUCH! --------------------------------------------
options(warn=-1, scipen=999)
fcs$dataframes.name <- vector()
fcs$dataframes1 <- fcs$dataframes2 <- as.data.frame(matrix(NA, 1, 1))
fcs$temptable.name <- vector()
fcs$df.num <- 2
fcs$plot.num <- 0
fcs$plot.windows <- vector()
fcs$plotter.di.graph <- 0
fcs$plotter.di.num <- 0
fcs$plotter.tri.num <- 0
fcs$plotter.quad.num <- 0
fcs$distepy <- 0
fcs$origin.ncells <- 0
fcs$ncell.sel <- 0
fcs$ncell.perc <- 0
fcs$cbVal <- list()
fcs$cbcutoffperc <- list()
fcs$saved.cutoffs <- list()
fcs$saved.checks <- list()
fcs$images <- list()
fcs$plot.attr <- list()

fcs$asinh <- list(
  scale = c(asinh(-1000), asinh(-100), asinh(-10), asinh(0), asinh(10), asinh(100), asinh(1000), asinh(10000), asinh(100000), asinh(1000000), asinh(10000000), asinh(100000000)),
  label = c("1e-3", "1e-2", "1e-1", "0", "1e1", "1e2", "1e3", "1e4", "1e5", "1e6", "1e7", "1e8"),
  step = 2,
  scipen = -1,   # when to display numbers as 1e5 rather than 10000
  range = c("0", "12", "0", "12"), # current x and y range
  binsize = 0.2,
  binsizes = as.character(c(0.1, 0.2, 0.4, 0.5, 0.6)),
  mincount = 10
)

fcs$biex  <-  list(
  scale = c(0, 1000, 2000, 3000, 4000, 5000, 6000),
  label = c(0, 1000, 2000, 3000, 4000, 5000, 6000),
  step = 500,
  scipen = 999,   # when to display numbers as 1e5 rather than 10000
  range = c("0", "4000", "0", "4000"), # current x and y range
  binsize = 50,
  binsizes = as.character(c(25, 50, 100, 200, 400)),
  mincount = 5
)

fcs$coords.info <- vector()
fcs$plotWindowExists <- FALSE
fcs$densityWindowExists <- FALSE
fcs$temp.data <- FALSE
fcs$ttprojectsopen <- FALSE
fcs$OverviewGate <- FALSE
fcs$diploTGate <- FALSE
fcs$current.xaxis <- c(0, 14)
fcs$current.project <- ""
fcs$current.trans <- "asinh"
fcs$current.projectnum <- fcs$selected.projectnum <- 1
fcs$current.filenum <- fcs$selected.filenum <- 1
fcs$current.tab <- "0"
fcs$current.var1.diploT <- ""
fcs$current.diploT.x <- 2
fcs$current.cofactor <- "1"
fcs$temp.num <- 0
fcs$min.counts <- as.character(c(2, 5, 10, 15, 20, 25, 50))
fcs$min.counts.diploT <- as.character(c(10, 20, 50, 100, 200, 300, 500))
fcs$binSizes <- as.character(c(0.1, 0.2, 0.4, 0.5, 0.6))
fcs$binSizes.biex <- as.character(c(25, 50, 100, 200))
fcs$preproc.types <- c("Choose option", "Normalize selected files", "Calculating cutoffs")
fcs$trim.num <- 0.0001
fcs$plot.percentage <- TRUE

### old
#fcs$col.rainbow <- c("#021BC0", "#021BC0", "#0092FFFF", "#00FFFFFF", "#0DE378", "#4dff4d", "#B6FF00FF", "#FFFF00", "#FFDB00FF", "#FF6D00FF", "#FF0000FF", "#FF0000FF")
### new
fcs$col.rainbow <- c("#001AC0", "#001AC0", "#00A7D0", "#00F0EC", "#00F884", "#81FF42", "#D1FF00", "#FFFF00", "#FFDB00FF", "#FF6D00FF", "#FF0000FF", "#FF0000FF")
#fcs$col.rainbow.pale <- c("#606DC0", "#606DC0", "#69BDD2", "#78F1EF", "#7CF8BE", "#C1FFA1", "#E9FF80", "#FFFF80", "#FFEF80", "#FFB780", "#FF8080")
fcs$col.rainbow.pale <- c("#868EC0", "#868EC0", "#93C5D2", "#A8F1EF", "#AEF8D6", "#DEFFCD", "#F2FFB3", "#FFFFB3", "#FFFACD", "#FFD4B3", "#FFB3B3")
#mittelblau,hellblau,türkisgrün,mittelgrün,hellgrün

fcs$col.blackwhite <- c("gray90", "gray90", "gray80", "gray70", "gray60", "gray50", "gray40", "gray30", "gray20", "gray10", "gray0", "gray0")
fcs$col.blackred <- c("gray95", "gray95", "gray80", "gray65", "gray50", "gray35", "darkred", "darkred")
# green
cols  <-  c("#F4FBEF", "#E0F3D3", "#C3E1AD", "#ACD88D", "#93CC6B", "#7DC24C", "#71B441", "#63AB30", "#56972B", "#4F8726")
fcs$col.green <- col2rgb(cols)

#fcs$ncores <- detectCores() - 1

### initiate parameters
fcs$vx1 <- tclVar("")
fcs$vx2 <- tclVar("")
fcs$vy1 <- tclVar("")
fcs$vy2 <- tclVar("")

fcs$legend.space <- 0
fcs$legend.space.di <- 0
if (!grepl("linux", sessionInfo()$R.version$os)) {
  fcs$legend.space <- 0.2
  fcs$legend.space.di <- 0.03
}
### global variables - END

### if changes in database needs to made do
#dbGetQuery(DB, "ALTER TABLE Surface_Marker_SG_fileIndex RENAME TO Surface_Marker_SG_fileIdentity")
#dbGetQuery(DB, "ALTER TABLE Surface_Marker_SG_colnameIndex RENAME TO Surface_Marker_SG_markerIdentity")
#dbGetQuery(DB, "ALTER TABLE Cytokines_SG_fileIndex RENAME TO Cytokines_SG_fileIdentity")
#dbGetQuery(DB, "ALTER TABLE Cytokines_SG_colnameIndex RENAME TO Cytokines_SG_markerIdentity")

### load PRI parameters if exist ----------------------------------------
paramfile <- "myPRIparam.rda"
if (!file.exists(paramfile)) {
  printf("Loading myPRIparam.rda..")
  load(paramfile)
  print(sprintf("diploT :: saved var1=%s var2=%s", param$currVarDi1, param$currVarDi2))
  print(sprintf("triploT :: saved var1=%s var2=%s var3=%s", param$currVarTri1, param$currVarTri2, param$currVarTri3))
  print(sprintf("quadruploT :: saved var1=%s var2=%s var3=%s var4=%s", param$currVarQuad1, param$currVarQuad2, param$currVarQuad3, param$currVarQuad4))

  # FI range
  fcs$vminvalX <- tclVar(param$minvalX)
  fcs$vmaxvalX <- tclVar(param$maxvalX)
  fcs$vminvalY <- tclVar(param$minvalY)
  fcs$vmaxvalY <- tclVar(param$maxvalY)
  fcs$vminMSI <- tclVar(param$minMSI)
  fcs$vmaxMSI <- tclVar(param$maxMSI)
  fcs$vminfreq <- tclVar(param$minfreq)
  fcs$vmaxfreq <- tclVar(param$maxfreq)

  # radio buttons
  fcs$rbasinh <- tclVar(param$cofactor)
  fcs$rbcalc <- tclVar(param$rbcalc)
  fcs$rbtrans <- tclVar(param$rbtrans)
  fcs$popC1 <- tclVar(param$rbpopC1)
  fcs$popC2 <- tclVar(param$rbpopC2)

  # check buttons
  fcs$cbtfeatA <- tclVar(param$cbtfeatA)
  fcs$cbtfeatB <- tclVar(param$cbtfeatB)
  fcs$cbtfeatC <- tclVar(param$cbtfeatC)
  fcs$cbtaddFilename <- tclVar(param$cbtaddFilename)
  fcs$cbtaddDate <- tclVar(param$cbtaddDate)
  fcs$cbtdisplayA <- tclVar(param$cbtdisplayA)
  fcs$cbtshowGrid <- tclVar(param$cbtshowGrid)
  fcs$cbtshowPercentage <- tclVar(param$cbtshowPercentage)
  fcs$cbttrimming <- tclVar(param$cbttrimming)
  fcs$cbtmanRect <- tclVar(param$cbtmanRect)
  fcs$cbtdynRange <- tclVar(param$cbtdynRange)
  fcs$cbtshowLegend <- tclVar(param$cbtshowLegend)
  fcs$cbtshowMinBins <- tclVar(param$cbtshowMinBins)
  fcs$cbtgateData <- tclVar(param$cbtgateData)
  fcs$cbtautoRect <- tclVar(param$cbtautoRect)

  # graphics  
  fcs$vnrow <- tclVar(param$nrow)
  fcs$vncol <- tclVar(param$ncol)

  fcs$current.cofactor <- param$cofactor
  print("Done loading.")
} else {
  printf("Could not find \"myPRIparam.rda\", load default vars..")

  # feature diplot
  param$currVarDi1 <- 1
  param$currVarDi2 <- 2
  param$binSizesDiPos <- 2
  param$minCountDiPos <- 2
  # feature triplot
  param$currVarTri1 <- 1
  param$currVarTri2 <- 2
  param$currVarTri3 <- 3
  param$binSizesTriPos <- 2
  param$minCountTriPos <- 3
  # feature quadruplot
  param$currVarQuad1 <- 1
  param$currVarQuad2 <- 2
  param$currVarQuad3 <- 3
  param$currVarQuad4 <- 4
  param$binSizesQuadPos <- 2
  param$minCountQuadPos <- 3

  # FI range
  fcs$vminvalX <- tclVar("0")
  fcs$vmaxvalX <- tclVar("12")
  fcs$vminvalY <- tclVar("0")
  fcs$vmaxvalY <- tclVar("12")
  fcs$vminMSI <- tclVar("0")
  fcs$vmaxMSI <- tclVar("0")
  fcs$vminfreq <- tclVar("0")
  fcs$vmaxfreq <- tclVar("100")

  # radio buttons
  fcs$rbasinh <- tclVar("1")
  fcs$rbcalc <- tclVar("MSI")
  fcs$rbtrans <- tclVar("asinh")
  fcs$popC1 <- tclVar("pos")
  fcs$popC2 <- tclVar("pos")

  # check buttons
  fcs$cbtfeatA <- tclVar("0") # fix feature
  fcs$cbtfeatB <- tclVar("0")
  fcs$cbtfeatC <- tclVar("0")
  fcs$cbtaddFilename <- tclVar("1")
  fcs$cbtaddDate <- tclVar("0")
  fcs$cbtdisplayA <- tclVar("0")
  fcs$cbtshowGrid <- tclVar("1")
  fcs$cbtshowPercentage <- tclVar("1")
  fcs$cbttrimming <- tclVar("0")
  fcs$cbtmanRect <- tclVar("0")
  fcs$cbtdynRange <- tclVar("1")
  fcs$cbtshowLegend <- tclVar("1")
  fcs$cbtshowMinBins <- tclVar("0")
  fcs$cbtgateData <- tclVar("0")
  fcs$cbtautoRect <- tclVar("0")

  # graphics
  fcs$vnrow <- tclVar("2")
  fcs$vncol <- tclVar("4")

  fcs$current.cofactor <- "1"
}
