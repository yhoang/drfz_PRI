#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020

### interact functions with database -----------------------------------------------------
fcs$getVariables <- function (staintable=NA, table=NA, index=NA){
  this <- fcs
  
  if (is.na(staintable))staintable <- this$current.staintable
  
  if (is.na(index)){
    if (exists("current.file", envir=fcs)){
      file <- tclvalue(tkget(this$tkchoosefile))
    } else {
      file <- this$current.filenames[1]
    }
    index <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  } else {
    file <- this$current.filetable[index, 2]
  }
  if (this$working)printf("w: do getVariables from project #%s=%s: fileidx #%s=%s", 
                           this$selected.projectnum, this$total.projects[this$selected.projectnum], 
                           index, file)
  
  vars <- staintable[which(staintable[, 1] == index), 4]
  
  if (this$working)printf("w: vars old(%s)=%s", length(this$selected.vars), paste(this$selected.vars, collapse=" "))
  if (this$working)printf("w: vars new(%s)=%s", length(vars), paste(vars, collapse=" "))
  if (this$working)printf("w: Done getVariables")
  
  this$selected.vars <- vars
  vars
}

fcs$getCutoffs <- function (fileindex=NA, new=FALSE){
  this <- fcs
  
  if (this$working)printf("w: do getCutoffs")
  
  if (exists("current.file", envir=fcs)){
    file <- tclvalue(tkget(this$tkchoosefile))
  } else {    
    file <- this$current.filenames[1]
  }
  
  if (is.na(fileindex)){
    fileindex <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  }
  
  if (new){
    cutoffs <- this$new.cutoffs[[fileindex]]
  } else {
    cutoffs <- this$saved.cutoffs[[fileindex]]
  }
  
  cutoffs
}

fcs$getData <- function (table, fileidx, columns=NA, stain=NA, cofactor = NA){
  this <- fcs
  # get data from database
  data <- dbGetQuery(this$conn, paste("SELECT * FROM ", table, " WHERE file_ID == '", fileidx, "'", sep=""))
  
  # and ignore columns with NAs
  col.NA <- NA
  for (i in 1:ncol(data)){
    if (any(is.na(data[, i]))) col.NA <- c(col.NA, i)
  }
  if (!is.na(col.NA))data <- data[, -col.NA]
  
  # change column names
  column.names <- colnames(data)
  if (!is.na(columns)){
    data <- data[, columns + 1]
  }
  
  if (!is.na(stain) & !is.na(columns)) {
    colnames(data) <- stain[columns]
  }
  if (is.na(stain) & !is.na(columns)) {
    colnames(data) <- this$selected.vars[columns]
  }
  if (!is.na(stain) & is.na(columns)) {
    colnames(data) <- c("file_ID", stain)
  }
  if (is.na(stain) & is.na(columns)) {
    colnames(data) <- c("file_ID", this$selected.vars)
  }
  
  # set asinh cofactor to 1 if not set
  cofactor <- as.numeric(tclvalue(fcs$rbasinh))
  if (is.na(cofactor))cofactor <- 1
  
  if (!is.na(columns)){
    data <- asinh(data / cofactor)
  } else {
    data <- asinh(data[, (2:dim(data)[2] / cofactor)])             
  }
  
  this$current.project <- table 
  this$current.filenum <- fileidx
  this$current.vars <- this$selected.vars
  this$current.cutoffs <- this$selected.cutoffs
  this$current.checks <- this$selected.checks
  
  this$data.trimmed <- FALSE
  
  printf("w: do getData(%s)from table='%s' with fileidx=%s and asinh cofactor=%s", nrow(data), table, fileidx, cofactor)
  
  data
}

fcs$getRectData <- function (table, file.idx, vars=NA){
  this <- fcs
  
  if (is.na(vars))vars <- c(this$checkMarker(tclvalue(tkget(this$cbvar1))), this$checkMarker(tclvalue(tkget(this$cbvar2))))
  
  checkGATED <- tclvalue(this$cbtgateData)
  
  # if manual rect was selected
  if (tclvalue(this$cbtmanRect) == "1"){
    this$coords <- list(
      x=c(as.double(tclvalue(this$vx1)), as.double(tclvalue(this$vx2))), 
      y=c(as.double(tclvalue(this$vy1)), as.double(tclvalue(this$vy2)))
    )
  }
  
  x1 <- this$coords$x[1]
  x2 <- this$coords$x[2]
  y1 <- this$coords$y[1]
  y2 <- this$coords$y[2]
  
  #### new: gate data from this$data
  tmp.data <- this$data[which(this$data[, vars[1]] > x1 & this$data[, vars[1]] <= x2), ]
  tmp.data <- tmp.data[which(tmp.data[, vars[2]] > y1 & tmp.data[, vars[2]] <= y2), ]
  
  if (checkGATED == "1")this$data <- tmp.data
  else this$rectdata <- tmp.data
  if (this$working)printf("w: do getRectData: dim(rectdata):%s from table='%s' with fileidx=%s", paste(dim(tmp.data), collapse="x"), table, file.idx)
  ####
  
  this$current.project <- table 
  this$current.filenum <- file.idx
  this$current.vars <- this$selected.vars
  this$current.cutoffs <- this$selected.cutoffs
  this$current.checks <- this$selected.checks
}

fcs$getFile <- function (table, fileidx, stain=NA, cofactor=NA){
  this <- fcs
  
  ### checkbutton options
  checkTRANS <- tclvalue(this$rbtrans)
  if (checkTRANS == "")checkTRANS <- tclvalue(this$rbtrans) <- "asinh"
  
  # get table from database
  data <- dbGetQuery(this$conn, paste0("SELECT * FROM ", table, " WHERE file_ID == '", fileidx, "'"))
  
  # cut column "file_ID" and ignore columns with NAs
  data$file_ID <- NULL
  col.NA <- NULL
  for (i in 1:ncol(data)){
    if (any(is.na(data[, i])))col.NA <- c(col.NA, i)
  }
  if (!is.null(col.NA))data <- data[, -col.NA]
  
  this$current.project <- table
  this$current.filenum <- fileidx
  this$current.vars <- this$selected.vars
  this$current.cutoffs <- this$selected.cutoffs
  this$current.checks <- this$selected.checks
  
  # set asinh cofactor to 1 if not set
  cofactor <- as.numeric(tclvalue(fcs$rbasinh))
  if (is.na(cofactor))cofactor <- 1
  
  if (checkTRANS == "asinh"){
    data <- asinh(data / cofactor)
    
    this$current.trans <- "asinh"
    
    this$asinh$range <- c(
      tclvalue(this$vminvalX), 
      tclvalue(this$vmaxvalX), 
      tclvalue(this$vminvalY), 
      tclvalue(this$vmaxvalY)
    )
    this$asinh$binsize <- as.numeric(tkget(this$binSize))
    this$asinh$mincount <- as.numeric(tkget(this$minCountTri))
    
    this$data.trimmed <- FALSE
    
    this$current.cofactor <- cofactor
  }
  
  if (FALSE) { #} else {
    this$getIndexRange()
    
    data <- as.data.frame(this$biex$call(data))
    
    this$current.trans <- "biex"
    
    this$biex$range <- c(
      tclvalue(this$vminvalX), 
      tclvalue(this$vmaxvalX), 
      tclvalue(this$vminvalY), 
      tclvalue(this$vmaxvalY)
    )
    this$biex$binsize <- as.numeric(tkget(this$binSize))
    this$biex$mincount <- as.numeric(tkget(this$minCountTri))
    
    this$current.biex <- list(
      x=c(tclvalue(this$vminvalX), tclvalue(this$vmaxvalX)), 
      y=c(tclvalue(this$vminvalY), tclvalue(this$vmaxvalY)), 
      binSize=as.numeric(tkget(this$binSize)), 
      mincount=as.numeric(tkget(this$minCountTri))
    )
  }
  
  if (!is.na(stain)){ 
    colnames(data) <- stain 
  } else { 
    colnames(data) <- this$selected.vars 
  }
  
  if (this$working)print(head(data, 2))
  printf("w: do getFile: %s from table='%s' with fileidx=%s and asinh cofactor=%s", nrow(data), table, fileidx, cofactor)
  
  this$data <- data
  ### set check button "Gate data" to zero
  # since you load a new file
  #this$cbtgateData <- tclVar("0")
  #tkconfigure(this$cbtgateData, onvalue=FALSE, offvalue=TRUE)
}

fcs$getDFtable <- function (table){
  this <- fcs
  
  table.df <- dbGetQuery(this$conn, paste("SELECT * FROM ", table))
  
  return(table.df)
}


fcs$saveCutoffsToDB <- function(){
  this <- fcs
  
  if (this$working){
    printf("w: saveCutoffsToDB: cutoffs saved for project=%s", this$selected.project)
    printf("w: %s", paste(unlist(this$new.cutoffs), collapse=" "))
  }
  new.staintable <- cbind(this$current.staintable[, 1:4], unlist(this$new.cutoffs[!is.na(this$new.cutoffs)]), unlist(this$new.checks[!is.na(this$new.checks)]))
  colnames(new.staintable)[5] <- "file_savedCutoffs"
  colnames(new.staintable)[6] <- "file_savedChecks"
  if (this$working)print(head(new.staintable))
  #idx.stain <- grep(paste0(this$current.project, this$markerid_name), dbListTables(this$conn))
  #idx.stain <- which(dbListTables(this$conn)== paste0(this$current.project, this$markerid_name))
  
  ### get name of stain table
  name.stain <- paste0(this$selected.project, this$markerid_name)
  ### remove and rewrite stain table
  dbRemoveTable(this$conn, name.stain)
  dbWriteTable(this$conn, name.stain, new.staintable)
  
  printf("Cutoffs saved in %s", name.stain)
}

fcs$saveCutoffsToAll <- function(){
  this <- fcs
  
  answer <- tkmessageBox(title = "Saving cutoffs and checks to ALL files in project", message = "Are you sure?", icon = "info", type = "yesno")
  if (tclvalue(answer) == "yes"){
    this$selected.checks <- vector()
    this$selected.cutoffs <- vector()
    for (i in 1:length(this$selected.vars)){
      this$selected.checks[i] <- tclvalue(this$cbVal[[i]])
      this$selected.cutoffs[i] <- as.numeric(tclvalue(this$vcutoffs[[i]]))
    }
    
    if (this$working){
      printf("w: saveCutoffsToall: cutoffs/checks saved for ALL files in project=%s", this$selected.project)
      printf("w: %s", paste(unlist(this$selected.cutoffs), collapse=" "))
    }
    # this$current.staintable <- this$getDFtable(paste0(this$selected.project, this$markerid_name))
    # this$selected.filenum <- this$current.filetable[which(this$current.filetable[, 2] == this$current.filenames[1]), 1]
    
    len.staintable <- length(this$selected.cutoffs) * length(this$current.filenames)
    new.staintable <- cbind(this$current.staintable[1:len.staintable, 1:4],
        rep(unlist(this$selected.cutoffs), length(this$current.filenames)),
        rep(unlist(this$selected.checks), length(this$current.filenames))
      )
    
    colnames(new.staintable)[5] <- "file_savedCutoffs"
    colnames(new.staintable)[6] <- "file_savedChecks"
    if (this$working)print(head(new.staintable))
    #idx.stain <- grep(paste0(this$current.project, this$markerid_name), dbListTables(this$conn))
    #idx.stain <- which(dbListTables(this$conn)== paste0(this$current.project, this$markerid_name))
    
    ### get name of stain table
    name.stain <- paste0(this$selected.project, this$markerid_name)
    ### remove and rewrite stain table
    dbRemoveTable(this$conn, name.stain)
    dbWriteTable(this$conn, name.stain, new.staintable)
    
    printf("Cutoffs saved in %s", name.stain)
    
    
  }
}

fcs$connectDb <- function(fname){
  this <- fcs
  this$conn <- dbConnect(SQLite(), dbname = fname)
  print(paste("Database opened:", fname))
}

fcs$disconnectDb <- function (){
  this <- fcs
  dbDisconnect(this$conn)
  printf("w: do disconnectDb: Database closed: %s", file.path(this$db.path, this$db.name))
}

fcs$start <- function (){
  this <- fcs
  if (file.exists(file.path(fcs$db.path, fcs$db.name))){
    fcs$connectDb(file.path(fcs$db.path, fcs$db.name))
  } else {
    file <- tclvalue(tkgetOpenFile(initialdir=fcs$db.path, defaultextension="sqlite3"))
    if (file.exists(file)){
      this$connectDb(file)
      this$db.name <- file
    }
  }
  this$plotWindowExists <- FALSE
  
  this$GUImain()
}

fcs$exit <- function(){
  this <- fcs
  continue <- FALSE
  saveCutoffs <- FALSE
  answer <- tclVar("no")
  
  ### save current cutoffs
  this$refreshCutoffs(current=TRUE)
  
  ### test if cutoffs had been changed
  new <- unlist(this$new.cutoffs)
  saved <- unlist(this$saved.cutoffs)
  both <- new[!is.na(new)] == saved[!is.na(saved)]
  
  new2 <- unlist(this$new.checks)
  saved2 <- unlist(this$saved.checks)
  both2 <- new2[!is.na(new2)] == saved2[!is.na(saved2)]
  
  ### if cutoffs did not change
  # if length(new)== length(saved)
  # if both == TRUE and cutoffs were already saved in DB
  #if (length(new)== length(saved)){
  #if (((all(both) & length(saved) > 0)| all(new == 0)) & ((all(both2) & length(saved2) > 0)| all(new2 == 0))) {
  if (((all(both) & length(saved) > 0) & length(both) > 0) & ((all(both2) & length(saved2) > 0) & length(both2) > 0)) {
    ### if cutoffs did not change
    if (this$working == FALSE){
      answer <- tkmessageBox(title = "Quitting..", message = "Are you sure?", icon = "info", type = "yesno")
      if (tclvalue(answer) == "yes")continue <- TRUE
    }
  } else {
    ### if cutoffs have been changed
    answer <- tkmessageBox(title = "Quitting..", message = "Save cutoffs?", icon = "info", type = "yesnocancel")
    if (tclvalue(answer) == "yes" | tclvalue(answer) == "no")continue <- TRUE
    if (tclvalue(answer) == "yes")saveCutoffs <- TRUE
  }
  
  
  if (this$working) {
    continue <- TRUE
    tclvalue(answer) <- "yes"
    printf("w: do exit saveCutoffs=%s continue=%s answer=%s", saveCutoffs, continue, tclvalue(answer))
  }
  
  if (continue){
    
    ### save work.station
    # feature diplot
    #tab <- tclvalue(tkindex(this$panelTabs, "current"))
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1di)))[1]
    param$currVarDi1 <- which(this$selected.vars == var1)
    var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2di)))[1]
    param$currVarDi2 <- which(this$selected.vars == var2)
    printf("diploT :: var1=%s var2=%s", param$currVarDi1, param$currVarDi2)
    mincountDi <- as.numeric(tclvalue(tkget(this$minCountDi)))
    param$minCountDiPos <- which(this$min.counts.diploT == mincountDi)
    binsizeDi <- as.numeric(tclvalue(tkget(this$binSizedi)))
    param$binSizesDiPos <- which(this$binSizes == binsizeDi)

    # feature triplot
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))[1]
    param$currVarTri1 <- which(this$selected.vars == var1)
    var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))[1]
    param$currVarTri2 <- which(this$selected.vars == var2)
    var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))[1]
    param$currVarTri3 <- which(this$selected.vars == var3)
    printf("triploT :: var1=%s var2=%s var3=%s", param$currVarTri1, param$currVarTri2, param$currVarTri3)
    mincount <- as.numeric(tclvalue(tkget(this$minCountTri)))
    param$minCountTriPos <- which(this$min.counts == mincount)
    binsize <- as.numeric(tclvalue(tkget(this$binSize)))
    param$binSizesTriPos <- which(this$binSizes == binsize)

    # feature quadruplot
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1quad)))[1]
    param$currVarQuad1 <- which(this$selected.vars == var1)
    var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2quad)))[1]
    param$currVarQuad2 <- which(this$selected.vars == var2)
    var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3quad)))[1]
    param$currVarQuad3 <- which(this$selected.vars == var3)
    var4 <- this$checkMarker(tclvalue(tkget(this$cbvar4quad)))[1]
    param$currVarQuad4 <- which(this$selected.vars == var4)
    printf("quadruploT :: var1=%s var2=%s var3=%s var4=%s", param$currVarQuad1, param$currVarQuad2, param$currVarQuad3, param$currVarQuad4)
    mincountQuad <- as.numeric(tclvalue(tkget(this$minCountQuad)))
    param$minCountQuadPos <- which(this$min.counts == mincountQuad)
    binsizeQuad <- as.numeric(tclvalue(tkget(this$binSizeQuad)))
    param$binSizesQuadPos <- which(this$binSizes == binsizeQuad)

    # general
    param$cofactor <- tclvalue(this$rbasinh)
    # FI range
    minX <- tclvalue(this$vminvalX)
    param$minvalX <- minX
    maxX <- tclvalue(this$vmaxvalX)
    param$maxvalX <- maxX
    minY <- tclvalue(this$vminvalY)
    param$minvalY <- minY
    maxY <- tclvalue(this$vmaxvalY)
    param$maxvalY <- maxY
    minMSI <- tclvalue(this$vminMSI)
    param$minMSI <- minMSI
    maxMSI <- tclvalue(this$vmaxMSI)
    param$maxMSI <- maxMSI
    minfreq <- tclvalue(this$vminfreq)
    param$minfreq <- minMSI
    maxfreq <- tclvalue(this$vmaxfreq)
    param$maxfreq <- maxfreq

    # radio and check buttons
    checkCALC <- tclvalue(this$rbcalc)
    param$rbcalc <- checkCALC
    checkTRANS <- tclvalue(this$rbtrans)
    param$rbtrans <- checkTRANS
    checkpopC1 <- tclVar(param$rbpopC1)
    param$rbpopC1 <- checkpopC1
    checkpopC2 <- tclVar(param$rbpopC2)
    param$rbpopC2 <- checkpopC2

    checkfeatA <- tclvalue(this$cbtfeatA) # fix feature
    param$cbtfeatA <- checkfeatA
    checkfeatB <- tclvalue(this$cbtfeatB)
    param$cbtfeatB <- checkfeatB
    checkfeatC <- tclvalue(this$cbtfeatC)
    param$cbtfeatC <- checkfeatC

    checkADDFILE <- tclvalue(this$cbtaddFilename)
    param$cbtaddFilename <- checkADDFILE
    checkADDDATE <- tclvalue(this$cbtaddDate)
    param$cbtaddDate <- checkADDDATE
    checkDISPLAYA <- tclvalue(this$cbtdisplayA)
    param$cbtdisplayA <- checkDISPLAYA
    checkGRID <- tclvalue(this$cbtshowGrid)
    param$cbtshowGrid <- checkGRID
    checkPERCENTAGE <- tclvalue(this$cbtshowPercentage)
    param$cbtshowPercentage <- checkPERCENTAGE
    param$cbttrimming <- tclvalue(this$cbttrimming)
    checkMANRECT <- tclvalue(this$cbtmanRect)
    param$cbtmanRect <- checkMANRECT
    checkDYNRANGE <- tclvalue(this$cbtdynRange)
    param$cbtdynRange <- checkDYNRANGE
    checkLEGEND <- tclvalue(this$cbtshowLegend)
    param$cbtshowLegend <- checkLEGEND
    checkSHOWMINBIN <- tclvalue(this$cbtshowMinBins)
    param$cbtshowMinBins <- checkSHOWMINBIN
    checkGATEDATA <- tclvalue(this$cbtgateData)
    param$cbtgateData <- checkGATEDATA
    checkAUTORECT <- tclvalue(this$cbtautoRect)
    param$cbtautoRect <- checkAUTORECT
    # graphics
    graphROW <- tclvalue(this$vnrow)
    param$nrow <- graphROW
    graphCOL <- tclvalue(this$vncol)
    param$ncol <- graphCOL
    save(param, file = "myPRIparam.rda")
    ###
    
    tkdestroy(this$tt)
    if (class(this$pb) != "NULL")close(this$pb)
    
    if (exists("ttdebug", where = eval(parse(text = "fcs")))) {
      tkdestroy(this$ttdebug)
    }
    if (exists("ttpreproc", where = eval(parse(text = "fcs")))) {
      tkdestroy(this$ttpreproc)
    }
    if (this$ttprojectsopen) {
      tkdestroy(this$ttprojects)
    }
    graphics.off()
    
    if (file.exists(this$png.file))system(paste("rm ", this$png.file, sep=""))
    
    temp.table.idx <- grep("^temp\\d+", dbListTables(this$conn))
    len <- length(temp.table.idx)
    if (len > 0){
      for (i in len:1){
        table.name <- dbListTables(this$conn)[temp.table.idx[i]]
        dbRemoveTable(this$conn, table.name)
        print(paste0("Removing temporary table '", table.name, "'"))
      }
    }
    
    ### save cutoffs into MarkerInfo table
    if (saveCutoffs)this$saveCutoffsToDB()
    this$disconnectDb()
  }
}

fcs$exitandstart <- function (){
  this <- fcs
  
  this$exit()
  source("Start_PRI-ana.R")
  # load(file = "myPRIparam.rda")
}

if (length(strsplit(fcs$db.name, "")[[1]]) >  0){
  fcs$connectDb(file.path(fcs$db.path, fcs$db.name))
} else {
  file <- tclvalue(tkgetOpenFile(initialdir=fcs$db.path, defaultextension="sqlite3"))
  fcs$connectDb(file)
  fcs$db.name <- file
}
