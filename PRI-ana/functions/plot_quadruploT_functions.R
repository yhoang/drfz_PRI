#! / usr / bin / R
# Author: Yen Hoang
# DRFZ 2015-2020


### quadruploT functions -----------------------------------------------------
fcs$doquadruploT <- function() {
  # calculate and display plot
  this <- fcs
  printf("w: do doquadruploT")
  
  this$refreshPlotters()
  
  checkGATED <- tclvalue(this$cbtgateData)
  checkCALC <- tclvalue(this$rbcalc)
  checkTRANS <- tclvalue(this$rbtrans)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkDATE <- tclvalue(this$cbtaddDate)
  checkTRIMMING <- tclvalue(this$cbttrimming)
  if (checkTRIMMING == "1") {
    this$preprocData(mode="trim")
  }
  checkDYNRANGE <- tclvalue(this$cbtdynRange)
  if (checkDYNRANGE != "1") {
    min.MSI <- this$checkDigits(num=tkget(this$minMSI))
    max.MSI <- this$checkDigits(num=tkget(this$maxMSI))
    if (max.MSI <= min.MSI) {
      tmp <- min.MSI 
      min.MSI <- max.MSI
      max.MSI <- tmp
    }
  }
  
  if (checkTRANS == "asinh") {
    scale <- this$asinh$scale
    label <- this$asinh$label
    grid.step <- this$asinh$step
  } else {
    scale <- this$biex$scale
    label <- this$biex$label
    grid.step <- this$biex$step
  } 
  popul <- c(1, 1)
  
  quadrants.col <- "black"
  
  v1 <- this$checkMarker(tclvalue(tkget(this$cbvar1quad)))
  tkset(this$cbvar1, v1)
  v2 <- this$checkMarker(tclvalue(tkget(this$cbvar2quad)))
  tkset(this$cbvar2, v2)
  v3 <- this$checkMarker(tclvalue(tkget(this$cbvar3quad)))
  tkset(this$cbvar3, v3)
  v4 <- this$checkMarker(tclvalue(tkget(this$cbvar4quad)))
  tkset(this$cbvar3, v4)
  vars <- c(v1, v2, v3, v4)
  
  ### if Feature A is not in sample
  if (any(length(v1) == 0 | length(v2) == 0 | length(v3) == 0 | length(v4) == 0)) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your Features.")
    stop("One or more selected features are not existent.")
  }
  
  ### if manual range for z-axis is checked but no input
  if (tclvalue(this$cbtdynRange) == "0" & tclvalue(this$vmaxMSI) == "0") {
    tkmessageBox(title = "An error has occured!", 
                 message = "You forgot to set maximum manual range for Feature C (It is still zero).", icon = "error", type = "ok")
    stop("Set maximum manual range for Feature C (It is still zero).")
  }
  
  cutoffz1 <- this$checkDigits(cutoff_id=which(this$selected.vars == v3))
  cutoffz2 <- this$checkDigits(cutoff_id=which(this$selected.vars == v4))
  
  ### cutoff(z1, z2) needs to be setted
  if (cutoffz1 <= 0 | cutoffz2 <= 0) {
    tkmessageBox(title = "An error has occured!", 
                 message = "You forgot to set the cutoff for Feature C1 and/or C2.", icon = "error", type = "ok")
    stop("Missing production cutoff for Feature C1 and/or C2.")
  }
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  dev.label <- paste("plotter", "quad", this$plotter.quad.num, sep=".")
  if (length(which(dev.label == names(devList()))) == 0) {
    this$plotter.quad.num <- this$plotter.quad.num + 1
    dev.label <- paste("plotter", "quad", this$plotter.quad.num, sep=".")
    ncol <- as.numeric(tclvalue(this$vncol))
    nrow <- as.numeric(tclvalue(this$vnrow))
    devNew(type="x11", title="n-quadruploTs", width=ncol * 3.4, height=nrow * 3.7, label=dev.label)
    # mar in points, mai in inches
    # oma adds title lines
    # order: bottom, left, top, and right
    par(mfrow=c(nrow, ncol), oma=c(0.5, 1, 2, 1), mar=c(3, 3, 4, 2))
    
    this$plot.windows <- c(this$plot.windows, dev.label)
  } else {
    devSet(devList()[which(dev.label == names(devList()))])
  }
  
  this$rect.lwd <- 1
  
  empty <- FALSE
  file <- tclvalue(tkget(this$tkchoosefile))
  file <- unlist(strsplit(file, "\\; |\\;|\\. | ")[[1]])
  binSize <- as.numeric(tkget(this$binSizeQuad))
  mincount <- as.numeric(tkget(this$minCountQuad))
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  
  tkconfigure(this$tt, cursor = "watch")
  if ((checkGATED == "1") & (this$temp.num > 0)) {
    #   table = file
    #   file = this$plot.attr[[1]]$file.name
    #   file.idx = 1
    # } else if (grepl("^temp", file)) {
    table <- file
    file.idx <- 0
  } else {
    table <- this$selected.project
    file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
    this$selected.filenum <- file.idx
    this$coords.info <- vector()
  }
  
  displayfile <- this$shortenFilename(file)
  
  file2 <- ""
  #### new getFile if temporary file is not present
  timeSTART <- Sys.time()
  if (is.null(this$data) | this$current.project != table | this$current.filenum != file.idx | this$current.trans != tclvalue(this$rbtrans) | this$current.cofactor != as.numeric(tclvalue(this$rbasinh))) {
    print("Time loading data:")
    this$getFile(table, file.idx)
    print(Sys.time() - timeSTART)
  }
  ####
  
  tdata <- this$data[vars]
  this$tdata <- tdata
  
  idx.X <- which(this$selected.vars == v1)
  idx.Y <- which(this$selected.vars == v2)
  idx.C1 <- which(this$selected.vars == v3)
  idx.C2 <- which(this$selected.vars == v4)
  cutoffs <- c(idx.X, idx.Y, idx.C1, idx.C2)
  
  ### if percentage is checked
  # calculate cutoffs and set check button to zero
  for (i in 1:length(cutoffs)) {
    if (tclvalue(this$cbcutoffperc[[cutoffs[i]]]) == "1") {
      cutoffs[i] <- this$calcCUTOFF(tdata[vars[i]], this$checkDigits(cutoff_id=cutoffs[i]), vars[i], cutoffs[i])
    } else {
      cutoffs[i] <- this$checkDigits(cutoff_id=cutoffs[i])
    }
  }
  printf("w: do doquadruploT :: file: %s", file)
  printf("w: do doquadruploT :: table: %s", table)
  printf("w: do doquadruploT :: file.idx=%s", file.idx)
  printf("w: do doquadruploT :: cutoffs=%s", paste(cutoffs, collapse=" "))
  
  ### calculate cells which were not plotted 
  cells.overmaxFI <- length(which(tdata[, 1] > xmaxval | tdata[, 2] > ymaxval))
  cells.underminFI <- length(which(tdata[, 1] < xminval | tdata[, 2] < yminval))
  cells.overmaxFI.perc <- round(100 * cells.overmaxFI / (dim(tdata)[1] - cells.underminFI))
  ### warn if more then 5% productive cells (q2 + q3 + q4) werent plotted
  if (cells.overmaxFI.perc >= 5 & !this$working) {
    tkmessageBox(title = "Warning!", 
                 message = sprintf("Your cells exceed %s%% of your plot max ranges. You might want to increase your max ranges.",
                 cells.overmaxFI.perc), 
                 icon = "info", type = "ok")
  }
  
  timeSTART <- Sys.time()
  if (this$working) print("Time loading plot:")
  
  # start plot
  # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
  # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
  set.cex <- 1.2
  set.cex.axes <- 1.0
  set.mgp <- c(1.7, 0.4, 0)

  if (cutoffs[1] > 0) {
    title.axis <- sprintf("%s (%s)", v1, cutoffs[1])
  } else {
    title.axis <- v1
  }
  
  if (cutoffs[2] > 0) {
    title.axis <- c(title.axis, sprintf("%s (%s)", v2, cutoffs[2]))
  } else {
    title.axis <- c(title.axis, v2)
  }
  
  plot(1, type="n", frame.plot=FALSE, xlim=c(xminval, xmaxval + 10 * binSize), axes=FALSE, ylim=c(yminval - 2.5 * binSize, ymaxval + 5 * binSize), xlab=title.axis[1], ylab=title.axis[2], cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
  box(lwd=0.8, col="darkgrey")
  
  ### draw axis on the bottom and on the left
  axis(side=1, at=scale, labels=label, las=1, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
  axis(side=2, at=scale, labels=label, las=3, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
  
  ### add grid
  if (checkGRID == "1") {
    xgrid.steps <- seq(0, (xmaxval), by=grid.step)
    ygrid.steps <- seq(0, (ymaxval), by=grid.step)
    abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
  }
  
  
  ### calc quadrants in total
  ncells <- ncells.total <- nrow(tdata)
  
  ### OLD tdata.zero where all rows with negative values where filtered
  #tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
  ### NEW tdata.zero where ONLY rows where filtered if v1 OR v2 are negative
  tdata.zero <- tdata[which(tdata[, 1] >= 0 | tdata[, 2] >= 0), ]
  ncells.zero <- nrow(tdata.zero)
  
  # q1 = left bottom
  # q2 = right bottom
  # q3 = right top
  # q4 = left top
  ### count cells in quadrant
  tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] < cutoffs[2]), ]
  tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] < cutoffs[2]), ]
  tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] >= cutoffs[2]), ]
  tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] >= cutoffs[2]), ]

  ### q[x].total [ink=black]
  ### percentage of cells in quadrant to total cells 
  this$q1.total <- abs(100 * nrow(tdata.q1) / ncells)
  this$q2.total <- abs(100 * nrow(tdata.q2) / ncells)
  this$q3.total <- abs(100 * nrow(tdata.q3) / ncells)
  this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)


  ######## NEW: SWITCH SIGNS FOR POPULATION OF INTEREST ---------------------------
  if (popul[1] == 1) {
    sign.C1 <- c(" >= ", "<")
    feat.label.C1 <- paste0(v3, "+")
  } else {
    sign.C1 <- c("<=", ">")
    feat.label.C1 <- paste0(v3, "-")
  }
  
  if (popul[2] == 1) {
    sign.C2 <- c(" >= ", "<")
    feat.label.C2 <- paste0(v4, "+")
  } else {
    sign.C2 <- c("<=", ">")
    feat.label.C2 <- paste0(v4, "-")
  }
  feat.label.C1 <- paste(sprintf("%s(%s)", feat.label.C1, cutoffs[3]))
  feat.label.C2 <- paste(sprintf("%s(%s)", feat.label.C2, cutoffs[4]))
  #########

  ### this$q[x].prodcells [ink=red]
  ### percentage of cells which are positive for feature C1/C2 in quadrant to total quadrant cells
  eval(parse(text=paste("q1.prodcells.num <- nrow(tdata.q1[which(tdata.q1[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q1[, 4]", sign.C2[1], "cutoffs[4]), ])", sep="")))
  eval(parse(text=paste("q2.prodcells.num <- nrow(tdata.q2[which(tdata.q2[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q2[, 4]", sign.C2[1], "cutoffs[4]), ])", sep="")))
  eval(parse(text=paste("q3.prodcells.num <- nrow(tdata.q3[which(tdata.q3[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q3[, 4]", sign.C2[1], "cutoffs[4]), ])", sep="")))
  eval(parse(text=paste("q4.prodcells.num <- nrow(tdata.q4[which(tdata.q4[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q4[, 4]", sign.C2[1], "cutoffs[4]), ])", sep="")))
  this$q1.prodcells <- round((100 * q1.prodcells.num / nrow(tdata.q1)), 2)
  if (is.nan(this$q1.prodcells)) {
    this$q1.prodcells <- 0
  }
  this$q2.prodcells <- round((100 * q2.prodcells.num / nrow(tdata.q2)), 2)
  if (is.nan(this$q2.prodcells)) {
    this$q2.prodcells <- 0
  }
  this$q3.prodcells <- round((100 * q3.prodcells.num / nrow(tdata.q3)), 2)
  if (is.nan(this$q3.prodcells)) {
    this$q3.prodcells <- 0
  }
  this$q4.prodcells <- round((100 * q4.prodcells.num / nrow(tdata.q4)), 2)
  if (is.nan(this$q4.prodcells)) {
    this$q4.prodcells <- 0
  }

  ### this$q[x].prodcellsplus [ink=green]
  ### percentage of cells which are positive for feature C1/C2 to total cells
  eval(parse(text = paste("this$q1.prodcellsplus <- round(100 * nrow(tdata.q1[which(tdata.q1[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q1[, 4]", sign.C2[1], "cutoffs[4]), ]) / ncells.total, 2)", sep = "")))
  if (is.nan(this$q1.prodcellsplus)) {
    this$q1.prodcellsplus <- 0
  }
  eval(parse(text = paste("this$q2.prodcellsplus <- round(100 * nrow(tdata.q2[which(tdata.q2[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q2[, 4]", sign.C2[1], "cutoffs[4]), ]) / ncells.total, 2)", sep = "")))
  if (is.nan(this$q2.prodcellsplus)) {
    this$q2.prodcellsplus <- 0
  }
  eval(parse(text = paste("this$q3.prodcellsplus <- round(100 * nrow(tdata.q3[which(tdata.q3[, 3]", sign.C1[1], "cutoffs[3] ", "& tdata.q3[, 4]", sign.C2[1], "cutoffs[4]), ]) / ncells.total, 2)", sep = "")))
  if (is.nan(this$q3.prodcellsplus)) {
    this$q3.prodcellsplus <- 0
  }
  eval(parse(text=paste("this$q4.prodcellsplus <- round(100 * nrow(tdata.q4[which(tdata.q4[, 3]", sign.C1[1], "cutoffs[3] ",  "& tdata.q4[, 4]", sign.C2[1], "cutoffs[4]), ]) / ncells.total, 2)", sep="")))
  if (is.nan(this$q4.prodcellsplus)) {
    this$q4.prodcellsplus <- 0
  }

  if (this$working) {
    printf("cells total: %s", ncells.total)
    printf("cells in quadrant: botleft q1:%s, botright q2: %s, topright q3: %s, topleft q4: %s", nrow(tdata.q1), nrow(tdata.q2), nrow(tdata.q3), nrow(tdata.q4))
    printf("cells double prod in quadrant (black): %.1f %.1f %.1f %.1f", q1.prodcells.num, q2.prodcells.num, q3.prodcells.num, q4.prodcells.num)
    printf("%% cells double prod in quad (red):  %.1f %.1f %.1f %.1f", this$q1.prodcells, this$q2.prodcells, this$q3.prodcells, this$q4.prodcells)
    printf("%% cells double prod in quad to total (green): %.1f %.1f %.1f %.1f", this$q1.prodcellsplus, this$q2.prodcellsplus, this$q3.prodcellsplus, this$q4.prodcellsplus)
  }

  
  if (checkGATED != "1") {
    this$origin.ncells <- ncells
    this$coords <- list()
  }
  
  ### plot bins ---------------------------------------------------------------
  this$binquadruplot(data=tdata, cutoffs=cutoffs, binSize=binSize, mincells=mincount)
  
  if (checkGATED != "1") {
    this$ncell.sel <- this$origin.ncells
    this$ncell.perc <- round(this$ncell.sel / this$origin.ncells * 100, 2)
    tkconfigure(this$ncell.gui, text=as.character(this$origin.ncells))
    tkconfigure(this$ncell.sel.gui, text=as.character(this$ncell.sel))
    tkconfigure(this$ncell.perc.gui, text=as.character(this$ncell.perc))
    tkconfigure(this$ncell.gui.di, text=as.character(this$origin.ncells))
    tkconfigure(this$ncell.sel.gui.di, text=as.character(this$ncell.sel))
    tkconfigure(this$ncell.perc.gui.di, text=as.character(this$ncell.perc))
  }
  
  ### add title for single plot
  if (checkGATED == "1") {
    firstLine <- sprintf("%s * (%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
  } else {
    firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
  }
  title(main=firstLine, line=3.2, cex.main=0.9, adj=0)
  
  if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
    secondLine <- sprintf("cells(min/max)=%s/%s; %s", mincount, this$maxcells, v3)
  } else {
    secondLine <- sprintf("cells(min/max)=%s/%s", mincount, this$maxcells)
  }
  title(main=secondLine, line=2.4, cex.main=0.9, adj=0)
  
  thirdLine <- sprintf("%s-%s(%0.1f%%); binSize=%s, #bins=%s", ncells.total, ncells.zero, (ncells.zero / ncells.total * 100), binSize, this$bincount)
  title(main=thirdLine, line=1.6, cex.main=0.7, adj=0)
  
  if (checkGATED == "1" | grepl("temp", file)) {
    if (length(this$coords.info) > 2) {
      fourthLine <- sprintf("%s %s", this$coords.info[1], this$coords.info[2])
      fifthLine <- sprintf("%s %s", this$coords.info[3], this$coords.info[4])
    } else {
      fourthLine <- sprintf("%s", paste(this$coords.info, collapse=";"))
      fifthLine <- ""
    }
    title(main=fourthLine, line=0.9, cex.main=0.6, adj=0)
    title(main=fifthLine, line=0.2, cex.main=0.6, adj=0)
  }
  
  ############# start plot history
  this$plot.num <- this$plot.num + 1
  ### push plot attributes one down
  # for (i in this$plot.num:1) {
  #   this$plot.attr[i + 1] <- this$plot.attr[i]
  # } 
  
  ### save FI ranges for this transformation type
  this$changeFI.range(mode=3)
  
  ############# stop plot history
  if (this$working) print(Sys.time() - timeSTART)
  
  tkconfigure(this$tt, cursor = "left_ptr")
  
  if (checkDATE == "1") {
    date <- gsub("-", "", Sys.Date())
    title(main=date, outer=TRUE, line=1, cex.main=1.3, adj=1)
  }
}

fcs$binquadruplot <- function(
  data, 
  cutoffs, 
  set.cex=1.1, 
  set.cex.axes=1.0, 
  set.mgp=c(1.5, 0.3, 0), 
  binSize=0.2, 
  mincells=10, 
  density=FALSE, 
  png=FALSE, 
  bg=FALSE, 
  overview=FALSE, 
  checkCALC=NA, 
  quadrants.color="black", 
  data.origin=NA, 
  file=NA) {
  this <- fcs

  if (FALSE) {
    # for debugging run manual
    data <- tdata
    set.cex <- 1.1
    set.cex.axes <- 1 - 0
    set.mgp <- c(1.5, 0.3, 0)
    png <- FALSE
    overview <- FALSE
    checkCALC <- NA
    quadrants.color <- "black"
    data.origin <- NA
    file <- NA
    popul <- c(1, 1)
  }
  
  prodcells.color <- "red"
  prodpluscells.color <- "chartreuse4"
  
  if (is.na(file)) file <- tclvalue(tkget(this$tkchoosefile))
  
  displayfile <- this$shortenFilename(file)
  
  metadatafile <- sprintf("%s_PRIquadruvis_metadata.csv", this$current.project)
  if (!file.exists(metadatafile)) {
    header <- c("date", "sample", "cofactor", "calc",
              "feat.A", "feat.B", "feat.C1", "feat.C2", 
              "cutoff.A", "cutoff.B", "cutoff.C1", "cutoff.C2",
              "q1.total", "q2.total", "q3.total", "q4.total",
              "q1.prodcells", "q2.prodcells", "q3.prodcells", "q4.prodcells", "q1.prodcellsplus", "q2.prodcellsplus", "q3.prodcellsplus", "q4.prodcellsplus")
    write.table(t(header), metadatafile, sep = "\t", row.names=FALSE, col.names=FALSE)
  }
  current.date <- format(Sys.Date(), "%y%m%d")
  
  data <- as.matrix(data)
  if (!is.na(data.origin)) data.origin <- as.matrix(data.origin)
  
  # axes range
  xmin.val <- as.numeric(tkget(this$minvalX))
  xmax.val <- as.numeric(tkget(this$maxvalX))
  ymin.val <- as.numeric(tkget(this$minvalY))
  ymax.val <- as.numeric(tkget(this$maxvalY))
  min.freq <- 0
  max.freq <- as.double(tclvalue(this$vmaxfreq))
  
  if (max.freq < min.freq) {
    tmp <- min.freq 
    min.freq <- max.freq
    max.freq <- tmp
  }

  if (popul[2] == 1) {
    sign.C2 <- c(" >= ", "<")
    feat.label.C2 <- paste0(v4, "+")
  } else {
    sign.C2 <- c("<=", ">")
    feat.label.C2 <- paste0(v4, "-")
  }

  # checkbutton options
  checkDYNRANGE <- tclvalue(this$cbtdynRange)
  checkTRANS <- tclvalue(this$rbtrans)
  if (checkTRANS  == "") checkTRANS <- tclvalue(this$rbtrans) <- "asinh"
  checkCALC <- tclvalue(this$rbcalc)
  if (checkCALC == "") checkCALC <- tclvalue(this$rbcalc) <- "MSI"
  if (checkCALC == "density") density <- TRUE
  checkPERCENTAGE <- tclvalue(this$cbtshowPercentage)
  if (checkPERCENTAGE == "1") {
    this$plot.percentage <- TRUE
  } else {
    this$plot.percentage <- FALSE
  }
  
  if (this$working) printf("w: do binquadruplot density=%s checkCALC=%s", density, checkCALC)
  
  if (checkTRANS == "asinh") {
    scipen <- this$asinh$scipen
  } else {
    scipen <- this$biex$scipen
  }
  options(scipen = scipen)
  
  # legend from blue to red
  if (bg) {
    cols <- rep("gray", 12)
  } else {
    cols <- this$col.green
  }
  
  # legend title
  tmp <- unlist(strsplit(colnames(data)[3], "\\."))
  if (length(tmp) > 1 & all(lengths(tmp) > 1)) {
    if (cutoffs[3] > 0) legend.title <- sprintf("%s(%s)", tmp[1], cutoffs[3]) 
    else legend.title <- tmp[1]
  } else {
    if (cutoffs[3] > 0) legend.title <- sprintf("%s(%s)", colnames(data)[3], cutoffs[3]) 
    else legend.title <- colnames(data)[3]
  }
  
  # set negative values of z-axis (colnum=3) to zero
  if (!density) data[which(data[, 3] < 0), 3] <- 0
  
  ncells <- nrow(data)
  if (ncells == 0) stop("No cells found.")
    
  ### tdata <- cut cells which lie in plot area
  tdata <- data[which(data[, 1] >= xmin.val & data[, 2] >= ymin.val), ]
    
  ### construct bins
  fX <- cut(tdata[, 1], breaks=seq(xmin.val, xmax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
  fY <- cut(tdata[, 2], breaks=seq(ymin.val, ymax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
  tab <- table(fX, fY)
    
  colnames(tab) <- seq(ymin.val, ymax.val - binSize, by=binSize)
  rownames(tab) <- seq(xmin.val, xmax.val - binSize, by=binSize)
  fXY <- as.factor(paste(fX, fY))
    
  if (density) {
    # number of cells in bin
    my.calc <- aggregate(tdata[, 3], by=list(fXY), length)
  } else {
    ########## CALCULATE FREQUENCIES
    ### frequency of feature Z1
    eval(parse(text=paste("my.calc = aggregate(tdata[, 3], by=list(fXY), 
          function(x) {
            y = round(100 * length(which(x", sign.C1[1], " cutoffs[3])) / length(x))
            return(y)
          })", sep="")))
    # bin color factor Z1
    my.calc.fac.C1 <- cut(my.calc$x, breaks=seq(0, 100, by=10), labels=1:10, include.lowest=TRUE)
    names(my.calc.fac.C1) <- my.calc$x

    ### frequency of feature Z2        
    eval(parse(text=paste("freq.C2 = aggregate(tdata[, 4], by=list(fXY), 
          function(x) {
            y = round(100 * length(which(x", sign.C2[1], " cutoffs[4])) / length(x))
            return(y)
          })", sep="")))
    # bin color factor Z2
    my.calc.fac.C2 <- cut(freq.C2$x, breaks=seq(0, 100, by=10), labels=1:10, include.lowest=TRUE)
    names(my.calc.fac.C2) <- freq.C2$x

    ### get only data table where Z1 and Z2 is produced
    eval(parse(text=paste("tdata.touble = data[ which((data[, 3]", sign.C1[1], " cutoffs[3]) ", "& (data[, 4]", sign.C2[1], " cutoffs[4])), ]", sep="")))
    ### construct bin table with number of cells per bin
    fX.double <- cut(tdata.touble[, 1], breaks=seq(xmin.val, xmax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
    fY.double <- cut(tdata.touble[, 2], breaks=seq(ymin.val, ymax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
    fXY.double <- as.factor(paste(fX.double, fY.double))
    
    length.double <- aggregate(tdata.touble[, 1], by=list(fXY.double), length)
    rownames(length.double) <- length.double$Group.1
    
    length.all <- aggregate(tdata[, 3], by=list(fXY), length)
    rownames(length.all) <- length.all$Group.1
    
    freq.double <- merge(length.all, length.double, by="row.names", all.x=TRUE)
    freq.double <- freq.double[, -c(2, 4)]
    freq.double <- cbind(freq.double, round(freq.double[, 3] / freq.double[, 2] * 100))
    
    # bin color factor double producer Z1 + Z2
    my.calc.fac.double <- cut(freq.double[, 4], breaks=seq(0, max.freq, by=max.freq / 10), labels=1:10, include.lowest=TRUE)
    names(my.calc.fac.double) <- freq.double[, 4]

    ### combine all frequencies in one table
    my.calc <- cbind(my.calc, fac.C1=as.numeric(my.calc.fac.C1))
    my.calc <- cbind(my.calc, freq.C2=freq.C2$x)
    my.calc <- cbind(my.calc, fac.C2=as.numeric(my.calc.fac.C2))
    my.calc <- cbind(my.calc, freq.double=freq.double[, 4])
    my.calc <- cbind(my.calc, fac.double=as.numeric(my.calc.fac.double))
    my.calc <- cbind(my.calc, ncells=length.all$x)
    my.calc <- cbind(my.calc, ncells.double = freq.double[, 3])

    max.freq.real <- max(my.calc$freq.double[which(!is.na(my.calc$freq.double) & my.calc$ncells > 9)])
    printf("MAXIMUM FREQUENCY = %s", max.freq.real)
    if (max.freq.real > max.freq) print("  !!!!! MAXFREQ SHOULD BE HIGHER !!!!!")
    
    this$my.calc.fac.C1 <- my.calc.fac.C1
    this$my.calc.fac.C2 <- my.calc.fac.C2
    this$my.calc.fac.double <- my.calc.fac.double
  }
  
  my.lengths <- aggregate(tdata[, 3], by=list(fXY), length)
  my.LENGTHS <- any(my.lengths[, 2] >= mincells)
   
  absRange <- 0
  ### if there are bins to display
  if (my.LENGTHS) {
    # there are bins to plot, so set grey.label to FALSE
    grey.label <- FALSE
    
    my.calc <- cbind(my.calc, ncells=my.lengths$x)
    ### get steps for legend and plot in mode "freq"
    decim <- 0
    #range <- ""
    col.minmax <- "black"
    if (density) {
      idx <- which(my.calc$ncells >= mincells)
      min.range <- floor(min(my.calc[idx, "x"]) * 10) / 10
      max.range <- max(tab)

      if (max.range < 200) {
        col.minmax <- "red"
      }
    } else {
      # frequency
      min.range <- min.freq
      max.range <- ymin.val + 3 * binSize + diff(c(ymin.val, ymax.val)) / 2
      
      label.steps <- seq(min.freq, max.freq, length.out = 11)
      
      # bin color factor
      my.calc.fac <- cut(my.calc$x, breaks=seq(min.freq, max.freq, length.out = 11), labels=1:10, include.lowest=TRUE)
      levels(my.calc.fac) <- c(0, levels(my.calc.fac), 11, 12)
      
    }
    
    # get steps
    step <- round(diff(range(max.range, min.range)) / 10, 2) 
    steps <- seq(min.range, max.range, by=step)
    label.steps <- steps[1:11]
    if (density | checkTRANS == "biex") {
      if (max.range > 500) {
        label.steps <- round(label.steps, -2)
      } else {
        label.steps <- round(label.steps)
      }
      if (density) label.steps[1] <- min.range
    } else if (checkDYNRANGE != "1") {
      if (label.steps[1] != "0") label.steps[1] <- sprintf("<=%s", label.steps[1])
      label.steps[11] <- sprintf(" >= %s", label.steps[11])
    } else {
      label.steps <- round(label.steps, 1)
    }
      
    # bin color factor
    my.calc.fac <- cut(my.calc$x, breaks=steps, labels=2:11, include.lowest=TRUE)
      
    levels(my.calc.fac) <- c(0, levels(my.calc.fac), 12)
    # if x < min.range
    my.calc.fac[which(my.calc$x < steps[1] & my.calc$ncells >= mincells)] <- 0
    # if x > max.range
    my.calc.fac[which(my.calc$x > steps[11] & my.calc$ncells >= mincells)] <- 11
    my.calc <- cbind(my.calc, fac=as.numeric(my.calc.fac) + 1)
    
    this$my.calc <- my.calc
    this$bincount <- 0
    this$maxcells <- 0
    
    cols.heat <- cols[, as.numeric(my.calc.fac.double)]
    ##### plot bins
    for (x in rownames(tab)) {
      for (y in colnames(tab)) {
        brackets.open <- c("(", "[")
        brackets.1 <- brackets.2 <- 1
        if (x == 0) brackets.1 <- 2
        if (y == 0) brackets.2 <- 2

        if (tab[x, y] >= mincells) {
          # if enough cells
          # get label
          fact <- as.factor(paste(brackets.open[brackets.1], x, ",", as.numeric(x) + binSize, "] ", brackets.open[brackets.2], y, ",", as.numeric(y) + binSize, "]", sep = ""))
          idx <- which(as.character(fact) == as.character(my.calc$Group.1))

          if ((length(cols.heat[, idx]) != 0) & (!is.na(cols.heat[1, idx]))) {
            rect(x, y, as.numeric(x) + binSize, as.numeric(y) + binSize,
              col = eval(parse(text = paste0("rgb(", paste0(cols.heat[, idx], collapse = ", "), ", maxColorValue=255)"))),
              border = NA)

            this$bincount <- this$bincount + 1
            if (tab[x, y] > this$maxcells) {
              this$maxcells <- tab[x, y]
            }
          }
        }
      }
    }
    ### space calculation
    rect.size <- diff(c(xmin.val, xmax.val)) / 25
    rect.size <- 0.2
    
    ### add production line
    this$addProdline(cutoffs)
    
    
    ###### legend plot + label
    if (density) {
      min.legend <- ymin.val + 3 * binSize
      max.legend <- ymin.val + 3 * binSize + diff(c(ymin.val, ymax.val)) / 2 
      
      step <- round(diff(range(max.legend, min.legend)) / 11, 1)
      steps <- seq(min.legend, max.legend + step, by=step)
      set.cex <- 1.2
    }
    
    ##### if its not background and not a png minimalistic picture, then print legend and label
    if ((!bg | density) & !png & this$plot.percentage) {
      if (cutoffs[1] > 0 & cutoffs[2] > 0 & cutoffs[3] > 0 & !density) {
        #if (this$working) print("w: add quadrant and product percentages on quadruplot")
        ### quadrant left lower
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.08 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.13 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        
        ### quadrant right lower
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.08 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.13 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        
        ### quadrant right upper
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.14 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        
        ### quadrant left upper
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.14 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        
        
        quadrant.percs <- cbind(current.date, displayfile, this$current.cofactor, checkCALC, colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange, cutoffs[1], cutoffs[2], cutoffs[3], round(this$q1.total, 1), round(this$q2.total, 1), round(this$q3.total, 1), round(this$q4.total, 1), round(this$q1.prodcells, 1), round(this$q2.prodcells, 1), round(this$q3.prodcells, 1), round(this$q4.prodcells, 1), round(this$q1.prodcellsplus, 1), round(this$q2.prodcellsplus, 1), round(this$q3.prodcellsplus, 1), round(this$q4.prodcellsplus, 1))
        
        write.table(quadrant.percs, sprintf("%s_PRIquadru_metadata.csv", this$current.project), sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)
        printf("Quadrant percs written in %s", sprintf("%s_PRIquadru_metadata.csv", this$current.project))
        
      } else if (cutoffs[1] > 0 & cutoffs[2]) {
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        
        quadrant.percs <- cbind(current.date, displayfile, this$current.cofactor, checkCALC, colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange, cutoffs[1], cutoffs[2], "NA", round(this$q1.total, 1), round(this$q2.total, 1), round(this$q3.total, 1), round(this$q4.total, 1))
        
        write.table(quadrant.percs, sprintf("%s_PRIquadru_metadata.csv", this$current.project), sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)
        printf("Quadrant percs written in %s", sprintf("%s_PRIquadru_metadata.csv", this$current.project))
      } else {
        ### no cutoffs are set
        ### write in csv at least the absRange
        quadrant.percs <- cbind(current.date, displayfile, this$current.cofactor, checkCALC, colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange)
        
        write.table(quadrant.percs, sprintf("%s_PRIquadru_metadata.csv", this$current.project), sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)
      }
      
      
      checkLEGEND <- tclvalue(this$cbtshowLegend)
      this$step <- step
      this$label.steps <- label.steps
      space <- 0.08 * (par()$usr[4] - par()$usr[3])
      
      ### not for history png
      #if (!png) {
      if (checkLEGEND == "1") {
        ##### legend title
        if (density) legend.title <- "# cells "
        if (!bg) text(par()$usr[2] + 0.02 * (par()$usr[2] - par()$usr[1]), steps[10] + 4.7 * rect.size + this$legend.space + space, label=legend.title, cex=0.85 * set.cex, pos=2)
        
        label.pos.x <- par()$usr[2] - 0.12 * (par()$usr[2] - par()$usr[1])
        
        for (i in 1:11) {
          ## print legend rectangles
          if (i < 11) {
            rect(xleft = par()$usr[2] - 0.13 * (par()$usr[2] - par()$usr[1]), 
                 ybottom = steps[i] + space, 
                 xright = par()$usr[2] - 0.105 * (par()$usr[2] - par()$usr[1]), 
                 ytop = steps[i] + space + step, 
                 col=cols[i + 1], border=NA, pos=2)
          }
          
          if (checkDYNRANGE != "1" & (i == 1 | i == 11)) {
            displaylabel <- label.steps[i]
          } else {
            displaylabel <- sprintf("%.1f", as.numeric(label.steps[i]))
          }
          
          if (checkCALC == "RSEM" & !density) {
            text(x=label.pos.x, y=steps[i] + space, label=displaylabel, col=col.minmax, cex=0.65 * set.cex, pos=4)
          } else if (i == 1 | i == 11) {
            text(x=label.pos.x, y=steps[i] + space, label=displaylabel, col=col.minmax, cex=0.65 * set.cex, pos=4)
          } else if (i == 6 & step >= (0.65 * binSize)) {
            text(x=label.pos.x, y=steps[i] + space, label=displaylabel, col=col.minmax, cex=0.65 * set.cex, pos=4)
          } else if (step >= (1.8 * binSize)) { 
            if (i == 3) {
              display.label <- sprintf("%.1f", (as.numeric(label.steps[i]) + (step / 2)))
              text(label.pos.x, steps[i] + space + 0.5 * step, label=display.label, col=col.minmax, cex=0.65 * set.cex, pos=4)
            }
            if (i == 9) {
              display.label <- sprintf("%.1f", (as.numeric(label.steps[i]) - (step / 2)))
              text(label.pos.x, steps[i] + space - 0.5 * step, label=display.label, col=col.minmax, cex=0.65 * set.cex, pos=4)
            }
          } 
        }
      }
    }
  }

  # in mode quadruploT: if autorect was selected
  if (tclvalue(this$cbtautoRect) == "1") {
    this$addRectInfo(setcex=set.cex)
    this$addCellInfo(setcex=set.cex)
  } 
}