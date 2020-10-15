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
  checkCALC <- "freq"
  checkTRANS <- tclvalue(this$rbtrans)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkDATE <- tclvalue(this$cbtaddDate)
  checkTRIMMING <- tclvalue(this$cbttrimming)
  if (checkTRIMMING == "1") {
    this$preprocData(mode="trim")
  }
  checkDYNRANGEFREQ <- tclvalue(this$cbtdynRangeFreq)
  if (checkDYNRANGEFREQ != "1") {
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
  
  quadrants.col <- "black"
  
  v1 <- this$checkMarker(tclvalue(tkget(this$cbvar1quad)))
  tkset(this$cbvar1quad, v1)
  v2 <- this$checkMarker(tclvalue(tkget(this$cbvar2quad)))
  tkset(this$cbvar2quad, v2)
  v3 <- this$checkMarker(tclvalue(tkget(this$cbvar3quad)))
  tkset(this$cbvar3quad, v3)
  v4 <- this$checkMarker(tclvalue(tkget(this$cbvar4quad)))
  tkset(this$cbvar4quad, v4)
  vars <- c(v1, v2, v3, v4)
  
  ### if Feature A is not in sample
  if (any(length(v1) == 0 | length(v2) == 0 | length(v3) == 0 | length(v4) == 0)) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your Features.")
    stop("One or more selected features are not existent.")
  }
  
  ### if manual range for z-axis is checked but no input
  if (tclvalue(this$cbtdynRange) == "0" & tclvalue(this$vmaxfreq) == "0") {
    tkmessageBox(title = "An error has occured!", 
                 message = "You forgot to set maximum manual range for Feature C (It is still zero).", icon = "error", type = "ok")
    stop("Set maximum manual range for Features C1/C2 (It is still zero).")
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
  
  plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
      xlim = c(xminval - 0.5, xmaxval + 1),
      ylim = c(yminval - 0.5, ymaxval + 1),
      xlab = title.axis[1], ylab = title.axis[2],
      cex.lab = set.cex, cex.axis = 0.5 * set.cex.axes, mgp = set.mgp
  )
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
  if (tclvalue(fcs$rbpopC1) == "pos") {
    sign.C1 <- c(" >= ", "<")
  } else {
    sign.C1 <- c("<=", ">")
  }
  
  if (tclvalue(fcs$rbpopC2) == "pos") {
    sign.C2 <- c(" >= ", "<")
  } else {
    sign.C2 <- c("<=", ">")
  }
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

  ### Testing to get all feature z1/z2 positive cells for later display
  print("extracted all cells which are positive for feature z1 and y")
  this$q4.feature_z = tdata.q4[which(tdata.q4[, 3] >= cutoffs[3]), 3]
  this$q4.feature_y = tdata.q4[which(tdata.q4[, 2] >= cutoffs[2]), 2]
  ###

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
  secondLine <- sprintf("cells(min/max)=%s/%s; %s/%s", mincount, this$maxcells, v3, v4)
  title(main=secondLine, line=2.4, cex.main=0.9, adj=0)
  
  thirdLine <- sprintf("%s-%s(%0.1f%%); binSize=%s, #bins=%s; maxfreq=%s", ncells.total, ncells.zero, (ncells.zero / ncells.total * 100), binSize, this$bincount, this$max.freq.real)
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
    set.cex.axes <- 1.0
    set.mgp <- c(1.5, 0.3, 0)
    png <- FALSE
    overview <- FALSE
    checkCALC <- NA
    quadrants.color <- "black"
    data.origin <- NA
    file <- NA
  }
  
  prodcells.color <- "red"
  prodpluscells.color <- "chartreuse4"
  col.maxwarn <- "black"
  
  if (is.na(file)) file <- tclvalue(tkget(this$tkchoosefile))
  
  displayfile <- this$shortenFilename(file)
  
  metadatafile <- sprintf("%s_PRIquadruvis_metadata.csv", this$current.project)
  if (!file.exists(metadatafile)) {
    header <- c("date", "sample", "cofactor", "calc",
              "feat.A", "feat.B", "feat.C1", "feat.C2", 
              "min.A", "max.A", "min.B", "max.B", "maxfreq",
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
  
  ### legend title
  if (density) {
    legend.title.C1 <- "# cells"
    legend.title.C2 <- ""
  } else {
    ######## NEW: SWITCH SIGNS FOR POPULATION OF INTEREST ----------------------
    # v3 <- this$checkMarker(tclvalue(tkget(this$cbvar3quad)))
    # v4 <- this$checkMarker(tclvalue(tkget(this$cbvar4quad)))
    v3 <- colnames(data)[3]
    v4 <- colnames(data)[4]
    if (tclvalue(this$rbpopC1) == "pos") {
      sign.C1 <- c(" >= ", "<")
      legend.title.C1 <- sprintf("%s+(%s)", v3, cutoffs[3])
    } else {
      sign.C1 <- c("<=", ">")
      legend.title.C1 <- sprintf("%s-(%s)", v3, cutoffs[3])
    }
    if (tclvalue(this$rbpopC2) == "pos") {
      sign.C2 <- c(" >= ", "<")
      legend.title.C2 <- sprintf("%s+(%s)", v4, cutoffs[4])
    } else {
      sign.C2 <- c("<=", ">")
      legend.title.C2 <- sprintf("%s-(%s)", v4, cutoffs[4])
    }
  }
  

  # checkbutton options
  checkDYNRANGEFREQ <- tclvalue(this$cbtdynRangeFreq)
  checkTRANS <- tclvalue(this$rbtrans)
  if (checkTRANS  == "") checkTRANS <- tclvalue(this$rbtrans) <- "asinh"
  checkCALC <- "freq"
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
  
  # legend colors from blue to red
  if (bg) {
    cols <- rep("gray", 12)
  } else {
    cols <- this$col.green
  }
  
  ### set negative values of z-axis (colnum=3) to zero
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
    colnames(my.calc)[2] <- "freq.C1"
    # bin color factor Z1
    my.calc.fac.C1 <- cut(my.calc$freq.C1, breaks=seq(0, 100, by=10), labels=1:10, include.lowest=TRUE)

    ### frequency of feature Z2        
    eval(parse(text=paste("freq.C2 = aggregate(tdata[, 4], by=list(fXY), 
          function(x) {
            y = round(100 * length(which(x", sign.C2[1], " cutoffs[4])) / length(x))
            return(y)
          })", sep="")))
    # bin color factor Z2
    my.calc.fac.C2 <- cut(freq.C2$x, breaks=seq(0, 100, by=10), labels=1:10, include.lowest=TRUE)

    ### get only data table where Z1 and Z2 is produced
    eval(parse(text=paste("tdata.double = tdata[ which((tdata[, 3]", sign.C1[1], " cutoffs[3]) ", "& (tdata[, 4]", sign.C2[1], " cutoffs[4])), ]", sep="")))
    ### construct bin table with number of cells per bin
    fX.double <- cut(tdata.double[, 1], breaks=seq(xmin.val, xmax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
    fY.double <- cut(tdata.double[, 2], breaks=seq(ymin.val, ymax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
    fXY.double <- as.factor(paste(fX.double, fY.double))

    ### count cells of double frequencies C1 + C2 in each bin
    length.double <- aggregate(tdata.double[, 3], by=list(fXY.double), length)
    rownames(length.double) <- length.double$Group.1

    ### count total cells in each bin
    length.all <- aggregate(tdata[, 4], by=list(fXY), length)
    rownames(length.all) <- length.all$Group.1

    ### merge and calculate cells of double frequencies to total
    freq.double <- merge(length.all, length.double, by="row.names", all.x=TRUE)
    freq.double <- freq.double[, -c(2, 4)]
    freq.double <- cbind(freq.double, round(freq.double[, 3] / freq.double[, 2] * 100))
    
    ### bin color factor double producer C1 + C2
    my.calc.fac.double <- cut(freq.double[, 4], breaks = seq(0, max.freq, by = max.freq / 10), labels = 1:10, include.lowest = TRUE)
    ### set factor to 10 if real max frequency is higher than set and cells of double frequencies are higher than mincells
    my.calc.fac.double[which(freq.double[, 4] >= max.freq & freq.double[, 3] >= mincells)] <- 10

    ### combine all frequencies in one table
    my.calc <- cbind(my.calc, fac.C1=as.numeric(as.character(my.calc.fac.C1)))
    my.calc <- cbind(my.calc, freq.C2=freq.C2$x)
    my.calc <- cbind(my.calc, fac.C2=as.numeric(as.character(my.calc.fac.C2)))
    my.calc <- cbind(my.calc, freq.double=freq.double[, 4])
    my.calc <- cbind(my.calc, fac.double=as.numeric(as.character(my.calc.fac.double)))
    my.calc <- cbind(my.calc, ncells=length.all$x)
    my.calc <- cbind(my.calc, ncells.double = freq.double[, 3])

    this$max.freq.real <- max(my.calc$freq.double[which(!is.na(my.calc$freq.double) & my.calc$ncells >= mincells)])
    printf("MAXIMUM FREQUENCY = %s", this$max.freq.real)
    max.freq <- as.double(tclvalue(this$vmaxfreq))
    if (checkDYNRANGEFREQ == "1") {
      this$vmaxfreq <- tclVar(this$max.freq.real)
      tkconfigure(this$maxfreq, textvariable = this$vmaxfreq)

      ### recalculate the bin factors
      max.freq <- this$max.freq.real
      # bin color factor double producer Z1 + Z2
      my.calc.fac.double <- cut(freq.double[, 4], breaks=seq(0, max.freq, by=max.freq / 10), labels=1:10, include.lowest=TRUE)
      my.calc$fac.double <- as.numeric(as.character(my.calc.fac.double))
    } else if (checkDYNRANGEFREQ == "0" & (this$max.freq.real > max.freq)) {
      print("!!!!! WARNING: MAXFREQ IS SET LOWER THAN IT COULD !!!!!")
      
      ### set factors of frequencies higher than set to 10
      my.calc$fac.double[which(my.calc$freq.double >= max.freq)] <- 10
      col.maxwarn <- "red"
    }
    
    this$my.calc.fac.C1 <- my.calc.fac.C1
    this$my.calc.fac.C2 <- my.calc.fac.C2
    this$my.calc.fac.double <- my.calc.fac.double
  }
  
  ### if there are bins to display
  if (any(my.calc$ncells >= mincells)) {
    col.minmax <- "black"
    
    rect.step <- round(diff(c(par()$usr[3], par()$usr[4])) / 37, 2)
    min.legend.y <- par()$usr[3] + 8 * rect.step
    max.legend.y <- par()$usr[3] + 18 * rect.step
    
    ### get rect steps
    rect.steps <- seq(min.legend.y, max.legend.y, by = rect.step)

    if (density) {
      idx <- which(my.calc$ncells >= mincells)
      min.range <- min(my.calc$ncells[idx])
      max.range <- max(my.calc$ncells[idx])

      if (max.range < 200) {
        col.minmax <- col.maxwarn <- "red"
      }

      # bin color factor
      my.calc.fac <- cut(my.calc$x, breaks = seq(min.range, max.range, length.out = 11), labels = 1:10, include.lowest = TRUE)
    } else {
      ### get steps for legend and plot in mode "freq"
      min.range <- min.freq
      max.range <- max.freq
      
      # bin color factor
      my.calc.fac <- cut(my.calc$freq.double, breaks = seq(min.range, max.range, length.out = 11), labels = 1:10, include.lowest = TRUE)
      # my.calc.fac <- cut(my.calc$freq.double, breaks=seq(min.freq, max.freq, length.out = 11), labels=1:10, include.lowest=TRUE)
      # levels(my.calc.fac) <- c(0, levels(my.calc.fac), 11, 12)
    }
    my.calc <- cbind(my.calc, fac=as.numeric(as.character(my.calc.fac)))
    this$my.calc <- my.calc


    ### get labels for rect steps
    label.steps <- range.steps <- seq(min.range, max.range, length.out = 11)
    range.step <- diff(c(range.steps[1], range.steps[2]))
    if (density | checkTRANS == "biex") {
      if (max.range > 500) {
        label.steps <- round(label.steps, -2)
      } else {
        label.steps <- round(label.steps)
      }
      if (density) label.steps[1] <- min.range
    } else if (checkDYNRANGEFREQ != "1") {
      if (label.steps[1] != "0") {
        label.steps[1] <- sprintf("<=%s", label.steps[1])
      }
      if (label.steps[11] != "100") {
        label.steps[11] <- sprintf(">=%s", label.steps[11])
      }
    }

    ##### plot bins
    this$bincount <- 0
    this$maxcells <- 0
    cols.heat <- cols[, as.numeric(as.character(my.calc.fac.double))]
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

          if (length(cols.heat[, idx]) != 0) {
            if (!is.na(cols.heat[1, idx])) {
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
    }
    
    ### add production line
    this$addProdline(cutoffs)

    ###### legend plot + label ----------------------------------------------------
    
    ### q1: quadrant left lower black ink
    text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), 
    y = par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), 
    label = sprintf("%0.1f%%", this$q1.total), col = quadrants.color, cex = 1.00 * set.cex, pos = 4, xpd = TRUE)
    ### q2: quadrant right lower black ink
      text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
    y = par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]),
    label = sprintf("%0.1f%%", this$q2.total), col = quadrants.color, cex = 1.00 * set.cex, pos = 2, xpd = TRUE)
    ### q3 quadrant right upper black ink
      text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
    y = par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]),
    label=sprintf("%0.1f%%", this$q3.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    ### q4 quadrant left upper black ink
      text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]),
    y = par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]),
    label = sprintf("%0.1f%%", this$q4.total), col = quadrants.color, cex = 1.00 * set.cex, pos = 4, xpd = TRUE)
    
    ### Testing added median of feature z and y
      text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]) + 2.5,
    y = par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]),
    label = paste0("z: ", round(median(this$q4.feature_z), 2)), col = "blue", cex = 1.00 * set.cex, pos = 4, xpd = TRUE)
     
     text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]) + 2.5,
    y = par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]),
    label = paste0("y: ", round(median(this$q4.feature_y), 2)), col = "blue", cex = 1.00 * set.cex, pos = 4, xpd = TRUE)

    ### q1: quadrant left lower red and green ink
    text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[3] + 0.08 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q1.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
    text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[3] + 0.13 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q1.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)

    ### q2: quadrant right lower red and green ink
    text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[3] + 0.08 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q2.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[3] + 0.13 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q2.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)

    ### q3 quadrant right upper red and green ink
    text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q3.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[4] - 0.14 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q3.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        
    ### q4 quadrant left upper red and green ink
    text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]),
        y = par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]),
        label=sprintf("%0.1f%%", this$q4.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
    text(x = par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), 
        y = par()$usr[4] - 0.14 * (par()$usr[4] - par()$usr[3]), 
        label=sprintf("%0.1f%%", this$q4.prodcellsplus), col=prodpluscells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)

    ### save meta table values -------------------------------------------
    csv.content <- c(current.date, displayfile,
      this$current.cofactor, checkCALC,
      colnames(data)[1], colnames(data)[2], colnames(data)[3], colnames(data)[4],
      xmin.val, xmax.val, ymin.val, ymax.val, max.freq,
      cutoffs[1], cutoffs[2], cutoffs[3], cutoffs[4],
      round(this$q1.total, 1), round(this$q2.total, 1), round(this$q3.total, 1), round(this$q4.total, 1),
      round(this$q1.prodcells, 1), round(this$q2.prodcells, 1), round(this$q3.prodcells, 1), round(this$q4.prodcells, 1),
      round(this$q1.prodcellsplus, 1), round(this$q2.prodcellsplus, 1), round(this$q3.prodcellsplus, 1), round(this$q4.prodcellsplus, 1))
    ###

    ### write meta table values -------------------------------------------
    write.table(csv.content, metadatafile, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)
    printf("Quadrant percs written in %s", metadatafile)
    
    this$rect.step <- rect.step
    this$rect.steps <- rect.steps
    this$label.steps <- label.steps

    checkLEGEND <- tclvalue(this$cbtshowLegend)
      
    if (checkLEGEND == "1") {
      ##### legend title C1
      text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
          y = rect.steps[10] + 3.0 * rect.step,
          label = legend.title.C1, cex = 0.9 * set.cex, pos = 2
      )
      ##### legend title C2
      text(x = par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]),
          y = rect.steps[10] + 4.8 * rect.step,
          label = legend.title.C2, cex = 0.9 * set.cex, pos = 2
      )
      
      label.pos.x <- par()$usr[2] - 0.12 * (par()$usr[2] - par()$usr[1])
      for (i in 1:11) {
        ## print legend rectangles
        if (i < 11) {
          rect(xleft = par()$usr[2] - 0.13 * (par()$usr[2] - par()$usr[1]),
              ybottom = rect.steps[i],
              xright = par()$usr[2] - 0.105 * (par()$usr[2] - par()$usr[1]),
              ytop = rect.steps[i] + rect.step,
              col = eval(parse(text = paste0("rgb(", paste0(cols[, i], collapse = ", "), ", maxColorValue=255)"))),
              border = NA, pos = 2)
        }

        if (i == 1) {
          text(x = label.pos.x, y = rect.steps[i], label = label.steps[i], col = col.minmax, cex = 0.9 * set.cex, pos = 4)
        } else if (i == 11) {
          text(x = label.pos.x, y = rect.steps[i], label = label.steps[i], col = col.maxwarn, cex = 0.9 * set.cex, pos = 4)
        } else if (i == 6 & rect.step >= 0.3) {
          text(x=label.pos.x, y=rect.steps[i], label=label.steps[i], col=col.minmax, cex=0.9 * set.cex, pos=4)
        } else if (rect.step >= 0.2) { 
          if (i == 3) {
            display.label <- sprintf("%.0f", (range.steps[i] + (range.step / 2)))
            text(label.pos.x, rect.steps[i] + 0.5 * rect.step, label=display.label, col=col.minmax, cex=0.9 * set.cex, pos=4)
          }
          if (i == 9) {
            display.label <- sprintf("%.0f", (range.steps[i] - (range.step / 2)))
            text(label.pos.x, rect.steps[i] - 0.5 * rect.step, label=display.label, col=col.minmax, cex=0.9 * set.cex, pos=4)
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