#! / usr / bin / R
# Author: Yen Hoang
# DRFZ 2015 - 2020


### triploT functions -----------------------------------------------------
fcs$dotriploT <- function() {
  # calculate and display plot
  this <- fcs
  printf("w: do dotriploT")
  
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
  
  
  
  quadrants.col <- "black"
  
  v1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
  tkset(this$cbvar1, v1)
  v2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
  tkset(this$cbvar2, v2)
  v3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))
  tkset(this$cbvar3, v3)
  vars <- c(v1, v2, v3)
  
  ### if Feature A is not in sample
  if (length(v1) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your Feature A.")
    stop("Feature A is not existent.")
  }
  ### if Feature A is not in sample
  if (length(v2) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your feature B.")
    stop("Feature B is not existent.")
  }
  ### if Feature A is not in sample
  if (length(v3) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your feature C.")
    stop("Feature C is not existent.")
  }
  
  ### if manual range for z-axis is checked but no input
  if (tclvalue(this$cbtdynRange) == "0" & tclvalue(this$vmaxMSI) == "0") {
    tkmessageBox(title = "An error has occured!", 
                 message = "You forgot to set maximum manual range for Feature C (It is still zero).", icon = "error", type = "ok")
    stop("Set maximum manual range for Feature C (It is still zero).")
  }
  
  cutoffz <- this$checkDigits(cutoff_id=which(this$selected.vars == v3))
  
  ### if method is freq or MSI(+), cutoff(z) needs to be setted
  if (cutoffz <= 0 & (checkCALC == "freq" | checkCALC == "MSI(+)")) {
    tkmessageBox(title = "An error has occured in mode: MSI(+) or freq!", 
                 message = "You forgot to set cutoff for Feature C.", icon = "error", type = "ok")
    stop("Missing production cutoff for Feature C.")
  }
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
  if (length(which(dev.label == names(devList()))) == 0) {
    this$plotter.tri.num <- this$plotter.tri.num + 1
    dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
    ncol <- as.numeric(tclvalue(this$vncol))
    nrow <- as.numeric(tclvalue(this$vnrow))
    devNew(type="x11", title="n-triploTs", width=ncol * 3.4, height=nrow * 3.7, label=dev.label)
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
  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  
  tkconfigure(this$tt, cursor = "watch")
  if ((checkGATED == "1") & (this$temp.num > 0)) {
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
  
  cutoff_idx <- which(this$selected.vars == v1)
  cutoff_idy <- which(this$selected.vars == v2)
  cutoff_idz <- which(this$selected.vars == v3)
  cutoffs <- c(cutoff_idx, cutoff_idy, cutoff_idz)
  
  ### if percentage is checked
  # calculate cutoffs and set check button to zero
  for (i in 1:length(cutoffs)) {
    if (tclvalue(this$cbcutoffperc[[cutoffs[i]]]) == "1") {
      cutoffs[i] <- this$calcCUTOFF(tdata[vars[i]], this$checkDigits(cutoff_id=cutoffs[i]), vars[i], cutoffs[i])
    } else {
      cutoffs[i] <- this$checkDigits(cutoff_id=cutoffs[i])
    }
  }
  printf("w: do dotriploT :: file: %s", file)
  printf("w: do dotriploT :: table: %s", table)
  printf("w: do dotriploT :: file.idx=%s", file.idx)
  printf("w: do dotriploT :: cutoffs=%s", paste(cutoffs, collapse=" "))
  
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
    cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
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
  ### cut all cells which are not producing cells
  if (checkCALC == "MSI(+)") {
    tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
  }
  
  # q1 Quadrant unten links
  # q2 Quadrant unten rechts
  # q3 Quadrant oben rechts
  # q4 Quadrant oben links
  if (cutoffs[1] > 0 & cutoffs[2] > 0) {
    
    ### count cells in quadrant
    tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
    tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
    tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
    tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
    
    ### q[x].total [ink=black]
    ### percentage of cells in quadrant to total cells 
    ### or in MSI(+): percentage of cells in quadrant to total positive cells
    this$q1.total <- abs(100 * length(tdata.q1) / ncells)
    this$q2.total <- abs(100 * length(tdata.q2) / ncells)
    this$q3.total <- abs(100 * length(tdata.q3) / ncells)
    this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
    
    if (cutoffs[3] > 0) {
      ### number of cells which are producing cells in feature C
      ncells <- nrow(tdata[which(tdata[, 3] > cutoffs[3]), ])
      
      ### q[x].prodcells [ink=red]
      ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
      this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / length(tdata.q1)
      if (is.nan(this$q1.prodcells)) {
        this$q1.prodcells <- 0
      }
      
      this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / length(tdata.q2)
      if (is.nan(this$q2.prodcells)) {
        this$q2.prodcells <- 0
      }
      
      this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / length(tdata.q3)
      if (is.nan(this$q3.prodcells)) {
        this$q3.prodcells <- 0
      }

      this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / length(tdata.q4)
      if (is.nan(this$q4.prodcells)) {
        this$q4.prodcells <- 0
      }
      
      ### only do MSI plots on producing cells only
      if (checkCALC == "MSI(+)") {
        ncells <- nrow(tdata.plus)
        
        tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
        tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
        tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
        tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
        
        ### q[x].total [ink=blue]
        ### in MSI(+): percentage of cells in quadrant to total positive cells
        this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)

        this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)

        this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
        
        this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
      }
      
      ### q[x].prodcellsplus [ink=green]
      ### percentage of cells which are positive for feature C to total cells
      this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / ncells.total

      if (is.nan(this$q1.prodcellsplus)) {
        this$q1.prodcellsplus <- 0
      }

      this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / ncells.total
      if (is.nan(this$q2.prodcellsplus)) {
        this$q2.prodcellsplus <- 0
      }
     
      this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / ncells.total
      if (is.nan(this$q3.prodcellsplus)) {
        this$q3.prodcellsplus <- 0
      }

      this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / ncells.total
      if (is.nan(this$q4.prodcellsplus)) {
        this$q4.prodcellsplus <- 0
      }
      
      if (this$working) {
        print("w: ncells ::  q1  q2  q3  q4")
        printf("w: %s(total) :: %s %s %s %s", ncells.total, length(tdata.q1), length(tdata.q2), length(tdata.q3), length(tdata.q4))
        printf("w: %s(black / blue) :: %.1f %.1f %.1f %.1f", ncells, this$q1.total, this$q2.total, this$q3.total, this$q4.total)
        printf("w: prodcells(red) :: %.1f %.1f %.1f %.1f", this$q1.prodcells, this$q2.prodcells, this$q3.prodcells, this$q4.prodcells)
        printf("w: prodcellsplus(green) :: %.1f %.1f %.1f %.1f", this$q1.prodcellsplus, this$q2.prodcellsplus, this$q3.prodcellsplus, this$q4.prodcells)
      }
    }
  } 
  
  if (checkGATED != "1") {
    this$origin.ncells <- ncells
    this$coords <- list()
  }
  
  if (checkCALC == "density") {
    this$bintriplot(data=tdata, cutoffs=cutoffs, density=TRUE, binSize=binSize, mincells=mincount)
  } else if (checkCALC == "MSI(+)") {
    this$bintriplot(data=tdata.plus, cutoffs=cutoffs, binSize=binSize, mincells=mincount, quadrants.color = "blue", data.origin=tdata)
  } else {
    this$bintriplot(data=tdata, cutoffs=cutoffs, binSize=binSize, mincells=mincount)
  }
  
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

fcs$dotriploTfiles <- function(read=FALSE) {
  # calculate and display plot
  this <- fcs
  printf("w: do dotriploT for all files")
  
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
  
  
  
  quadrants.col <- "black"
  
  v1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
  tkset(this$cbvar1, v1)
  v2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
  tkset(this$cbvar2, v2)
  v3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))
  tkset(this$cbvar3, v3)
  vars <- c(v1, v2, v3)
  
  ### if Feature A is not in sample
  if (length(v1) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your Feature A.")
    stop("Feature A is not existent.")
  }
  ### if Feature A is not in sample
  if (length(v2) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your feature B.")
    stop("Feature B is not existent.")
  }
  ### if Feature A is not in sample
  if (length(v3) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your feature C.")
    stop("Feature C is not existent.")
  }
  
  ### if manual range for z-axis is checked but no input
  if (tclvalue(this$cbtdynRange) == "0" & tclvalue(this$vmaxMSI) == "0") {
    tkmessageBox(title = "An error has occured!", 
                 message = "You forgot to set maximum manual range for Feature C (It is still zero).", icon = "error", type = "ok")
    stop("Set maximum manual range for Feature C (It is still zero).")
  }
  
  cutoffz <- this$checkDigits(cutoff_id=which(this$selected.vars == v3))
  
  ### if method is freq or MSI(+), cutoff(z) needs to be set
  if (cutoffz <= 0 & (checkCALC == "freq" | checkCALC == "MSI(+)")) {
    tkmessageBox(title = "An error has occured in mode: MSI(+) or freq!", 
                 message = "You forgot to set cutoff for Feature C.", icon = "error", type = "ok")
    stop("Missing production cutoff for Feature C.")
  }
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  this$rect.lwd <- 1
  
  empty <- FALSE
  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  tkconfigure(this$tt, cursor = "watch")
  timeSTART <- Sys.time()
  

  max.nhorizplots <- as.numeric(tclvalue(this$vncol))
  max.nvertiplots <- as.numeric(tclvalue(this$vnrow))
  file.idx.vec <- 1:length(this$current.filenames)
  pdf.file <- sprintf("%s/%s_%s_%s_%s_%s_triploTs_%s.pdf", getwd(), this$current.project, v1, v2, v3, checkCALC, this$version)

  timeSTART <- Sys.time()
  cat("\n\n>>>> Start triploTOverviewXY with total data files: \n\n")
  if (this$working) printf("w: %s - time started", timeSTART)
  
  toPDF(file=pdf.file, 
        path=this$saveinFolder, 
        title=sprintf("project %s - triploTs of %s(%s.%s.%s)", this$current.project, checkCALC, v1, v2, v3), 
        ### 
        width=3.21 * max.nhorizplots, 
        height=3.5 * max.nvertiplots, 
        pointsize=11, {
          label.cex <- 1.1 - 0.5 * this$legend.space
          set.cex.axes <- 1
          set.mgp <- c(1.9, 0.5, 0)
          par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 6, 1), mar=c(3, 4, 5, 1))
          
          for (file.idx in file.idx.vec) {
            
            table <- this$selected.project
            displayfile <- this$shortenFilename(this$current.filenames[file.idx])
            this$coords.info <- vector()
            
            #### new getFile 
            this$getFile(table, file.idx)
            ####
            tdata <- this$data[vars]
            this$tdata <- tdata
            
            
            cutoff_idx <- which(this$selected.vars == v1)
            cutoff_idy <- which(this$selected.vars == v2)
            cutoff_idz <- which(this$selected.vars == v3)
            cutoffs <- c(cutoff_idx, cutoff_idy, cutoff_idz)
            
            ### if percentage is checked
            # calculate cutoffs and set check button to zero
            for (i in 1:length(cutoffs)) {
              if (tclvalue(this$cbcutoffperc[[cutoffs[i]]]) == "1") {
                cutoffs[i] <- this$calcCUTOFF(tdata[vars[i]], this$checkDigits(cutoff_id=cutoffs[i]), vars[i], cutoffs[i])
              } else {
                cutoffs[i] <- this$checkDigits(cutoff_id=cutoffs[i])
              }
            }
            printf("w: do dotriploT :: file: %s", displayfile)
            printf("w: do dotriploT :: table: %s", table)
            printf("w: do dotriploT :: file.idx=%s", file.idx)
            printf("w: do dotriploT :: cutoffs=%s", paste(cutoffs, collapse=" "))
            printf("w: do dotriploT :: vars=%s", paste(colnames(this$tdata), collapse=" "))
            
            ### calculate cells which were not plotted 
            cells.overmaxFI <- length(which(tdata[, 1] > xmaxval | tdata[, 2] > ymaxval))
            cells.underminFI <- length(which(tdata[, 1] < xminval | tdata[, 2] < yminval))
            cells.overmaxFI.perc <- round(100 * cells.overmaxFI / (dim(tdata)[1] - cells.underminFI))
            ### warn if more then 5% productive cells (q2 + q3 + q4) werent plotted
            if (cells.overmaxFI.perc >= 5 & !this$working) {
              tkmessageBox(title = "Warning!", 
                           message = sprintf("Your cells exceed %s%% of your plot max ranges. You might want to increase your max ranges.", cells.overmaxFI.perc), 
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
            if (cutoffs[1] > 0) title.axis <- sprintf("%s (%s)", v1, cutoffs[1])
            else title.axis <- v1
            if (cutoffs[2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", v2, cutoffs[2]))
            else title.axis <- c(title.axis, v2)
            
            plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
              xlim = c(xminval - 0.5, xmaxval + 1),
              ylim = c(yminval - 0.5, ymaxval + 1),
              xlab = title.axis[1], ylab = title.axis[2],
              cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
            box(lwd=0.8, col="darkgrey")
            
            ### draw axis on the bottom and on the left
            axis(side=1, at=scale, labels=label, las=1, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
            axis(side=2, at=scale, labels=label, las=3, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
            
            ### add grid
            if (checkGRID == "1") {
              xgrid.steps  <- seq(0, (xmaxval), by=grid.step)
              ygrid.steps  <- seq(0, (ymaxval), by=grid.step)
              abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
            }
            
            
            ### calc quadrants in total
            ncells <- ncells.total <- nrow(tdata)
            
            ### OLD tdata.zero where all rows with negative values where filtered
            #tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
            ### NEW tdata.zero where ONLY rows where filtered if v1 or v2 are negative
            tdata.zero <- tdata[which(tdata[, 1] >= 0 | tdata[, 2] >= 0), ]
            ncells.zero <- nrow(tdata.zero)
            # q1 Quadrant unten links
            # q2 Quadrant unten rechts
            # q3 Quadrant oben rechts
            # q4 Quadrant oben links
            if (cutoffs[1] > 0 & cutoffs[2] > 0) {
              
              ### count cells in quadrant
              tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
              tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
              tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
              tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
              
              ### q[x].total [ink=black]
              ### percentage of cells in quadrant to total cells 
              ### or in MSI(+): percentage of cells in quadrant to total positive cells
              this$q1.total <- abs(100 * length(tdata.q1) / ncells)
              this$q2.total <- abs(100 * length(tdata.q2) / ncells)
              this$q3.total <- abs(100 * length(tdata.q3) / ncells)
              this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
              
              if (cutoffs[3] > 0) {
                ### number of cells which are producing cells in feature Z
                ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[3]), ])
                
                ### q[x].prodcells [ink=red]
                ### percentage of cells which are positive for feature Z in quadrant to total quadrant cells
                this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / length(tdata.q1)
                if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / length(tdata.q2)
                if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / length(tdata.q3)
                if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / length(tdata.q4)
                if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                
                ### only do MSI plots on producing cells only
                if (checkCALC == "MSI(+)") {
                  ### cut all cells which are not producing cells
                  tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                  ncells <- nrow(tdata.plus)
                  
                  tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                  tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                  tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                  tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                  
                  ### q[x].total [ink=blue]
                  ### in MSI(+): percentage of cells in quadrant to total positive cells
                  this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                  this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                  this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                  this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                }
                
                ### q[x].prodcellsplus [ink=green]
                ### percentage of cells which are positive for feature Z to total cells
                this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / ncells.total
                if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / ncells.total
                if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / ncells.total
                if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / ncells.total
                if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                
                if (this$working) {
                  print("w: ncells ::  q1  q2  q3  q4")
                  printf("w: %s(total) :: %s %s %s %s", ncells.total, length(tdata.q1), length(tdata.q2), length(tdata.q3), length(tdata.q4))
                  printf("w: %s(black / blue) :: %.1f %.1f %.1f %.1f", ncells, this$q1.total, this$q2.total, this$q3.total, this$q4.total)
                  printf("w: prodcells(red) :: %.1f %.1f %.1f %.1f", this$q1.prodcells, this$q2.prodcells, this$q3.prodcells, this$q4.prodcells)
                  printf("w: prodcellsplus(green) :: %.1f %.1f %.1f %.1f", this$q1.prodcellsplus, this$q2.prodcellsplus, this$q3.prodcellsplus, this$q4.prodcells)
                }
              }
            } 
            
            
            if (checkGATED != "1") {
            this$origin.ncells <- ncells
            this$coords <- list()
                }
            
            if (checkCALC == "density") {
              this$bintriplot(data=tdata, cutoffs=cutoffs, density=TRUE, binSize=binSize, mincells=mincount, file=displayfile)
            } else if (checkCALC == "MSI(+)") {
              this$bintriplot(data=tdata.plus, cutoffs=cutoffs, binSize=binSize, mincells=mincount, quadrants.color = "blue", data.origin=tdata, file=displayfile)
            } else {
              this$bintriplot(data=tdata, cutoffs=cutoffs, binSize=binSize, mincells=mincount, file=displayfile)
            }
            
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
              firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
            } else {
              firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
            }
            title(main=firstLine, line=3.2, cex.main=0.9, adj=0)
            
            if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
              secondLine <- sprintf("cells(min/max)=%s/%s;%s", mincount, this$maxcells, v3)
            }  else {
              secondLine <- sprintf("cells(min/max)=%s/%s", mincount, this$maxcells)
            }
            title(main=secondLine, line=2.4, cex.main=0.9, adj=0)
            
            thirdLine <- sprintf("%s-%s(%0.1f%%); binSize=%s, #bins=%s", ncells.total, ncells.zero, (ncells.zero / ncells.total * 100), binSize, this$bincount)
            title(main=thirdLine, line=1.6, cex.main=0.7, adj=0)
          }
        })
  
  ### save FI ranges for this transformation type
  this$changeFI.range(mode=3)
  
  ############# stop 
  if (this$working) print(Sys.time() - timeSTART)
  
  printf("Saved in %s.", pdf.file)
  
  tkconfigure(this$tt, cursor = "left_ptr")
  
  if (checkDATE == "1") {
    date <- gsub("-", "", Sys.Date())
    title(main=date, outer=TRUE, line=1, cex.main=1.3, adj=1)
  }
  
}

fcs$dotriploTtable <- function() {
  # calculate and display plot
  this <- fcs
  printf("w: do dotriploT for all files first 10 triploTs")
  
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
  
  quadrants.col <- "black"
  
  
  this$rect.lwd <- 1
  
  ### if manual range for z-axis is checked but no input
  if (tclvalue(this$cbtdynRange) == "0" & tclvalue(this$vmaxMSI) == "0") {
    tkmessageBox(title = "An error has occured!", 
                 message = "You forgot to set maximum manual range for Feature C (It is still zero).", icon = "error", type = "ok")
    stop("Set maximum manual range for Feature C (It is still zero).")
  }
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  empty <- FALSE
  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  tkconfigure(this$tt, cursor = "watch")
  timeSTART <- Sys.time()
  
  
  # ask for triplot values ---------------------------------------------------
  printf("w: do open triplot table")
  file <- tclvalue(tkgetOpenFile(initialdir=this$table.dir, defaultextension="csv"))
  triplot.table <- read.table(file, fill=TRUE, header=TRUE, sep="\t")
  
  
  
  
  
  for (i in 1:10) {
    v1 <- this$checkMarker(as.character(triplot.table$X[i]))
    v2 <- this$checkMarker(as.character(triplot.table$Y[i]))
    v3 <- this$checkMarker(as.character(triplot.table$Z[i]))
    vars <- c(v1, v2, v3)
    
    triplot.q <- as.character(triplot.table$Q[i])
    

    max.nhorizplots <- as.numeric(tclvalue(this$vncol))
    max.nvertiplots <- as.numeric(tclvalue(this$vnrow))
    file.idx.vec <- 1:length(this$current.filenames)
    pdf.file <- sprintf("%s/%s_%s_%s_%s_%s_triploTs_%s.pdf", getwd(), this$current.project, v1, v2, v3, checkCALC, this$version)
    tissue <- ""
    
    
    timeSTART <- Sys.time()
    if (this$working) printf("w: %s - time started", timeSTART)
    
    toPDF(file=pdf.file, 
          path=this$saveinFolder, 
          title=sprintf("%s - triploTs of %s(%s.%s.%s) %s", tissue, checkCALC, v1, v2, v3, triplot.q), 
          ### 
          width=3.21 * max.nhorizplots, 
          height=3.5 * max.nvertiplots, 
          pointsize=11, {
            label.cex <- 1.1 - 0.5 * this$legend.space
            set.cex.axes <- 1
            set.mgp <- c(1.9, 0.5, 0)
            par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 6, 1), mar=c(3, 4, 5, 1))
            
            for (file.idx in file.idx.vec) {
              
              table <- this$selected.project
              displayfile <- this$shortenFilename(this$current.filenames[file.idx])
              this$coords.info <- vector()
              
              #### new getFile 
              this$getFile(table, file.idx)
              ####
              tdata <- this$data[vars]
              printf(">>>>>>%s", colnames(tdata))
              this$tdata <- tdata
              
              
              cutoff_idx <- which(this$selected.vars == v1)
              cutoff_idy <- which(this$selected.vars == v2)
              cutoff_idz <- which(this$selected.vars == v3)
              cutoffs <- c(cutoff_idx, cutoff_idy, cutoff_idz)
              
              ### if percentage is checked
              # calculate cutoffs and set check button to zero
              for (i in 1:length(cutoffs)) {
                if (tclvalue(this$cbcutoffperc[[cutoffs[i]]]) == "1") {
                  cutoffs[i] <- this$calcCUTOFF(tdata[vars[i]], this$checkDigits(cutoff_id=cutoffs[i]), vars[i], cutoffs[i])
                } else {
                  cutoffs[i] <- this$checkDigits(cutoff_id=cutoffs[i])
                }
              }
              printf("w: do dotriploT :: file: %s", displayfile)
              printf("w: do dotriploT :: table: %s", table)
              printf("w: do dotriploT :: file.idx=%s", file.idx)
              printf("w: do dotriploT :: cutoffs=%s", paste(cutoffs, collapse=" "))
              printf("w: do dotriploT :: vars=%s", paste(colnames(this$tdata), collapse=" "))
              
              ### calculate cells which were not plotted 
              cells.overmaxFI <- length(which(tdata[, 1] > xmaxval | tdata[, 2] > ymaxval))
              cells.underminFI <- length(which(tdata[, 1] < xminval | tdata[, 2] < yminval))
              cells.overmaxFI.perc <- round(100 * cells.overmaxFI / (dim(tdata)[1] - cells.underminFI))
              ### warn if more then 5% productive cells (q2 + q3 + q4) werent plotted
              if (cells.overmaxFI.perc >= 5 & !this$working) {
                tkmessageBox(title = "Warning!", 
                             message = sprintf("Your cells exceed %s%% of your plot max ranges. You might want to increase your max ranges.", cells.overmaxFI.perc), 
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
              if (cutoffs[1] > 0) title.axis <- sprintf("%s (%s)", v1, cutoffs[1])
              else title.axis <- v1
              if (cutoffs[2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", v2, cutoffs[2]))
              else title.axis <- c(title.axis, v2)
              
              plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                xlim = c(xminval - 0.5, xmaxval + 1),
                ylim = c(yminval - 0.5, ymaxval + 1),
                xlab = title.axis[1], ylab = title.axis[2],
                cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
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
              ### NEW tdata.zero where ONLY rows where filtered if v1 or v2 are negative
              tdata.zero <- tdata[which(tdata[, 1] >= 0 | tdata[, 2] >= 0), ]
              ncells.zero <- nrow(tdata.zero)
              # q1 Quadrant unten links
              # q2 Quadrant unten rechts
              # q3 Quadrant oben rechts
              # q4 Quadrant oben links
              if (cutoffs[1] > 0 & cutoffs[2] > 0) {
                
                ### count cells in quadrant
                tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
                tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
                tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
                tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
                
                ### q[x].total [ink=black]
                ### percentage of cells in quadrant to total cells 
                ### or in MSI(+): percentage of cells in quadrant to total positive cells
                this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                
                if (cutoffs[3] > 0) {
                  ### number of cells which are producing cells in feature C
                  ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[3]), ])
                  
                  ### q[x].prodcells [ink=red]
                  ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                  this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / length(tdata.q1)
                  if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                  this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / length(tdata.q2)
                  if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                  this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / length(tdata.q3)
                  if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                  this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / length(tdata.q4)
                  if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                  
                  ### only do MSI plots on producing cells only
                  if (checkCALC == "MSI(+)") {
                    ### cut all cells which are not producing cells
                    tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                    ncells <- nrow(tdata.plus)
                    
                    tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                    tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                    tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                    tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                    
                    ### q[x].total [ink=blue]
                    ### in MSI(+): percentage of cells in quadrant to total positive cells
                    this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                    this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                    this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                    this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                  }
                  
                  ### q[x].prodcellsplus [ink=green]
                  ### percentage of cells which are positive for feature C to total cells
                  this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / ncells.total
                  if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                  this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / ncells.total
                  if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                  this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / ncells.total
                  if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                  this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / ncells.total
                  if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                  
                  if (this$working) {
                    print("w: ncells ::  q1  q2  q3  q4")
                    printf("w: %s(total) :: %s %s %s %s", ncells.total, length(tdata.q1), length(tdata.q2), length(tdata.q3), length(tdata.q4))
                    printf("w: %s(black / blue) :: %.1f %.1f %.1f %.1f", ncells, this$q1.total, this$q2.total, this$q3.total, this$q4.total)
                    printf("w: prodcells(red) :: %.1f %.1f %.1f %.1f", this$q1.prodcells, this$q2.prodcells, this$q3.prodcells, this$q4.prodcells)
                    printf("w: prodcellsplus(green) :: %.1f %.1f %.1f %.1f", this$q1.prodcellsplus, this$q2.prodcellsplus, this$q3.prodcellsplus, this$q4.prodcells)
                  }
                }
              } 
              
              
              if (checkGATED != "1") {
                this$origin.ncells <- ncells
                this$coords  <- list()
              }
              
              if (checkCALC == "density") {
                this$bintriplot(data=tdata, cutoffs=cutoffs, density=TRUE, binSize=binSize, mincells=mincount)
              } else if (checkCALC == "MSI(+)") {
                this$bintriplot(data=tdata.plus, cutoffs=cutoffs, binSize=binSize, mincells=mincount, quadrants.color = "blue", data.origin=tdata)
              } else {
                this$bintriplot(data=tdata, cutoffs=cutoffs, binSize=binSize, mincells=mincount)
              }
              
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
                firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
              } else {
                firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
              } 
              title(main=firstLine, line=3.2, cex.main=0.9, adj=0)
              
              if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                secondLine <- sprintf("cells(min/max)=%s/%s;%s", mincount, this$maxcells, v3)
              } else { 
                secondLine <- sprintf("cells(min/max)=%s/%s", mincount, this$maxcells)
              }
              title(main=secondLine, line=2.4, cex.main=0.9, adj=0)
              
              thirdLine <- sprintf("%s-%s(%0.1f%%); binSize=%s, #bins=%s", ncells.total, ncells.zero, (ncells.zero / ncells.total * 100), binSize, this$bincount)
              title(main=thirdLine, line=1.6, cex.main=0.7, adj=0)
            }
          })
    printf("Saved file in %s", pdf.file)
    
    ### save FI ranges for this transformation type
    this$changeFI.range(mode=3)
    
    ############# stop 
    if (this$working) print(Sys.time() - timeSTART)
    
    printf("Saved in %s.", pdf.file)
    
    tkconfigure(this$tt, cursor = "left_ptr")
    
    if (checkDATE == "1") {
      date <- gsub("-", "", Sys.Date())
      title(main=date, outer=TRUE, line=1, cex.main=1.3, adj=1)
    }
  }
}

fcs$dotriploTRectData <- function() {
  this <- fcs
  printf("w: do dotriploTRectData")
  
  # get information: file, vars, cutoffs
  file <- tclvalue(tkget(this$tkchoosefile))
  var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
  tkset(this$cbvar1, var1)
  var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
  tkset(this$cbvar2, var2)
  var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))
  tkset(this$cbvar3, var3)
  vars <- c(var1, var2, var3)
  
  ### if features are not in sample
  if (length(var1) == 0 | length(var2) == 0 | length(var3) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your features.")
    stop("One of the features are not existent.")
  }
  
  cutoffx <- this$checkDigits(cutoff_id=which(this$selected.vars == var1))
  cutoffy <- this$checkDigits(cutoff_id=which(this$selected.vars == var2))
  cutoffz <- this$checkDigits(cutoff_id=which(this$selected.vars == var3))
  cutoffs <- c(cutoffx, cutoffy, cutoffz)
  
  ### if axes ranges are not the same
  try(if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  })
  
  # set plot window
  dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
  devSet(devList()[which(dev.label == names(devList()))])
  # set info label
  tkconfigure(this$ncell.gui, text=as.character(this$origin.ncells))
  tkconfigure(this$ncell.gui.di, text=as.character(this$origin.ncells))
  
  # get information: binSize, mincount, FIs
  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  checkGATED <- tclvalue(this$cbtgateData)
  checkCALC <- tclvalue(this$rbcalc)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkTRANS <- tclvalue(this$rbtrans)
  if (checkTRANS == "asinh") {
    scale <- this$asinh$scale
    label <- this$asinh$label
    grid.step <- this$asinh$step
  } else {
    scale <- this$biex$scale
    label <- this$biex$label
    grid.step <- this$biex$step
  } 
  
  tkconfigure(this$tt, cursor = "watch")
  
  
  data <- this$data
  if (nrow(data) == 0) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please select an area with bins.", icon = "error", type = "ok")
    stop("No area with bins selected.")
  } else {
    # if temporary rect data should be saved
    if (checkGATED == "1") {
      this$temp.num <- this$temp.num + 1
      
      file <- unlist(strsplit(file, "temp[0-9]{2}_"))
      file <- file[[length(file)]]
      if (this$working) printf("w: do dotriploTRectData: file: %s", file)
      
      ### only allow underscores and dots for special characters
      temp.name <- sprintf("temp%02i_%s", this$temp.num, file)
      temp.name <- sub(".fcs$", "", temp.name)
      temp.name <- sub(".csv$", "", temp.name)
      temp.name <- gsub("[^[:alnum:]_]", "", temp.name)
      
      if (this$working) printf("w: do dotriploTRectData: temp.name: %s", temp.name)
      
      this$temptable.name[this$temp.num] <- temp.name
      col1 <- as.matrix(rep(1, nrow(data)))
      colnames(col1) <- "file_ID"
      data <- sinh(data)
      data <- cbind(col1, data)
      data <- transform(data, as.numeric("file_ID"))
      
      dbWriteTable(this$conn, temp.name, data, overwrite=TRUE)
      print(paste("writing temporary", nrow(data), "cells in database", temp.name, ".."))
      
      
      this$current.filenames <- c(temp.name, this$current.filenames)
      
      # set file name
      tkconfigure(this$tkchoosefile, values=this$current.filenames)
      tkset(this$tkchoosefile, temp.name)
      
      this$temp.data <- TRUE
      
      table <- temp.name
      file.idx <- 0
      
    } else {
      table <- this$selected.project
      file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
    }
    
    
    ### get rect data
    if (this$temp.data) {
      this$getInfo(temp.name, vars)
    } else {
      this$getInfo(file, vars)
    }
    
    ### remove ending from filename
    displayfile <- this$shortenFilename(file)
    
    timeSTART <- Sys.time()
    if (this$working) print("Time loading plot:")
    
    # start plot
    # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
    # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
    set.cex <- 1.2
    set.cex.axes <- 1.0
    set.mgp <- c(1.7, 0.4, 0)
    
    if (cutoffs[1] > 0) title.axis <- sprintf("%s (%s)", var1, cutoffs[1])
    else title.axis <- var1
    if (cutoffs[2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", var2, cutoffs[2]))
    else title.axis <- c(title.axis, var2)
    
    plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
      xlim = c(xminval - 0.5, xmaxval + 1),
      ylim = c(yminval - 0.5, ymaxval + 1),
      xlab = title.axis[1], ylab = title.axis[2],
      cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
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
    
    
    # get rect data to plot
    if (checkGATED == "1") tdata <- this$data[c(var1, var2, var3)]
    else tdata <- this$rectdata[c(var1, var2, var3)]
    
    ncells <- ncells.total <- nrow(tdata)
    quadrants.col <- "black"
    # q1 Quadrant unten links
    # q2 Quadrant unten rechts
    # q3 Quadrant oben rechts
    # q4 Quadrant oben links
    if (cutoffs[1] > 0 & cutoffs[2] > 0) {
      
      ### count cells in quadrant
      tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
      tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] < cutoffs[2]), 3]
      tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
      tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[1] & tdata[, 2] >= cutoffs[2]), 3]
      
      ### q[x].total [ink=black]
      ### percentage of cells in quadrant to total cells 
      ### or in MSI(+): percentage of cells in quadrant to total positive cells
      this$q1.total <- abs(100 * length(tdata.q1) / ncells)
      this$q2.total <- abs(100 * length(tdata.q2) / ncells)
      this$q3.total <- abs(100 * length(tdata.q3) / ncells)
      this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
      
      if (cutoffs[3] > 0) {
        ### number of cells which are producing cells in feature C
        ncells <- nrow(tdata[which(tdata[, 3] > cutoffs[3]), ])

        ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
        this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / length(tdata.q1)
        if (is.nan(this$q1.prodcells)) {
          this$q1.prodcells <- 0
          this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / length(tdata.q2)
        }
         
        if (is.nan(this$q2.prodcells)) {
          this$q2.prodcells <- 0
          this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / length(tdata.q3)
        }
       
        if (is.nan(this$q3.prodcells)) {
          this$q3.prodcells <- 0
          this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[3])]) / length(tdata.q4)
        }
        if (is.nan(this$q4.prodcells)) {
          this$q4.prodcells <- 0
        }
        
        ### only do MSI plots on producing cells only
        if (checkCALC == "MSI(+)") {
          ### cut all cells which are not producing cells
          tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
          ncells <- nrow(tdata.plus)
          
          tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
          tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
          tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & data.plus[, 2] >= cutoffs[2]), 3]
           
          ### in MSI(+): percentage of cells in quadrant to total positive cells
          this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
          this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
          this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
          this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total) 
        }
        

        ### percentage of cells which are positive for feature C to total cells
        this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[3])]) / ncells.total
        if (is.nan(this$q1.prodcellsplus)) {
          this$q1.prodcellsplus <-  0
          this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[3])]) / ncells.total
        }
        
        if (is.nan(this$q2.prodcellsplus)) {
          this$q2.prodcellsplus <- 0
          this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[3])]) / ncells.total
        }

        if (is.nan(this$q3.prodcellsplus)) {
          this$q3.prodcellsplus <- 0
          this$q4.prodcellspl
        }
        if (is.nan(this$q4.prodcellsplus)) {
          this$q4.prodcellsplus <- 0
        }
        
        if (this$working) {
          print("w: ncells ::  q1  q2  q3  q4")
          printf("w: %s(total) :: %s %s %s %s", ncells.total, length(tdata.q1), length(tdata.q2), length(tdata.q3), length(tdata.q4))
          printf("w: %s(black / blue) :: %.1f %.1f %.1f %.1f", ncells, this$q1.total, this$q2.total, this$q3.total, this$q4.total)
          printf("w: prodcells(red) :: %.1f %.1f %.1f %.1f", this$q1.prodcells, this$q2.prodcells, this$q3.prodcells, this$q4.prodcells)
          printf("w: prodcellsplus(green) :: %.1f %.1f %.1f %.1f", this$q1.prodcellsplus, this$q2.prodcellsplus, this$q3.prodcellsplus, this$q4.prodcells)
        }
      }
    } 
    
    
    # get only the cells which are greater than 0
    tdata.zero <- tdata[tdata[, 1] >= 0 & tdata[, 2] >= 0, ]
    ncells.zero <- nrow(tdata.zero)
    
    ### calc quadrants with only positive values
    # q1 Quadrant unten links
    # q2 Quadrant unten rechts
    # q3 Quadrant oben rechts
    # q4 Quadrant oben links
    if (cutoffs[1] > 0 & cutoffs[2] > 0) {
      q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[1] & tdata.zero[, 2] < cutoffs[2])) / ncells.zero)      
      q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[1] & tdata.zero[, 2] < cutoffs[2])) / ncells.zero)                    
      q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[1] & tdata.zero[, 2] >= cutoffs[2])) / ncells.zero)                    
      q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
    }
    
    if (checkGATED != "1") this$origin.ncells  <- ncells 
    tkconfigure(this$ncell.sel.gui, text=as.character(ncells))
    tkconfigure(this$ncell.sel.gui.di, text=as.character(ncells))
    
    # plot rect data
    if (checkCALC == "density") {
      this$bintriplot(data=tdata, cutoffs=cutoffs, density=TRUE, binSize=binSize, mincells=mincount)
    } else if (checkCALC == "MSI(+)") {
      this$bintriplot(data=tdata.plus, cutoffs=cutoffs, binSize=binSize, mincells=mincount, quadrants.color = "blue", data.origin=tdata)
    } else {
      this$bintriplot(data=tdata, cutoffs=cutoffs, binSize=binSize, mincells=mincount)
    }
    
    this$ncell.perc  <- ncells / this$origin.ncells * 100
    if (checkGATED == "1") {
      ### remove ending from filename
      firstLine <- sprintf("%s * (%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
    } else {
      firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
    }
    title(main=firstLine, line=3.2, cex.main=0.9, adj=0)
    
    #if (checkCALC == "freq" | checkCALC == "MSI(+)") secondLine <- sprintf("cells(min/max)=%s/%s; cutoff=%s", mincount, this$maxcells, cutoffs[3])
    if (checkCALC == "freq" | grepl("MSI", checkCALC)) secondLine <- sprintf("cells(min/max)=%s/%s; %s", mincount, this$maxcells, var3)
    else secondLine <- sprintf("cells(min/max)=%s/%s", mincount, this$maxcells)
    title(main=secondLine, line=2.4, cex.main=0.9, adj=0)
    
    thirdLine <- sprintf("%s-%s(%0.1f%%); binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), binSize, this$bincount)
    title(main=thirdLine, line=1.6, cex.main=0.7, adj=0)
    
    
    if (checkGATED == "1") {
      coords.string <- paste(var1, "[", this$coords$x[1], ", ", this$coords$x[2], "]", var2, "[", this$coords$y[1], ", ", this$coords$y[2], "]", sep="")
      print(coords.string)
      this$coords.info <- c(this$coords.info, coords.string)
    }
    
    if (length(this$coords.info) > 2) {
      fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
      fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
    } else {
      fourthLine <- sprintf("%s", paste(this$coords.info, collapse=";"))
      fifthLine <- ""
    }
    title(main=fourthLine, line=0.9, cex.main=0.6, adj=0)
    title(main=fifthLine, line=0.2, cex.main=0.6, adj=0)
    
    
    this$plot.num <- this$plot.num + 1
    
  }
  tkconfigure(this$tt, cursor = "left_ptr")
}

fcs$dotriploTOverview <- function(table=NA) {
  this <- fcs
  answer <- tclVar("yes")
  freqans <- tclVar("no")
  
  
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  if (!this$working) {
    answer  <- tkmessageBox(title = "Starting..", message = "Are you sure?", icon = "info", type = "yesno")
  }  
  
  if (tclvalue(answer) == "no") stop("OK.")
  
  checkCALC <- tclvalue(this$rbcalc)
  checkTRANS <- tclvalue(this$rbtrans)
  checkGATED <- tclvalue(this$cbtgateData)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkTRIMMING <- tclvalue(this$cbttrimming)
    
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
    
  len <- length(this$selected.vars)
   
  # column and cutoff number vector
  colvec <- vector()
  cutoffs <- vector()
  for (i in 1:len) {
    if (tclvalue(this$cbVal[[i]]) == "1") {
      colvec <- c(colvec, i)
      cutoffs <- c(cutoffs, this$checkDigits(cutoff_id=i))
    }
  }
  len.colvec <- length(colvec)
  # title length
  titlelen <- length(unlist(strsplit(tclvalue(tkget(this$title, "1.0", "end-1c")), "")))
  titleans <- tclVar("yes")
    
  if (len.colvec < 3) {
    tkmessageBox(title = "An error has occured!", 
                   message = "Please select at least three markers.", icon = "error", type = "ok")
    stop("Please select at least three markers.")
  }

  if (titlelen <= 1 & this$working == FALSE) {
    titleans <- tkmessageBox(title = "Are you sure?", 
                            message = "Do not forget to set page title. Continue?", icon = "info", type = "yesno")
  } else if (checkCALC == "freq" & !is.integer(which(cutoffs == 0)) & this$working == FALSE) {
    freqans <- tkmessageBox(title = "Are you sure?", 
                            message = "Do not forget to set cutoffs. Continue?", icon = "info", type = "yesno")
  } else if (checkCALC == "density") {
    tkmessageBox(title = "For your information.", 
                   message = "It is not meaningful to plot method \"density\".", icon = "error", type = "ok")
    stop("Chose another statistical method.")
  }
  
  if ((tclvalue(titleans) == "yes") | tclvalue(freqans) == "yes") {
    if (this$working) {
      this$saveinFolder  <- getwd()
    } else {
      # if path didnt save yet
      if (!exists("lastpath", where=eval(parse(text="fcs")))){
        this$saveinFolder  <- tk_choose.dir(default = getwd(), caption = "Select directory to save the PDFs")
      } else {
        this$saveinFolder  <- tk_choose.dir(default = this$lastpath, caption = "Select directory to save the PDFs")
      }
    }
      
    if (!is.na(this$saveinFolder)) {
      tkconfigure(this$tt, cursor = "watch")
        
      this$lastpath <- this$saveinFolder
        
      max.nhorizplots <- 6
      max.nvertiplots <- 8
        
      res.nhorizplots <- (len.colvec - 2) %% max.nhorizplots
      if (res.nhorizplots != 0) res.nhorizplots <- max.nhorizplots - res.nhorizplots
      max.nplots <- len.colvec * (len.colvec - 1) * (len.colvec - 2)
      max.npages <- ceiling((max.nplots + res.nhorizplots * (len.colvec - 1) * len.colvec) / (max.nhorizplots * max.nvertiplots)) # + 1
        
      ### Create Progress Bar
      this$pb <- tkProgressBar(title="triploT-Overview", label=paste("Creating page 1 / ", max.npages, sep=""), min = 0, max = max.npages, initial = 1, width = 300)
      
      # set progress bar
      setTkProgressBar(this$pb, 1, label=paste("Creating page 1 / ", max.npages, sep=""))
        
      timeSTART  <- Sys.time()
        
      if (!this$OverviewGate) {
        cat("\n\n>>>> Start triploT-Overview with total data: \n\n")
      } else {
        cat("\n\n>>>> Start triploT-Overview with gated data: \n\n")
      }
      
      if (this$working) print(sprintf("w: %s - time started", timeSTART))
        
      tmp.folder  <- file.path(this$saveinFolder, "tmp")
      if (grepl("linux", sessionInfo()$R.version$os)) {
        ### on linux
        system(paste("mkdir -p -v ", tmp.folder, sep=""))
      } else {
        ### on Windows
        dir.create(tmp.folder)
        printf(">>on Windows<< Creating folder: %s", tmp.folder)
      }
        
      file <- tclvalue(tkget(this$tkchoosefile))
      if (this$OverviewGate) {
        displayfile <- this$shortenFilename(this$plot.attr[[1]]$file.name, title=TRUE)
      } else {
        displayfile <- this$shortenFilename(file)
      }
        
      table <- this$total.projects[this$selected.projectnum]
      if (grepl("temp", file)) {
        file.idx <- 1
        table <- file
      } else {
        file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
        this$selected.filenum <- file.idx
      }
        
      if (this$working) printf("w: table=%s file.idx=%s", table, file.idx)
        
      binSize <- as.numeric(tkget(this$binSize))
      mincount <- as.numeric(tkget(this$minCountTri))
      xminval <- as.numeric(tkget(this$minvalX))
      xmaxval <- as.numeric(tkget(this$maxvalX))
      yminval <- as.numeric(tkget(this$minvalY))
      ymaxval <- as.numeric(tkget(this$maxvalY))
        
      #### Do not trim and remove doublets if data is gated
      if (this$OverviewGate) {
      # if data is gated, just recall
        data <- this$getData(table, file.idx, columns=colvec)
      } else if (checkTRIMMING == "1") {
        this$preprocData(mode="trim")
        data <- this$data[, colvec]
      } else {
        this$getFile(table, file.idx)
        data <- this$data[, colvec]
      }
        
      len <- dim(data)[2]
        
      printf("Columns selected (%s): %s", dim(data)[2], paste(colvec, collapse=" "))
      printf("Features selected: %s", paste(colnames(data), collapse=" "))
        
      title <- as.character(tclvalue(tkget(this$title, "1.0", "end-1c")))
        
      ### if less than 16 markers were selected
      # do all triplots in one pdf file
      # otherwise make one pdf file for one page
      if (len.colvec <= 16){
        toPDF(file=sprintf("%s_triploTOverview_%s%s_%s.pdf", displayfile, len.colvec, checkCALC, this$version), 
                path=this$saveinFolder, 
                title=sprintf("triploTOverview of %s", displayfile), 
                width=3.21 * max.nhorizplots, 
                height=3.5 * max.nvertiplots, 
                pointsize=11, {
                  
                  ##### start triploT overview
                  ### set label and axes font sizes
                  
                  label.cex <- 1.1 - 0.5 * this$legend.space
                  set.cex.axes <- 1
                  set.mgp <- c(1.9, 0.5, 0)
                  par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 6, 1), mar=c(3, 4, 5, 1))
                  
                  
                  plot.idx <- 0
                  page.idx <- 0
                  current.page <- 0
                  for (v1 in 1:len) {
                    for (v2 in 1:len) {
                      
                      if (v1 == v2) next
                      
                      for (v3 in 1:len) {
                        
                        ## skip loop if one of the axes are the same
                        if (v1 == v3 | v2 == v3) next
                        
                        #select columns to plot
                        tdata <- as.matrix(data[, c(v1, v2, v3)])
                        this$cnames <- colnames(tdata)
                        
                        if (cutoffs[v1] > 0) title.axis <- sprintf("%s (%s)", colnames(data)[v1], cutoffs[v1])
                        else title.axis <- colnames(data)[v1]
                        if (cutoffs[v2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", colnames(data)[v2], cutoffs[v2]))
                        else title.axis <- c(title.axis, colnames(data)[v2])
                        # start plot
                        # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
                        # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
                        plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                          xlim = c(xminval - 0.5, xmaxval + 1),
                          ylim = c(yminval - 0.5, ymaxval + 1),
                          xlab = title.axis[1], ylab = title.axis[2],
                          cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
                        box(lwd=0.5, col="darkgrey")
                        
                        plot.idx  <- plot.idx + 1
                        
                        ### draw axis on the bottom and on the left
                        axis(side=1, at=scale, labels=label, las=1, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
                        axis(side=2, at=scale, labels=label, las=3, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
                        
                        ### add grid
                        if (checkGRID == "1") {
                          xgrid.steps <- seq(0, (xmaxval), by=grid.step)
                          ygrid.steps <- seq(0, (ymaxval), by=grid.step)
                          abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
                        }
                        
                        
                        ncells <- ncells.total <- nrow(tdata)
                        # q1 Quadrant unten links
                        # q2 Quadrant unten rechts
                        # q3 Quadrant oben rechts
                        # q4 Quadrant oben links
                        if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                          
                          ### count cells in quadrant
                          tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                          tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                          tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                          tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                          
                          ### q[x].total [ink=black]
                          ### percentage of cells in quadrant to total cells 
                          ### or in MSI(+): percentage of cells in quadrant to total positive cells
                          this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                          this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                          this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                          this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                          
                          if (cutoffs[v3] > 0) {
                            ### number of cells which are producing cells in feature C
                            ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
                            
                            ### q[x].prodcells [ink=red]
                            ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                            this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
                            if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                            this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
                            if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                            this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
                            if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                            this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
                            if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                            
                            ### only do MSI plots on producing cells only
                            if (checkCALC == "MSI(+)") {
                              ### cut all cells which are not producing cells
                              tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                              ncells <- nrow(tdata.plus)
                              
                              tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                              tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                              tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                              tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                              
                              ### q[x].total [ink=blue]
                              ### in MSI(+): percentage of cells in quadrant to total positive cells
                              this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                              this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                              this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                              this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                            }
                            
                            ### q[x].prodcellsplus [ink=green]
                            ### percentage of cells which are positive for feature C to total cells
                            this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
                            if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                            this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
                            if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                            this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
                            if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                            this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
                            if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                          }
                        } 
                        
                        
                        tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1,    
                            function(row) {
                              all(row > 0)
                              }
                              ), ]
                        ncells.zero <- nrow(tdata.zero)
                        
                        ### calc quadrants with only positive values
                        # q1 Quadrant unten links
                        # q2 Quadrant unten rechts
                        # q3 Quadrant oben rechts
                        # q4 Quadrant oben links
                        if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                          q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)      
                          q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)                    
                          q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] >= cutoffs[v2])) / ncells.zero)                    
                          q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
                        }
                        
                        if (checkCALC == "density") {
                          this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                        } else if (checkCALC == "MSI(+)") {
                          this$bintriplot(data=tdata.plus, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, , overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
                        } else {
                          this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                        }
                        
                        
                        if (this$OverviewGate) {
                          this$ncell.perc  <- this$ncell.sel / this$origin.ncells * 100
                        }
                        
                        # add title for single plot
                        if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                          if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s, %s/cof=%s", displayfile, this$ncell.perc, checkCALC, v3, this$current.cofactor)
                          else firstLine <- sprintf("%s: %s, cutoff=%s/cof=%s", displayfile, checkCALC, cutoffs[v3], this$current.cofactor)
                        } else {
                          if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
                          else firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
                        }
                        title(main=firstLine, line=3.2, cex.main=0.9 * label.cex, adj=0)
                        
                        secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
                        title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
                        
                        thirdLine <- ""
                        if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                          thirdLine <- sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
                        }
                        title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
                        
                        if (checkGATED == "1" | grepl("temp", file)) {
                          if (length(this$coords.info) > 2) {
                            fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
                            fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
                          } else {
                            fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
                            fifthLine <- ""
                          }
                          title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
                          title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
                        }
                        
                        ### update progress bar
                        if ((plot.idx != 0) & ((plot.idx - 1) %% (max.nhorizplots * max.nvertiplots) == 0)) {
                          page.idx <- page.idx + 1
                          info <- paste("Creating page ", page.idx, " / ", max.npages, sep="")
                          if (this$working) printf("%s with plot# %s", info, plot.idx)
                          setTkProgressBar(this$pb, page.idx, label=info)
                        }
                        
                        #add title for page in 3D-Overview tab
                        if (current.page != page.idx) {
                          mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2, xpd=TRUE)
                          if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                          current.page <- page.idx
                        }
                      }
                      
                      ### plot empty plots
                      if ((plot.idx != 0) & (res.nhorizplots > 0)) {
                        for (i in 1:res.nhorizplots) {
                          plot.new()
                          plot.idx <- plot.idx + 1
                        } 
                      }
                      
                    }
                  }
                  
                  ### plot histograms
                  this$plotHistograms(plotter="triover", pdf=TRUE)
                  
                  tkmessageBox(title = "Output of overview", 
                               message = paste("PDF created in folder ", this$saveinFolder, sep=""))
                  
                }
         )
      } else {
        toPDF(file=sprintf("%s_triploTOverview_%shist_%s.pdf", 
          displayfile, len.colvec, this$version), path=this$saveinFolder, title=sprintf("Histograms of %s", displayfile), 
          width=3.21 * max.nhorizplots, 
          height=3.5 * max.nvertiplots, 
          pointsize=11, 
            {
              # plot histograms
              this$plotHistograms(plotter="triover", pdf=TRUE)
            }
        )
          
        for (v1 in 1:len) {
          printf("New PDF: #%s", v1)
          plot.idx <- 0
          page.idx <- 0
            
          toPDF(file=sprintf("%s_triploTOverview_%s_%s%s_%s.pdf", displayfile, 
            colnames(data)[v1], len.colvec - 1, checkCALC, this$version), 
            path=this$saveinFolder, 
            title=sprintf("triploTOverview of %s(%s)", displayfile, 
            colnames(data)[v1]), 
            width=3.21 * max.nhorizplots, 
            height=3.5 * max.nvertiplots, 
            pointsize=11, {
              ### new
              label.cex <- 1.1 - 0.5 * this$legend.space
              set.cex.axes <- 1
              set.mgp <- c(1.9, 0.5, 0)
              par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 5, 1), mar=c(3, 4, 5, 1))
              ###
                    
              for (v2 in 1:len) {
                if (v1 == v2) next
                for (v3 in 1:len) {
                  ## skip loop if one of the axes are the same
                  if (v1 == v3 | v2  == v3) next
                  
                  plot.idx <- plot.idx + 1

                  #select columns to plot
                  tdata <- as.matrix(data[, c(v1, v2, v3)])
                  this$cnames <- c(colnames(data)[v1], colnames(data)[v2], colnames(data)[v3])
                        
                  if (cutoffs[v1] > 0) {
                    title.axis <- sprintf("%s (%s)", colnames(data)[v1], cutoffs[v1])
                  } else {
                    title.axis <- colnames(data)[v1]
                  }
                  
                  if (cutoffs[v2] > 0) {
                    title.axis <- c(title.axis, sprintf("%s (%s)", colnames(data)[v2], cutoffs[v2]))
                  } else {
                    title.axis <- c(title.axis, colnames(data)[v2])
                  }
                      
                  # start plot
                  # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
                  # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
                  plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                    xlim = c(xminval - 0.5, xmaxval + 1),
                    ylim = c(yminval - 0.5, ymaxval + 1),
                    xlab=title.axis[1], ylab=title.axis[2], 
                    cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
                  box(lwd=0.5, col="darkgrey")
                        
                  ### draw axis on the bottom and on the left
                  axis(side=1, at=scale, labels=label, las=1,
                    cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
                  axis(side=2, at=scale, labels=label, las=3, 
                    cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
                        
                  ### add grid
                  if (checkGRID == "1") {
                    xgrid.steps <- seq(0, (xmaxval), by=grid.step)
                    ygrid.steps <- seq(0, (ymaxval), by=grid.step)
                    abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
                  }
                        
                  ### calc quadrants in total
                  ncells <- ncells.total <- nrow(tdata)
                  # q1 Quadrant unten links
                  # q2 Quadrant unten rechts
                  # q3 Quadrant oben rechts
                  # q4 Quadrant oben links
                  if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                          
                    ### count cells in quadrant
                    tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                    tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                    tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                    tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                          
                    ### q[x].total [ink=black]
                    ### percentage of cells in quadrant to total cells 
                    ### or in MSI(+): percentage of cells in quadrant to total positive cells
                    this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                    this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                    this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                    this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                          
                    if (cutoffs[v3] > 0) {
                      ### number of cells which are producing cells in feature C
                      ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
                            
                      ### q[x].prodcells [ink=red]
                      ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                      this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
                      if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                            
                      this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
                      if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                            
                      this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
                      if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                            
                      this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
                      if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                            
                      ### only do MSI plots on producing cells only
                      if (checkCALC == "MSI(+)") {
                        ### cut all cells which are not producing cells
                        tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                        ncells <- nrow(tdata.plus)
                              
                        tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                        tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                        tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                        tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                              
                        ### q[x].total [ink=blue]
                        ### in MSI(+): percentage of cells in quadrant to total positive cells
                        this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                        this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                        this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                        this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                      }
                            
                      ### q[x].prodcellsplus [ink=green]
                      ### percentage of cells which are positive for feature C to total cells
                      this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                            
                      this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0

                      this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                            
                      this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                    }
                  } 
                        
                  tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {
                    all(row > 0)}), ]
                  ncells.zero <- nrow(tdata.zero)
                  
                  ### calc quadrants with only positive values
                  # q1 Quadrant unten links
                  # q2 Quadrant unten rechts
                  # q3 Quadrant oben rechts
                  # q4 Quadrant oben links
                  if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                    q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)      
                    q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)                    
                    q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] >= cutoffs[v2])) / ncells.zero)                    
                    q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
                  }
                        
                  if (checkCALC == "density") {
                      this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                  } else if (checkCALC == "MSI(+)") {
                      this$bintriplot(data=tdata.plus, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, , overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
                  } else {
                      this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                  }
                        
                  if (this$OverviewGate) {this$ncell.perc = this$ncell.sel / this$origin.ncells * 100}
                        
                  # add title for single plot
                  if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                    if (this$OverviewGate) {
                      firstLine <- sprintf("%s(%0.1f%%): %s, %s/cof=%s", displayfile, this$ncell.perc, checkCALC, v3, this$current.cofactor)
                    } else {
                      firstLine <- sprintf("%s: %s, cutoff=%s/cof=%s", displayfile, checkCALC, cutoffs[v3], this$current.cofactor)
                    }
                  } else {
                    if (this$OverviewGate) {
                      firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
                    } else {
                      firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
                    }
                  }
                  title(main=firstLine, line=3.2, cex.main=0.9 * label.cex, adj=0)
                        
                  secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
                  title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
                        
                  thirdLine <- ""
                  if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                    thirdLine <- sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
                  }
                  title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
                        
                  if (checkGATED == "1" | grepl("temp", file)) {
                    if (length(this$coords.info) > 2) {
                      fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
                      fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
                    } else {
                      fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
                      fifthLine <- ""
                    }
                    title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
                    title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
                  }
                }
                      
                ### update progress bar
                if ((plot.idx != 0) & ((plot.idx - 1) %% (max.nhorizplots * max.nvertiplots) == 0)) {
                  page.idx <- page.idx + 1
                  info <- paste("Creating page ", page.idx, " / ", max.npages, sep="")
                  if (this$working) printf("%s with plot# %s", info, plot.idx)
                  setTkProgressBar(this$pb, page.idx, label=info)
                }
                      
                #add title for page in 3D-Overview tab
                if (current.page != page.idx) {
                  mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2, xpd=TRUE)
                  if (length(this$coords.info>0)) {
                    mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                  }
                  current.page <- page.idx
                }
                      
                ### plot emptry plots
                printf("last plot #:%s with X:%s Y:%s; plotting %s more empty plots", plot.idx, v1, v2, res.nhorizplots)
                if (res.nhorizplots > 0) {
                  for (i in 1:res.nhorizplots) {
                    plot(1, type="n", frame.plot=FALSE, axes=FALSE, ylab="")
                    plot.idx <- plot.idx + 1
                  }
                }
              }
            }
           )
          }
          
          tkmessageBox(title = "Output of overview", 
                       message = paste ("Plots created in folder ", this$saveinFolder, sep=""))
      }
        
      tkconfigure(this$tt, cursor = "left_ptr")
        
      cat("\n")
      # close progress bar
      close(this$pb)
        
      print(Sys.time() - timeSTART)
        
      cat("\n>>>> Done.\n")
      }
    }
  this$OverviewGate=FALSE
}

fcs$dotriploTOverview_ALL <- function(table=NA) {
  this <- fcs
  answer <- tclVar("yes")
  freqans <- tclVar("no")
  
  
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  if (!this$working) {
    answer <- tkmessageBox(title = "Starting..",
                          message = "Are you sure? Did you check the setting with fixed features?",
                          icon = "info", type = "yesno")
  }
  
  if (tclvalue(answer) == "no") stop("OK.");
  
  checkCALC <- tclvalue(this$rbcalc)
  checkTRANS <- tclvalue(this$rbtrans)
  checkGATED <- tclvalue(this$cbtgateData)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkTRIMMING <- tclvalue(this$cbttrimming)
  checkFeatA <- tclvalue(this$cbtfeatA)
  checkFeatB <- tclvalue(this$cbtfeatB)
  checkFeatC <- tclvalue(this$cbtfeatC)
  
  if (checkCALC == "density") {
    tkmessageBox(title = "For your information.", 
                 message = "It is not meaningful to plot method \"density\".", icon = "error", type = "ok")
    stop("Chose another statistical method.")
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
  
  FEATURES.fix <- c(checkFeatA, checkFeatB, checkFeatC)
  FEATURES.idx <- FEATURES.cutoff <- c(0, 0, 0)
  
  fixed.features <- 0
  for (i in 1:length(FEATURES.fix)) {
    if (FEATURES.fix[i] == "1") {
      tmp.var = this$checkMarker(tclvalue(tkget(eval(parse(text=paste0("this$cbvar", i))))))
      if (length(tmp.var) == 0) {
        tkmessageBox(title = "An error has occured!", message = sprintf("Check your Feature pos %s.", i))
        stop("This feature is not existent in this file.")
      } else {
        FEATURES.fix[i] <- tmp.var
        FEATURES.idx[i] <- which(this$selected.vars == tmp.var)
        FEATURES.cutoff[i] <- this$checkDigits(cutoff_id=as.numeric(checkFEATURES[[i]][2]))
        fixed.features <- fixed.features  + 1
        if (this$working) printf("Fixed Feature #%s", i)
      }
    }
  }
  # stop("Funzt!")
  
  ### get column and cutoff number vector
  len <- length(this$selected.vars)
  colvec <- vector()
  cutoffs <- vector()
  for (i in 1:len) {
    if (tclvalue(this$cbVal[[i]]) == "1") {
      colvec <- c(colvec, i);
      cutoffs <- c(cutoffs, this$checkDigits(cutoff_id=i))
    }
  }
  ### remove doubled checked features
  remove.idx <- which(colvec == FEATURES.cutoff)
  colvec <- colvec[-remove.idx]
  cutoffs <- cutoffs[-remove.idx]
  
  len.colvec <- length(colvec)
  
  ### title length
  titlelen <- length(unlist(strsplit(tclvalue(tkget(this$title, "1.0", "end-1c")), "")))
  titleans <- tclVar("yes")
  
  if (titlelen <= 1 & this$working == FALSE) {
    titleans <- tkmessageBox(title = "Are you sure?", 
                            message = "Do not forget to set page title. Continue?", icon = "info", type = "yesno")
  } else if (checkCALC == "freq" & !is.integer(which(cutoffs == 0)) & this$working == FALSE) {
    freqans <- tkmessageBox(title = "Are you sure?", 
                           message = "Do not forget to set cutoffs. Continue?", icon = "info", type = "yesno")
  }
  
  if ((tclvalue(titleans) == "no") | tclvalue(freqans) == "no") stop("OK.")
  
  if (this$working) {
    this$saveinFolder <- getwd()
  } else {
    # if path havent been saved yet
    if (!exists("lastpath", where=eval(parse(text="fcs")))){
      this$saveinFolder = tk_choose.dir(default = getwd(), caption = "Select directory to save the PDFs")
    } else {
      this$saveinFolder = tk_choose.dir(default = this$lastpath, caption = "Select directory to save the PDFs")
    }
  }

  if (!is.na(this$saveinFolder)) {
    tkconfigure(this$tt, cursor = "watch")
    
    this$lastpath <- this$saveinFolder
    
    max.nhorizplots <- 6
    max.nvertiplots <- 8
    
    res.nhorizplots <- (len.colvec - 2) %% max.nhorizplots
    if (res.nhorizplots != 0) res.nhorizplots <- max.nhorizplots - res.nhorizplots
    max.nplots <- len.colvec * (len.colvec - 1) * (len.colvec - 2)
    max.npages <- ceiling((max.nplots + res.nhorizplots * (len.colvec - 1) * len.colvec) / (max.nhorizplots * max.nvertiplots)) # + 1
    
    ### Create Progress Bar
    this$pb <- tkProgressBar(title="triploT-Overview", label=paste("Creating page 1 / ", max.npages, sep=""), min = 0, max = max.npages, initial = 1, width = 300)
    # set progress bar
    setTkProgressBar(this$pb, 1, label=paste("Creating page 1 / ", max.npages, sep=""))
    
    timeSTART <- Sys.time()
    if (!this$OverviewGate) cat("\n\n>>>> Start triploT-Overview with total data: \n\n")
    else cat("\n\n>>>> Start triploT overview with gated data: \n\n")
    if (this$working) print(sprintf("w: %s - time started", timeSTART))
    
    tmp.folder <- file.path(this$saveinFolder, "tmp")
    if (grepl("linux", sessionInfo()$R.version$os)) {
      ### on linux
      system(paste("mkdir -p -v ", tmp.folder, sep=""))
    } else {
      ### on Windows
      dir.create(tmp.folder)
      printf(">>on Windows<< Creating folder: %s", tmp.folder)
      #system(paste("mkdir ", tmp.folder, sep=""))
      # or shell()?
    }
    
    file <- tclvalue(tkget(this$tkchoosefile))
    if (this$OverviewGate) {
      displayfile <- this$shortenFilename(this$plot.attr[[1]]$file.name, title=TRUE)
    } else {
      displayfile <- this$shortenFilename(file)
    }
    
    table <- this$total.projects[this$selected.projectnum]
    if (grepl("temp", file)) {
      file.idx <- 1
      table <- file
    } else {
      file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
      this$selected.filenum <- file.idx
    }
    
    if (this$working) printf("w: table=%s file.idx=%s", table, file.idx)
    
    binSize <- as.numeric(tkget(this$binSize))
    mincount <- as.numeric(tkget(this$minCountTri))
    xminval <- as.numeric(tkget(this$minvalX))
    xmaxval <- as.numeric(tkget(this$maxvalX))
    yminval <- as.numeric(tkget(this$minvalY))
    ymaxval <- as.numeric(tkget(this$maxvalY))
    
    #### Do not trim and remove doublets if data is gated
    if (this$OverviewGate) {
      # if data is gated, just recall
      data <- this$getData(table, file.idx, columns=colvec)
    } else if (checkTRIMMING == "1") {
      this$preprocData(mode="trim")
      data <- this$data[, colvec]
    } else {
      this$getFile(table, file.idx)
      data <- this$data[, colvec]
    }
    
    len <- dim(data)[2]
    
    printf("Columns selected (%s): %s", dim(data)[2], paste(colvec, collapse=" "))
    printf("Features selected: %s", paste(colnames(data), collapse=" "))
    
    title <- as.character(tclvalue(tkget(this$title, "1.0", "end-1c")))
    
    ### if less than 16 markers were selected
    # do all triplots in one pdf file
    # otherwise make one pdf file for one page
    if (len.colvec<=16){
      
      toPDF(file=sprintf("%s_triploTOverview_%s%s_%s.pdf", displayfile, len.colvec, checkCALC, this$version), 
            path=this$saveinFolder, 
            title=sprintf("triploTOverview of %s", displayfile), 
            width=3.21 * max.nhorizplots, 
            height=3.5 * max.nvertiplots, 
            pointsize=11, 
            {
              # plot histograms
              #this$plotHistograms(plotter="triover", pdf=TRUE)
              
              # set progress bar
              #setTkProgressBar(this$pb, 2, label=paste("Creating page 2 / ", len.colvec + 2, sep=""))
              # plot density plot
              #this$plotDensities(plotter="triover", pdf=TRUE)
              
              ##### start triploT overview
              ### set label and axes font sizes
              
              label.cex <- 1.1 - 0.5 * this$legend.space
              set.cex.axes <- 1
              set.mgp <- c(1.9, 0.5, 0)
              par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 6, 1), mar=c(3, 4, 5, 1))
              
              
              plot.idx <- 0
              page.idx <- 0
              current.page <- 0
              for (v1 in 1:len) {
                for (v2 in 1:len) {
                  
                  if (v1 == v2) next
                  
                  for (v3 in 1:len) {
                    
                    ## skip loop if one of the axes are the same
                    if (v1 == v3 | v2 == v3) next
                    
                    #select columns to plot
                    tdata <- as.matrix(data[, c(v1, v2, v3)])
                    this$cnames <- colnames(tdata)
                    
                    if (cutoffs[v1] > 0) title.axis <- sprintf("%s (%s)", colnames(data)[v1], cutoffs[v1])
                    else title.axis <- colnames(data)[v1]
                    if (cutoffs[v2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", colnames(data)[v2], cutoffs[v2]))
                    else title.axis <- c(title.axis, colnames(data)[v2])
                    # start plot
                    # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
                    # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
                    plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                        xlim = c(xminval - 0.5, xmaxval + 1),
                        ylim = c(yminval - 0.5, ymaxval + 1),
                        xlab = title.axis[1], ylab = title.axis[2],
                        cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
                    box(lwd=0.5, col="darkgrey")
                    
                    plot.idx <- plot.idx + 1
                    
                    ### draw axis on the bottom and on the left
                    axis(side=1, at=scale, labels=label, las=1, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
                    axis(side=2, at=scale, labels=label, las=3, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
                    
                    ### add grid
                    if (checkGRID == "1") {
                      xgrid.steps <- seq(0, (xmaxval), by=grid.step)
                      ygrid.steps <- seq(0, (ymaxval), by=grid.step)
                      abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
                    }
                    
                    
                    ncells <- ncells.total <- nrow(tdata)
                    # q1 Quadrant unten links
                    # q2 Quadrant unten rechts
                    # q3 Quadrant oben rechts
                    # q4 Quadrant oben links
                    if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                      
                      ### count cells in quadrant
                      tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                      tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                      tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                      tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                      
                      ### q[x].total [ink=black]
                      ### percentage of cells in quadrant to total cells 
                      ### or in MSI(+): percentage of cells in quadrant to total positive cells
                      this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                      this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                      this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                      this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                      
                      if (cutoffs[v3] > 0) {
                        ### number of cells which are producing cells in feature C
                        ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
                        
                        ### q[x].prodcells [ink=red]
                        ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                        this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
                        if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                        this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
                        if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                        this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
                        if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                        this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
                        if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                        
                        ### only do MSI plots on producing cells only
                        if (checkCALC == "MSI(+)") {
                          ### cut all cells which are not producing cells
                          tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                          ncells <- nrow(tdata.plus)
                          
                          tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                          tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                          tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                          tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                          
                          ### q[x].total [ink=blue]
                          ### in MSI(+): percentage of cells in quadrant to total positive cells
                          this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                          this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                          this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                          this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                        }
                        
                        ### q[x].prodcellsplus [ink=green]
                        ### percentage of cells which are positive for feature C to total cells
                        this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                        this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                        this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                        this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                      }
                    } 
                    
                    
                    tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
                    ncells.zero <- nrow(tdata.zero)
                    
                    ### calc quadrants with only positive values
                    # q1 Quadrant unten links
                    # q2 Quadrant unten rechts
                    # q3 Quadrant oben rechts
                    # q4 Quadrant oben links
                    if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                      q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)      
                      q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)                    
                      q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] >= cutoffs[v2])) / ncells.zero)                    
                      q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
                    }
                    
                    #this$bintriplot(tdata, cutoffs[c(v1, v2, v3)], set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp
                    #	, binSize=binSize, mincells=mincount, quadrants.color = quadrants.col)
                    if (checkCALC == "density") {
                      this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                    } else if (checkCALC == "MSI(+)") {
                      this$bintriplot(data=tdata.plus, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, , overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
                    } else {
                      this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                    }
                    
                    
                    if (this$OverviewGate) {this$ncell.perc <- this$ncell.sel / this$origin.ncells * 100}
                    
                    # add title for single plot
                    if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                      if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s, %s/cof=%s", displayfile, this$ncell.perc, checkCALC, v3, this$current.cofactor)
                      else firstLine <- sprintf("%s: %s, cutoff=%s/cof=%s", displayfile, checkCALC, cutoffs[v3], this$current.cofactor)
                    } else {
                      if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
                      else firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
                    }
                    title(main=firstLine, line=3.2, cex.main=1.0 * label.cex, adj=0)
                    
                    secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
                    title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
                    
                    thirdLine <- ""
                    if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                      thirdLine=sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
                    }
                    title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
                    
                    if (checkGATED == "1" | grepl("temp", file)) {
                      if (length(this$coords.info) > 2) {
                        fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
                        fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
                      } else {
                        fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
                        fifthLine <- ""
                      }
                      title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
                      title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
                    }
                    
                    ### update progress bar
                    if ((plot.idx != 0) & ((plot.idx - 1) %% (max.nhorizplots * max.nvertiplots) == 0)) {
                      page.idx <- page.idx + 1
                      info <- paste("Creating page ", page.idx, " / ", max.npages, sep="")
                      if (this$working) printf("%s with plot# %s", info, plot.idx)
                      setTkProgressBar(this$pb, page.idx, label=info)
                    }
                    
                    #add title for page in 3D-Overview tab
                    if (current.page != page.idx) {
                      mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2, xpd=TRUE)
                      if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                      #printf("Print on Page %s", page.idx)
                      current.page <- page.idx
                    }
                  }
                  
                  ### plot empty plots
                  if ((plot.idx != 0) & (res.nhorizplots > 0)) {
                    for (i in 1:res.nhorizplots) {
                      plot.new(); 
                      plot.idx <- plot.idx + 1
                    } 
                  }
                  
                }
              }
              
              ### plot histograms
              this$plotHistograms(plotter="triover", pdf=TRUE)
              
              tkmessageBox(title = "Output of overview", 
                           message = paste ("PDF created in folder ", this$saveinFolder, sep=""))
              
            }
     )
    } else {
      toPDF(file=sprintf("%s_triploTOverview_%shist_%s.pdf", displayfile, len.colvec, this$version), 
            path=this$saveinFolder, 
            title=sprintf("Histograms of %s", displayfile), 
            
            width=3.21 * max.nhorizplots, 
            height=3.5 * max.nvertiplots, 
            pointsize=11, 
            {
              # plot histograms
              this$plotHistograms(plotter="triover", pdf=TRUE)
              
              # set progress bar
              #setTkProgressBar(this$pb, 2, label=paste("Creating page 2 / ", max.npages, sep=""))
              
              # plot density plot
              #this$plotDensities(plotter="triover", pdf=TRUE)
            }
     )
      
      for (v1 in 1:len) {
        printf("New PDF: #%s", v1)
        plot.idx <- 0
        page.idx <- 0
        
        ### update progress bar
        #info <- paste("Creating page ", page.idx + 1, " / ", max.npages, sep="")
        #print(info)
        
        #setTkProgressBar(this$pb, v1 + 1, label=info)
        toPDF(file=sprintf("%s_triploTOverview_%s_%s%s_%s.pdf", displayfile, colnames(data)[v1], len.colvec - 1, checkCALC, this$version), 
              #toPDF(file=sprintf("%s_triploTOverview_%s%s_%s_%s.pdf", displayfile, len.colvec, checkCALC, colnames(data)[v1], this$version), 
              path=this$saveinFolder, 
              title=sprintf("triploTOverview of %s(%s)", displayfile, colnames(data)[v1]), 
              
              width=3.21 * max.nhorizplots, 
              height=3.5 * max.nvertiplots, 
              pointsize=11, 
              {
                
                ### new
                label.cex <- 1.1 - 0.5 * this$legend.space
                set.cex.axes <- 1
                set.mgp <- c(1.9, 0.5, 0)
                par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 5, 1), mar=c(3, 4, 5, 1))
                ###
                
                for (v2 in 1:len) {
                  if (v1 == v2) next
                  for (v3 in 1:len) {
                    
                    ## skip loop if one of the axes are the same
                    if (v1 == v3 | v2  == v3) next
                    plot.idx <- plot.idx + 1
                    
                    ### update progress bar
                    #if (plot.idx %% (max.nhorizplots * max.nvertiplots) == 0) {
                    #	page.idx = page.idx + 1
                    #	info <- paste("Creating page ", page.idx, " / ", max.npages, sep="")
                    #	if (this$working) printf("%s with plot# %s", info, plot.idx)
                    #	setTkProgressBar(this$pb, page.idx, label=info)
                    #}
                    
                    #else {
                    #select columns to plot
                    tdata <- as.matrix(data[, c(v1, v2, v3)])
                    this$cnames <- c(colnames(data)[v1], colnames(data)[v2], colnames(data)[v3])
                    
                    if (cutoffs[v1] > 0) title.axis <- sprintf("%s (%s)", colnames(data)[v1], cutoffs[v1])
                    else title.axis <- colnames(data)[v1]
                    if (cutoffs[v2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", colnames(data)[v2], cutoffs[v2]))
                    else title.axis <- c(title.axis, colnames(data)[v2])
                    
                    # start plot
                    # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
                    # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
                    plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                      xlim = c(xminval - 0.5, xmaxval + 1),
                      ylim = c(yminval - 0.5, ymaxval + 1),
                      xlab = title.axis[1], ylab = title.axis[2],
                      cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
                    box(lwd=0.5, col="darkgrey")
                    
                    #add title for page in 3D-Overview tab
                    #mtext(title, outer = TRUE, cex = 1.5, line=1.3, pos=2, xpd=TRUE)
                    #if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                    
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
                    # q1 Quadrant unten links
                    # q2 Quadrant unten rechts
                    # q3 Quadrant oben rechts
                    # q4 Quadrant oben links
                    if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                      
                      ### count cells in quadrant
                      tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                      tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] < cutoffs[v2]), 3]
                      tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                      tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[v1] & tdata[, 2] >= cutoffs[v2]), 3]
                      
                      ### q[x].total [ink=black]
                      ### percentage of cells in quadrant to total cells 
                      ### or in MSI(+): percentage of cells in quadrant to total positive cells
                      this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                      this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                      this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                      this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                      
                      if (cutoffs[v3] > 0) {
                        ### number of cells which are producing cells in feature C
                        ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
                        
                        ### q[x].prodcells [ink=red]
                        ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                        this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
                        if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                        this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
                        if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                        this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
                        if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                        this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
                        if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                        
                        ### only do MSI plots on producing cells only
                        if (checkCALC == "MSI(+)") {
                          ### cut all cells which are not producing cells
                          tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                          ncells <- nrow(tdata.plus)
                          
                          tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                          tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                          tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                          tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                          
                          ### q[x].total [ink=blue]
                          ### in MSI(+): percentage of cells in quadrant to total positive cells
                          this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                          this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                          this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                          this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                        }
                        
                        ### q[x].prodcellsplus [ink=green]
                        ### percentage of cells which are positive for feature C to total cells
                        this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                        this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                        this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                        this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                      }
                    } 
                    
                    tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
                    ncells.zero <- nrow(tdata.zero)
                    
                    ### calc quadrants with only positive values
                    # q1 Quadrant unten links
                    # q2 Quadrant unten rechts
                    # q3 Quadrant oben rechts
                    # q4 Quadrant oben links
                    if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                      q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)      
                      q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)                    
                      q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] & tdata.zero[, 2] >= cutoffs[v2])) / ncells.zero)                    
                      q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
                    }
                    
                    #this$bintriplot(tdata, cutoffs[c(v1, v2, v3)], set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp
                    #	, binSize=binSize, mincells=mincount, quadrants.color = quadrants.col)
                    if (checkCALC == "density") {
                      this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                    } else if (checkCALC == "MSI(+)") {
                      this$bintriplot(data=tdata.plus, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, , overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
                    } else {
                      this$bintriplot(data=tdata, cutoffs=cutoffs[c(v1, v2, v3)], binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                    }
                    
                    if (this$OverviewGate) {this$ncell.perc = this$ncell.sel / this$origin.ncells * 100}
                    
                    # add title for single plot
                    if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                      if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s, %s/cof=%s", displayfile, this$ncell.perc, checkCALC, v3, this$current.cofactor)
                      else firstLine <- sprintf("%s: %s, cutoff=%s/cof=%s", displayfile, checkCALC, cutoffs[v3], this$current.cofactor)
                    } else {
                      if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
                      else firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
                    }
                    title(main=firstLine, line=3.2, cex.main=1.0 * label.cex, adj=0)
                    
                    secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
                    title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
                    
                    thirdLine <- ""
                    if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
                      thirdLine=sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
                    }
                    title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
                    
                    if (checkGATED == "1" | grepl("temp", file)) {
                      if (length(this$coords.info) > 2) {
                        fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
                        fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
                      } else {
                        fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
                        fifthLine <- ""
                      }
                      title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
                      title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
                    }
                  }
                  ### update progress bar
                  if ((plot.idx != 0) & ((plot.idx - 1) %% (max.nhorizplots * max.nvertiplots) == 0)) {
                    page.idx <- page.idx + 1
                    info <- paste("Creating page ", page.idx, " / ", max.npages, sep="")
                    if (this$working) printf("%s with plot# %s", info, plot.idx)
                    setTkProgressBar(this$pb, page.idx, label=info)
                  }
                  
                  #add title for page in 3D-Overview tab
                  if (current.page != page.idx) {
                    mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2, xpd=TRUE)
                    if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                    #printf("Print on Page %s", page.idx)
                    current.page <- page.idx
                  }
                  
                  ### plot emptry plots
                  printf("last plot #:%s with X:%s Y:%s; plotting %s more empty plots", plot.idx, v1, v2, res.nhorizplots)
                  if (res.nhorizplots > 0) {
                    for (i in 1:res.nhorizplots) {
                      plot(1, type="n", frame.plot=FALSE, axes=FALSE, ylab="")
                      plot.idx <- plot.idx + 1
                    }
                  }
                }
              }
       )
      }
      
      tkmessageBox(title = "Output of overview", 
                   message = paste ("Plots created in folder ", this$saveinFolder, sep=""))
    }
    
    tkconfigure(this$tt, cursor = "left_ptr")
    
    cat("\n")
    # close progress bar
    close(this$pb)
    
    #if (checkPNG == "1") printf("PNGs are saved in %s.", file.path(this$saveinFolder, "png"))
    
    print(Sys.time() - timeSTART)
    
    cat("\n>>>> Done.\n")
  }
  # }
  # }	
  this$OverviewGate=FALSE
}

fcs$dotriploTOverviewX <- function(table=NA) {
  ### triploTs Overviews only with Feature A on x axis
  this <- fcs
  answer <- tclVar("yes")
  freqans <- tclVar("no")
  
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  if (!this$working) {
    answer <- tkmessageBox(title = "Starting..", message = "Are you sure?",
    icon = "info", type = "yesno")
  }
  
  if (tclvalue(answer) == "yes") {
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
    ### if features are not in sample
    if (length(var1) == 0){
      tkmessageBox(title = "An error has occured!", 
                   message = "Check your Feature A.")
      stop("One of the features are not existent.")
    }
    
    var1.idx <- which(this$selected.vars == var1)
    cutoff.var1 <- this$checkDigits(cutoff_id=var1.idx)
    
    checkCALC <- tclvalue(this$rbcalc)
    checkTRANS <- tclvalue(this$rbtrans)
    checkGATED <- tclvalue(this$cbtgateData)
    checkGRID <- tclvalue(this$cbtshowGrid)
    checkTRIMMING <- tclvalue(this$cbttrimming)
    
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
    
    len <- length(this$selected.vars)
    
    # column and cutoff number vector
    colvec <- vector()
    cutoffs <- vector()
    for (i in 1:len) {
      if (tclvalue(this$cbVal[[i]]) == "1") {
        colvec <- c(colvec, i);
        cutoffs <- c(cutoffs, this$checkDigits(cutoff_id=i))
      }
    }
    ### if var1 is already checked in cutoff panel
    if (any(var1.idx == colvec)) {
      colvec.var1.idx <- which(var1.idx == colvec)
      colvec <- colvec[-colvec.var1.idx]
      cutoffs <- cutoffs[-colvec.var1.idx]
    }
    colvec <- c(var1.idx, colvec)
    cutoffs <- c(cutoff.var1, cutoffs)
    len.colvec <- length(colvec)
    # title length
    titlelen <- length(unlist(strsplit(tclvalue(tkget(this$title, "1.0", "end-1c")), "")))
    titleans <- tclVar("yes")
    
    if (len.colvec < 3) {
      tkmessageBox(title = "An error has occured!", 
                   message = "Please select at least three markers.", icon = "error", type = "ok")
      stop("Please select at least three markers.")
    }
    if (titlelen <= 1 & this$working == FALSE) {
      titleans = tkmessageBox(title = "Are you sure?", 
                              message = "Do not forget to set page title. Continue?", icon = "info", type = "yesno")
    } else if (checkCALC == "freq" & !is.integer(which(cutoffs == 0)) & this$working == FALSE) {
      freqans = tkmessageBox(title = "Are you sure?", 
                             message = "Do not forget to set cutoffs. Continue?", icon = "info", type = "yesno")
    } else if (checkCALC == "density") {
      tkmessageBox(title = "For your information.", 
                   message = "It is not meaningful to plot method \"density\".", icon = "error", type = "ok")
      stop("Chose another statistical method.")
    }
    
    if ((tclvalue(titleans) == "yes") | tclvalue(freqans) == "yes") {
      if (this$working) this$saveinFolder <- getwd()
      else this$saveinFolder <- tk_choose.dir(default = getwd(), caption = "Select directory to save the PDFs")
      
      
      if (!is.na(this$saveinFolder)) {
        tkconfigure(this$tt, cursor = "watch")
        
        max.nhorizplots <- 6
        max.nvertiplots <- 8
        res.nhorizplots <- (len.colvec - 2) %% max.nhorizplots
        if (res.nhorizplots != 0) res.nhorizplots <- max.nhorizplots - res.nhorizplots
        max.nplots <- (len.colvec - 2) * (len.colvec - 3)
        max.npages <- ceiling((max.nplots + res.nhorizplots * (len.colvec - 1)) / (max.nhorizplots * max.nvertiplots))
        
        ### Create Progress Bar
        this$pb <- tkProgressBar(title="triploT overview", label=paste("Creating page 1 / ", max.npages, sep=""), min = 0, max = max.npages, initial = 1, width = 300)
        # Set progress bar
        setTkProgressBar(this$pb, 1, label=paste("Creating page 1 / ", max.npages, sep=""))
        
        timeSTART <- Sys.time()
        if (!this$OverviewGate) cat("\n\n>>>> Start triploTOverview with total data: \n\n")
        else cat("\n\n>>>> Start triploTOverview with gated data: \n\n")
        if (this$working) printf("w: %s - time started", timeSTART)
        
        tmp.folder <- file.path(this$saveinFolder, "tmp")
        if (grepl("linux", sessionInfo()$R.version$os)) {
          ### on linux
          system(paste("mkdir -p -v ", tmp.folder, sep=""))
        } else {
          ### on Windows
          dir.create(tmp.folder)
          printf("Creating folder: %s", tmp.folder)
          #system(paste("mkdir ", tmp.folder, sep=""))
          # or shell()?
        }
        
        file <- tclvalue(tkget(this$tkchoosefile))
        if (this$OverviewGate) {
          displayfile <- this$shortenFilename(this$plot.attr[[1]]$file.name, title=TRUE)
        } else {
          displayfile <- this$shortenFilename(file, title=TRUE)
        }
        
        table <- this$total.projects[this$selected.projectnum]
        if (grepl("temp", file)) {
          file.idx <- 1
          table <- file
        } else {
          file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
          this$selected.filenum <- file.idx
        }
        
        if (this$working) printf("w: table=%s file.idx=%s", table, file.idx)
        
        binSize <- as.numeric(tkget(this$binSize))
        mincount <- as.numeric(tkget(this$minCountTri))
        xminval <- as.numeric(tkget(this$minvalX))
        xmaxval <- as.numeric(tkget(this$maxvalX))
        yminval <- as.numeric(tkget(this$minvalY))
        ymaxval <- as.numeric(tkget(this$maxvalY))
        
        
        #### Do not trim and remove doublets if data is gated
        if (this$OverviewGate) {
          # if data is gated, just recall
          data <- this$getData(table, file.idx, columns=colvec)
        } else if (checkTRIMMING == "1") {
          this$preprocData(mode="trim")
          data <- this$data[, colvec]
        } else {
          this$getFile(table, file.idx)
          data <- this$data[, colvec]
        }
        
        len <- dim(data)[2]
        
        printf("Fixed Feature A: %s", var1)
        printf("Columns selected (%s): %s", dim(data)[2], paste(colvec, collapse=" "))
        printf("Column names: %s", paste(colnames(data), collapse=" "))
        
        title <- as.character(tclvalue(tkget(this$title, "1.0", "end-1c")))
        
        
        toPDF(file=sprintf("%s_triploTOverviewX_%s_%s%s_%s.pdf", displayfile, var1, len.colvec - 1, checkCALC, this$version), 
              path=tmp.folder, 
              title=sprintf("triploTOverviewX of %s", displayfile), 
              ### 
              width=3.21 * max.nhorizplots, 
              height=3.5 * max.nvertiplots, 
              pointsize=11, {
                # set progress bar
                #setTkProgressBar(this$pb, 2, label=paste("Creating page 2 / ", max.npages + 2, sep=""))
                # plot density plot
                #this$plotDensities(plotter="triover", pdf=TRUE)
                
                ##### start triploT overview
                
                ### label and axes font sizes
                label.cex <- 1.1 - 0.5 * this$legend.space
                set.cex.axes <- 1
                set.mgp <- c(1.9, 0.5, 0)
                par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 5, 1), mar=c(3, 4, 5, 1))
                ###
                
                #add title for page in 3D-Overview tab
                #mtext(title, outer = TRUE, cex = 1.5, line=1.3, pos=2, xpd=TRUE)
                if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                
                plot.idx <- 0
                page.idx <- 0
                current.page <- 0
                for (v2 in 2:len){
                  for (v3 in 2:len){
                    
                    ## skip loop if one of the axes are the same
                    if (v2  == v3) next
                    
                    ### update progress bar
                    if (plot.idx %% (max.nhorizplots * max.nvertiplots) == 0) {
                      page.idx <- page.idx + 1
                      info <- paste("Creating page ", page.idx, " / ", max.npages, sep="")
                      print(info)
                      printf("plot #:%s", plot.idx)
                      setTkProgressBar(this$pb, page.idx, label=info)
                    }
                    
                    #select columns to plot
                    tdata <- as.matrix(data[c(1, v2, v3)])
                    this$cnames <- colnames(tdata)
                    
                    if (cutoff.var1 > 0) title.axis <- sprintf("%s(%s)", var1, cutoff.var1)
                    else title.axis <- var1
                    if (cutoffs[2] > 0) title.axis <- c(title.axis, sprintf("%s(%s)", colnames(data)[v2], cutoffs[v2]))
                    else title.axis <- c(title.axis, colnames(data)[v2])
                    
                    # start plot
                    # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
                    # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
                    plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                      xlim = c(xminval - 0.5, xmaxval + 1),
                      ylim = c(yminval - 0.5, ymaxval + 1),
                      xlab = title.axis[1], ylab = title.axis[2],
                      cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
                    box(lwd=0.5, col="darkgrey")
                    
                    plot.idx <- plot.idx + 1
                    
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
                    # q1 Quadrant unten links
                    # q2 Quadrant unten rechts
                    # q3 Quadrant oben rechts
                    # q4 Quadrant oben links
                    if (cutoff.var1 > 0 & cutoffs[v2] > 0) {
                      
                      ### count cells in quadrant
                      tdata.q1 <- tdata[which(tdata[, 1] < cutoff.var1 & tdata[, 2] < cutoffs[v2]), 3]
                      tdata.q2 <- tdata[which(tdata[, 1] >= cutoff.var1 & tdata[, 2] < cutoffs[v2]), 3]
                      tdata.q3 <- tdata[which(tdata[, 1] >= cutoff.var1 & tdata[, 2] >= cutoffs[v2]), 3]
                      tdata.q4 <- tdata[which(tdata[, 1] < cutoff.var1 & tdata[, 2] >= cutoffs[v2]), 3]
                      
                      ### q[x].total [ink=black]
                      ### percentage of cells in quadrant to total cells 
                      ### or in MSI(+): percentage of cells in quadrant to total positive cells
                      this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                      this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                      this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                      this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                      
                      if (cutoffs[v3] > 0) {
                        ### number of cells which are producing cells in feature C
                        ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
                        
                        ### q[x].prodcells [ink=red]
                        ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                        this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
                        if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                        this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
                        if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                        this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
                        if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                        this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
                        if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                        
                        ### only do MSI plots on producing cells only
                        if (checkCALC == "MSI(+)") {
                          ### cut all cells which are not producing cells
                          tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                          ncells <- nrow(tdata.plus)
                          
                          tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                          tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                          tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                          tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                          
                          ### q[x].total [ink=blue]
                          ### in MSI(+): percentage of cells in quadrant to total positive cells
                          this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                          this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                          this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                          this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                        }
                        
                        ### q[x].prodcellsplus [ink=green]
                        ### percentage of cells which are positive for feature C to total cells
                        this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                        this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                        this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                        this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
                        if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                      }
                    }
                    
                    tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
                    ncells.zero <- nrow(tdata.zero)
                    
                    ### calc quadrants with only positive values
                    # q1 Quadrant unten links
                    # q2 Quadrant unten rechts
                    # q3 Quadrant oben rechts
                    # q4 Quadrant oben links
                    if (cutoff.var1 > 0 & cutoffs[v2] > 0) {
                      q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoff.var1 & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)      
                      q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoff.var1 & tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)                    
                      q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoff.var1 & tdata.zero[, 2] >= cutoffs[v2])) / ncells.zero)                    
                      q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
                    }
                    
                    if (checkCALC == "density") {
                      this$bintriplot(data=tdata, cutoffs=c(cutoff.var1, cutoffs[c(v2, v3)]), density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                    } else if (checkCALC == "MSI(+)") {
                      this$bintriplot(data=tdata.plus, cutoffs=c(cutoff.var1, cutoffs[c(v2, v3)]), binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
                    } else {
                      this$bintriplot(data=tdata, cutoffs=c(cutoff.var1, cutoffs[c(v2, v3)]), binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                    }
                    
                    if (this$OverviewGate) {
                    this$ncell.perc <- this$ncell.sel / this$origin.ncells * 100
                    }
                    
                    # add title for single plot
                    if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                      if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s(%s)/cof=%s", displayfile, this$ncell.perc, checkCALC, this$cnames[3], this$current.cofactor)
                      else firstLine <- sprintf("%s: %s(%s)/cof=%s", displayfile, checkCALC, this$cnames[3], this$current.cofactor)
                    } else {
                      if (this$OverviewGate) {
                        firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
                      } else {
                        firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
                      }
                    }
                    title(main=firstLine, line=3.2, cex.main=1.0 * label.cex, adj=0)
                    
                    secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
                    title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
                    
                    thirdLine <- ""
                    if (cutoff.var1 > 0 & cutoffs[v2] > 0) {
                      thirdLine=sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
                    }
                    title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
                    
                    if (checkGATED == "1" | grepl("temp", file)) {
                      if (length(this$coords.info) > 2) {
                        fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
                        fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
                      } else {
                        fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
                        fifthLine <- ""
                      }
                      title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
                      title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
                    }
                  }
                  #add title for page in 3D-Overview tab
                  if (current.page != page.idx) {
                    mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2, xpd=TRUE)
                    if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                    #printf("Print on Page %s", page.idx)
                    current.page <- page.idx
                  }
                  if (res.nhorizplots > 0) {
                    for (i in 1:res.nhorizplots) {
                      plot(1, type="n", frame.plot=FALSE, axes=FALSE, ylab=""); 
                      plot.idx <- plot.idx + 1;
                    }
                  }
                }
                
                ### plot histograms
                this$plotHistograms(plotter="triover", pdf=TRUE)
              })
        tkconfigure(this$tt, cursor = "left_ptr")
        
        cat("\n")
        # close progress bar
        close(this$pb)
        if (!this$working) tkmessageBox(title = "Output of overview", 
                                        message = paste ("Plots created in folder ", tmp.folder, sep=""))
        
        print(Sys.time() - timeSTART)
        cat("\n>>>> Done.\n")
      }
    }
  }	
  this$OverviewGate <- FALSE
}

fcs$dotriploTOverviewXY <- function(table=NA) {
  ### triploTs Overviews only with Feature A on x axis and feature Y on y axis
  this <- fcs
  answer <- tclVar("yes")
  
  ### if axes ranges are not the same
  if (!this$checkAxesRange()) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please set x and y axis with the same range.", icon = "error", type = "ok")
    stop("Set x and y axis with the same range.")
  }
  
  ### if Feature A or Y is not in sample
  var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
  var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
  if (length(var1) == 0 | length(var2) == 0){
    tkmessageBox(title = "An error has occured!", 
                 message = "Check your Feature A or Y.")
    stop("One of the features are not existent.")
  }
  
  var1.idx <- which(this$selected.vars == var1)
  cutoff.var1 <- this$checkDigits(cutoff_id=var1.idx)
  var2.idx <- which(this$selected.vars == var2)
  cutoff.var2 <- this$checkDigits(cutoff_id=var2.idx)
  
  file <- tclvalue(tkget(this$tkchoosefile))
  displayfile <- this$shortenFilename(file)
  table <- this$total.projects[this$selected.projectnum]
  if (grepl("temp", file)) {
    file.idx <- 1
    table <- file
  } else {
    file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
    this$selected.filenum <- file.idx
  }
  if (this$working) printf("w: table=%s file.idx=%s", table, file.idx)
  
  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  checkCALC <- tclvalue(this$rbcalc)
  checkTRANS <- tclvalue(this$rbtrans)
  checkGATED <- tclvalue(this$cbtgateData)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkTRIMMING <- tclvalue(this$cbttrimming)
  
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
  
  len <- length(this$selected.vars)
  
  ### select checked features
  # column and cutoff vector
  colvec <- vector()
  cutoffs <- vector()
  for (i in 1:len) {
    if (tclvalue(this$cbVal[[i]]) == "1") {
      colvec <- c(colvec, i);
      cutoffs <- c(cutoffs, this$checkDigits(cutoff_id=i))
    }
  }
  
  ### if method is freq or MSI(+), cutoff(z) needs to be setted
  if (any(cutoffs == 0) & (checkCALC == "freq" | checkCALC == "MSI(+)")) {
    tkmessageBox(title = "An error has occured in method: MSI(+) or freq!", 
                 message = "You forgot to set cutoff for some features C.", icon = "error", type = "ok")
    stop("Missing production cutoff for features C.")
  } else if (checkCALC == "density") {
    tkmessageBox(title = "For your information.", 
                 message = "It is not meaningful to plot method \"density\".", icon = "error", type = "ok")
    stop("Chose another statistical method.")
  }
  
  ### if var1 is already checked in cutoff panel, then remove
  if (any(var1.idx == colvec)) {
    colvec.var1.idx <- which(var1.idx == colvec)
    colvec <- colvec[-colvec.var1.idx]
    cutoffs <- cutoffs[-colvec.var1.idx]
  }
  ### if var2 is already checked in cutoff panel, then remove
  if (any(var2.idx == colvec)) {
    colvec.var2.idx <- which(var2.idx == colvec)
    colvec <- colvec[-colvec.var2.idx]
    cutoffs <- cutoffs[-colvec.var2.idx]
  }
  colvec <- c(var1.idx, var2.idx, colvec)
  cutoffs <- c(cutoff.var1, cutoff.var2, cutoffs)
  len.colvec <- length(colvec)
  
  # title length
  titlelen <- length(unlist(strsplit(tclvalue(tkget(this$title, "1.0", "end-1c")), "")))
  titleans <- tclVar("yes")
  
  ### if there is no feature C to plot
  if (len.colvec < 3) {
    tkmessageBox(title = "An error has occured!", 
                 message = "Please check at least one feature to the left.", icon = "error", type = "ok")
    stop("Select one or more features to the left.")
  }
  
  #### new getFile if current file is not recent one
  timeSTART <- Sys.time()
  
  if (is.null(this$data) | this$current.project != table | this$current.filenum != file.idx | 
       this$current.trans != tclvalue(this$rbtrans) | this$current.cofactor != as.numeric(tclvalue(this$rbasinh))) {
    if (this$working) print("Time loading data:")
    this$getFile(table, file.idx)
    if (checkTRIMMING == "1") this$preprocData(mode="trim")
    if (this$working) print(Sys.time() - timeSTART)
  }
  data <- this$data[, colvec]
  
  ### if percentage is checked
  # calculate cutoffs and set check button to zero
  for (i in 1:length(cutoffs)) {
    if (tclvalue(this$cbcutoffperc[[colvec[i]]]) == "1") {
      cutoffs[colvec[i]] <- this$calcCUTOFF(this$data[colvec[i]], this$checkDigits(cutoff_id=colvec[i]), colnames(this$data[colvec[i]]), cutoffs[colvec[i]])
    }
  }
  ####
  
  printf("Fixed Feature A: %s", var1)
  printf("Fixed feature Y: %s", var2)
  printf("Columns selected (%s): %s", dim(data)[2], paste(colvec, collapse=" "))
  printf("Column names: %s", paste(colnames(data), collapse=" "))
  
  
  ### plot triploTs in plot window if there are not many features to plot
  plot.ncol <- as.numeric(tclvalue(this$vncol))
  plot.nrow <- as.numeric(tclvalue(this$vnrow))
  
  ### label and axes font sizes
  label.cex <- 1.1 - 0.5 * this$legend.space
  set.cex.axes <- 1
  set.mgp <- c(1.9, 0.5, 0)
  plot.idx <- 0
  
  if ((len.colvec - 2) <= (plot.ncol * plot.nrow)) {
    ### plot triplots in plot window
    dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
    if (length(which(dev.label == names(devList()))) == 0) {
      this$plotter.tri.num <- this$plotter.tri.num + 1
      dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
      devNew(type="x11", title="n-triploTs", width=plot.ncol * 3.4, height=plot.nrow * 3.7, label=dev.label)
      # mar in points, mai in inches
      # oma adds title lines
      # order: bottom, left, top, and right
      par(mfrow=c(plot.nrow, plot.ncol), oma=c(0.5, 1, 2, 1), mar=c(3, 3, 4, 2))
      
      this$plot.windows <- c(this$plot.windows, dev.label)
    } else {
      devSet(devList()[which(dev.label == names(devList()))])
    }
    
    
    for (v3 in 3:len.colvec){
      #select columns to plot
      tdata <- as.matrix(data[c(1, 2, v3)])
      this$cnames <- colnames(tdata)
      
      ### calculate cells which were not plotted 
      cells.overmaxFI <- length(which(tdata[, 1] > xmaxval | tdata[, 2] > ymaxval))
      cells.underminFI <- length(which(tdata[, 1] < xminval | tdata[, 2] < yminval))
      cells.overmaxFI.perc <- round(100 * cells.overmaxFI / (dim(tdata)[1] - cells.underminFI))
      ### warn if more then 5% productive cells (q2 + q3 + q4) werent plotted
      if (cells.overmaxFI.perc >= 5 & !this$working) {
        tkmessageBox(title = "Warning!", 
                     message = sprintf("Your cells exceed %s%% of your plot max ranges. You might want to increase your max ranges.", cells.overmaxFI.perc), 
                     icon = "info", type = "ok")
      }
      
      if (cutoff.var1 > 0) title.axis <- sprintf("%s(%s)", var1, cutoff.var1)
      else title.axis <- var1
      if (cutoff.var2 > 0) title.axis <- c(title.axis, sprintf("%s(%s)", var2, cutoff.var2))
      else title.axis <- c(title.axis, var2)
      
      # start plot
      # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
      # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
      plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
        xlim = c(xminval - 0.5, xmaxval + 1),
        ylim = c(yminval - 0.5, ymaxval + 1),
        xlab = title.axis[1], ylab = title.axis[2],
        cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
      box(lwd=0.5, col="darkgrey")
      
      plot.idx <- plot.idx + 1
      
      
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
      # q1 Quadrant unten links
      # q2 Quadrant unten rechts
      # q3 Quadrant oben rechts
      # q4 Quadrant oben links
      if (cutoff.var1 > 0 & cutoff.var2 > 0) {
        ### count cells in quadrant
        tdata.q1 <- tdata[which(tdata[, 1] < cutoff.var1 & tdata[, 2] < cutoff.var2), 3]
        tdata.q2 <- tdata[which(tdata[, 1] >= cutoff.var1 & tdata[, 2] < cutoff.var2), 3]
        tdata.q3 <- tdata[which(tdata[, 1] >= cutoff.var1 & tdata[, 2] >= cutoff.var2), 3]
        tdata.q4 <- tdata[which(tdata[, 1] < cutoff.var1 & tdata[, 2] >= cutoff.var2), 3]
        
        ### q[x].total [ink=black]
        ### percentage of cells in quadrant to total cells 
        ### or in MSI(+): percentage of cells in quadrant to total positive cells
        this$q1.total <- abs(100 * length(tdata.q1) / ncells)
        this$q2.total <- abs(100 * length(tdata.q2) / ncells)
        this$q3.total <- abs(100 * length(tdata.q3) / ncells)
        this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
        
        if (cutoffs[v3] > 0) {
          ### number of cells which are producing cells in feature C
          ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
          
          ### q[x].prodcells [ink=red]
          ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
          this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
          if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
          this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
          if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
          this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
          if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
          this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
          if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
          
          ### only do MSI plots on producing cells only
          if (checkCALC == "MSI(+)") {
            ### cut all cells which are not producing cells
            tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
            ncells <- nrow(tdata.plus)
            
            tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
            tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
            tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
            tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
            
            ### q[x].total [ink=blue]
            ### in MSI(+): percentage of cells in quadrant to total positive cells
            this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
            this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
            this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
            this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
          }
          
          ### q[x].prodcellsplus [ink=green]
          ### percentage of cells which are positive for feature C to total cells
          this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
          if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
          this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
          if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
          this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
          if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
          this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
          if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
        }
      }
      
      tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
      ncells.zero <- nrow(tdata.zero)
      
      ### calc quadrants with only positive values
      # q1 Quadrant unten links
      # q2 Quadrant unten rechts
      # q3 Quadrant oben rechts
      # q4 Quadrant oben links
      if (cutoff.var1 > 0 & cutoff.var2 > 0) {
        q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoff.var1 & tdata.zero[, 2] < cutoff.var2)) / ncells.zero)      
        q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoff.var1 & tdata.zero[, 2] < cutoff.var2)) / ncells.zero)                    
        q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoff.var1 & tdata.zero[, 2] >= cutoff.var2)) / ncells.zero)                    
        q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
      }
      
      if (checkCALC == "density") {
        this$bintriplot(data=tdata, cutoffs=c(cutoff.var1, cutoff.var2, cutoffs[v3]), density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
      } else if (checkCALC == "MSI(+)") {
        this$bintriplot(data=tdata.plus, cutoffs=c(cutoff.var1, cutoff.var2, cutoffs[v3]), binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
      } else {
        this$bintriplot(data=tdata, cutoffs=c(cutoff.var1, cutoff.var2, cutoffs[v3]), binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
      }
      if (this$OverviewGate) {
        this$ncell.perc <- this$ncell.sel / this$origin.ncells * 100
      }
      
      # add title for single plot
      if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
        if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s, %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$cnames[3], this$current.cofactor)
        else firstLine <- sprintf("%s: %s, %s/cof=%s", displayfile, checkCALC, this$cnames[3], this$current.cofactor)
      } else {
        if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
        else firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
      }
      title(main=firstLine, line=3.2, cex.main=0.9 * label.cex, adj=0)
      
      secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
      title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
      
      thirdLine <- ""
      if (cutoff.var1 > 0 & cutoff.var2 > 0) {
        thirdLine=sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
      }
      title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
      
      if (checkGATED == "1" | grepl("temp", file)) {
        if (length(this$coords.info) > 2) {
          fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
          fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
        } else {
          fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
          fifthLine <- ""
        }
        title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
        title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
      }
    }
    
    ### DONE plot triploTs in plot window
    
    
  } else {
    ### plot triploTs in pdf
    if (titlelen <= 1 & this$working == FALSE) {
      titleans <- tkmessageBox(title = "Are you sure?", 
                              message = "Do not forget to set page title. Continue?", icon = "info", type = "yesno")
    } 
    
    if (tclvalue(titleans) == "yes") {
      if (this$working) this$saveinFolder <- getwd()
      else this$saveinFolder <- tk_choose.dir(default = getwd(), caption = "Select directory to save the PDFs")
      
      if (!is.na(this$saveinFolder)) {
        tkconfigure(this$tt, cursor = "watch")
        
        max.nhorizplots <- 6
        max.nvertiplots <- ceiling((len.colvec - 2) / max.nhorizplots)
        
        timeSTART <- Sys.time()
        cat("\n\n>>>> Start triploTOverviewXY with total data: \n\n")
        if (this$working) printf("w: %s - time started", timeSTART)
        
        toPDF(file=sprintf("%s_triploTOverviewXY_%s_%s_%s%s_%s.pdf", displayfile, var1, var2, len.colvec - 2, checkCALC, this$version), 
              path=this$saveinFolder, 
              title=sprintf("triploTOverviewXY of %s", displayfile), 
              ### 
              width=3.21 * max.nhorizplots, 
              height=3.5 * max.nvertiplots, 
              pointsize=11, {
                ##### start triploT-OverviewXY
                par(mfrow=c(max.nvertiplots, max.nhorizplots), oma=c(0.5, 1, 5, 1), mar=c(3, 4, 5, 1))
                ###
              
                #add title for page in 3D-Overview tab
                #mtext(title, outer = TRUE, cex = 1.5, line=1.3, pos=2, xpd=TRUE)
                if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                
                page.idx <- 0
                current.page <- 0
                for (v3 in 3:len.colvec){
                  #select columns to plot
                  tdata <- as.matrix(data[c(1, 2, v3)])
                  this$cnames <- colnames(tdata)
                  
                  ### calculate cells which were not plotted 
                  cells.overmaxFI <- length(which(tdata[, 1] > xmaxval | tdata[, 2] > ymaxval))
                  cells.underminFI <- length(which(tdata[, 1] < xminval | tdata[, 2] < yminval))
                  cells.overmaxFI.perc <- round(100 * cells.overmaxFI / (dim(tdata)[1] - cells.underminFI))
                  ### warn if more then 5% productive cells (q2 + q3 + q4) werent plotted
                  if (cells.overmaxFI.perc >= 5 & !this$working) {
                    tkmessageBox(title = "Warning!", 
                                 message = sprintf("Your cells exceed %s%% of your plot max ranges. You might want to increase your max ranges.", cells.overmaxFI.perc), 
                                 icon = "info", type = "ok")
                  }
                  
                  if (cutoff.var1 > 0) title.axis <- sprintf("%s(%s)", var1, cutoff.var1)
                  else title.axis <- var1
                  if (cutoff.var2 > 0) title.axis <- c(title.axis, sprintf("%s(%s)", var2, cutoff.var2))
                  else title.axis <- c(title.axis, var2)
                  
                  # start plot
                  # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
                  # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
                  plot(1, type = "n", frame.plot = FALSE, axes = FALSE,
                    xlim = c(xminval - 0.5, xmaxval + 1),
                    ylim = c(yminval - 0.5, ymaxval + 1),
                    xlab = title.axis[1], ylab = title.axis[2],
                    cex.lab=label.cex, cex.axis=set.cex.axes, mgp=set.mgp)
                  box(lwd=0.5, col="darkgrey")
                  
                  plot.idx <- plot.idx + 1
                  
                  
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
                  # q1 Quadrant unten links
                  # q2 Quadrant unten rechts
                  # q3 Quadrant oben rechts
                  # q4 Quadrant oben links
                  if (cutoff.var1 > 0 & cutoff.var2 > 0) {
                    ### count cells in quadrant
                    tdata.q1 <- tdata[which(tdata[, 1] < cutoff.var1 & tdata[, 2] < cutoff.var2), 3]
                    tdata.q2 <- tdata[which(tdata[, 1] >= cutoff.var1 & tdata[, 2] < cutoff.var2), 3]
                    tdata.q3 <- tdata[which(tdata[, 1] >= cutoff.var1 & tdata[, 2] >= cutoff.var2), 3]
                    tdata.q4 <- tdata[which(tdata[, 1] < cutoff.var1 & tdata[, 2] >= cutoff.var2), 3]
                    
                    ### q[x].total [ink=black]
                    ### percentage of cells in quadrant to total cells 
                    ### or in MSI(+): percentage of cells in quadrant to total positive cells
                    this$q1.total <- abs(100 * length(tdata.q1) / ncells)
                    this$q2.total <- abs(100 * length(tdata.q2) / ncells)
                    this$q3.total <- abs(100 * length(tdata.q3) / ncells)
                    this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                    
                    if (cutoffs[v3] > 0) {
                      ### number of cells which are producing cells in feature C
                      ncells <- nrow(tdata[which(tdata[, 3] >  cutoffs[v3]), ])
                      
                      ### q[x].prodcells [ink=red]
                      ### percentage of cells which are positive for feature C in quadrant to total quadrant cells
                      this$q1.prodcells <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / length(tdata.q1)
                      if (is.nan(this$q1.prodcells)) this$q1.prodcells <- 0
                      this$q2.prodcells <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / length(tdata.q2)
                      if (is.nan(this$q2.prodcells)) this$q2.prodcells <- 0
                      this$q3.prodcells <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / length(tdata.q3)
                      if (is.nan(this$q3.prodcells)) this$q3.prodcells <- 0
                      this$q4.prodcells <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / length(tdata.q4)
                      if (is.nan(this$q4.prodcells)) this$q4.prodcells <- 0
                      
                      ### only do MSI plots on producing cells only
                      if (checkCALC == "MSI(+)") {
                        ### cut all cells which are not producing cells
                        tdata.plus <- tdata[which(tdata[, 3] >  cutoffs[3]), ]
                        ncells <- nrow(tdata.plus)
                        
                        tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                        tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2]), 3]
                        tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                        tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] >= cutoffs[2]), 3]
                        
                        ### q[x].total [ink=blue]
                        ### in MSI(+): percentage of cells in quadrant to total positive cells
                        this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                        this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] < cutoffs[2])) / ncells)
                        this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] & tdata.plus[, 2] >= cutoffs[2])) / ncells)
                        this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
                      }
                      
                      ### q[x].prodcellsplus [ink=green]
                      ### percentage of cells which are positive for feature C to total cells
                      this$q1.prodcellsplus <- 100 * length(tdata.q1[which(tdata.q1 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q1.prodcellsplus)) this$q1.prodcellsplus <- 0
                      this$q2.prodcellsplus <- 100 * length(tdata.q2[which(tdata.q2 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q2.prodcellsplus)) this$q2.prodcellsplus <- 0
                      this$q3.prodcellsplus <- 100 * length(tdata.q3[which(tdata.q3 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q3.prodcellsplus)) this$q3.prodcellsplus <- 0
                      this$q4.prodcellsplus <- 100 * length(tdata.q4[which(tdata.q4 >= cutoffs[v3])]) / ncells.total
                      if (is.nan(this$q4.prodcellsplus)) this$q4.prodcellsplus <- 0
                    }
                  }
                  
                  tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {all(row > 0)}), ]
                  ncells.zero <- nrow(tdata.zero)
                  
                  ### calc quadrants with only positive values
                  # q1 Quadrant unten links
                  # q2 Quadrant unten rechts
                  # q3 Quadrant oben rechts
                  # q4 Quadrant oben links
                  if (cutoff.var1 > 0 & cutoff.var2 > 0) {
                    q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoff.var1 & tdata.zero[, 2] < cutoff.var2)) / ncells.zero)      
                    q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoff.var1 & tdata.zero[, 2] < cutoff.var2)) / ncells.zero)                    
                    q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoff.var1 & tdata.zero[, 2] >= cutoff.var2)) / ncells.zero)                    
                    q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
                  }
                  
                  if (checkCALC == "density") {
                    this$bintriplot(data=tdata, cutoffs=c(cutoff.var1, cutoff.var2, cutoffs[v3]), density=TRUE, binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                  } else if (checkCALC == "MSI(+)") {
                    this$bintriplot(data=tdata.plus, cutoffs=c(cutoff.var1, cutoff.var2, cutoffs[v3]), binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
                  } else {
                    this$bintriplot(data=tdata, cutoffs=c(cutoff.var1, cutoff.var2, cutoffs[v3]), binSize=binSize, mincells=mincount, overview=TRUE, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
                  }
                  if (this$OverviewGate) {
                    this$ncell.perc <- this$ncell.sel / this$origin.ncells * 100
                  }
                  
                  # add title for single plot
                  if (checkCALC == "freq" | grepl("MSI", checkCALC)) {
                    if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s(%s)/cof=%s", displayfile, this$ncell.perc, checkCALC, this$cnames[3], this$current.cofactor)
                    else firstLine <- sprintf("%s: %s(%s)/cof=%s", displayfile, checkCALC, this$cnames[3], this$current.cofactor)
                  } else {
                    if (this$OverviewGate) firstLine <- sprintf("%s(%0.1f%%): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
                    else firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
                  }
                  title(main=firstLine, line=3.2, cex.main=0.9 * label.cex, adj=0)
                  
                  secondLine <- sprintf("%s-%s(%0.1f%%); min/max=%s/%s; binSize=%s, #bins=%s", ncells.total, ncells.zero, (ncells.zero / ncells * 100), mincount, this$maxcells, binSize, this$bincount)
                  title(main=secondLine, line=2.4, cex.main=0.7 * label.cex, adj=0)
                  
                  thirdLine <- ""
                  if (cutoff.var1 > 0 & cutoff.var2 > 0) {
                    thirdLine=sprintf("Q1=%0.1f / %0.1f Q2=%0.1f / %0.1f Q3=%0.1f / %0.1f Q4=%0.1f / %0.1f", q1.zero, this$q1.total, q2.zero, this$q2.total, q3.zero, this$q3.total, q4.zero, this$q4.total)
                  }
                  title(main=thirdLine, line=1.6, cex.main=0.7 * label.cex, adj=0)
                  
                  if (checkGATED == "1" | grepl("temp", file)) {
                    if (length(this$coords.info) > 2) {
                      fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
                      fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
                    } else {
                      fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
                      fifthLine <- ""
                    }
                    title(main=fourthLine, line=0.9, cex.main=0.6 * label.cex, adj=0)
                    title(main=fifthLine, line=0.1, cex.main=0.6 * label.cex, adj=0)
                  }
                }
                #add title for page in 3D-Overview tab
                if (current.page != page.idx) {
                  mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2, xpd=TRUE)
                  if (length(this$coords.info>0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
                  #printf("Print on Page %s", page.idx)
                  current.page <- page.idx
                }
              })
        tkconfigure(this$tt, cursor = "left_ptr")
        
        cat("\n")
        if (!this$working) tkmessageBox(title = "Output of overview", 
                                        message = paste ("Plots created in folder ", this$saveinFolder, sep=""))
        
        print(Sys.time() - timeSTART)
        cat("\n>>>> Done.\n")
        this$OverviewGate=FALSE
      }
    }
  }	
}

fcs$bintriplot <- function(
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
  
  prodcells.color <- "red"
  prodpluscells.color <- "chartreuse4"
  
  if (is.na(file)) file <- tclvalue(tkget(this$tkchoosefile))
  
  displayfile <- this$shortenFilename(file)
  
  metadatafile <- sprintf("%s_PRItrivis_metadata.csv", this$current.project)
  if (!file.exists(metadatafile)) {
    header <- c("date", "sample", "cofactor", "calc",
              "feat.A", "feat.B", "feat.C", "absRange.C",
              "min.A", "max.A", "min.B", "max.B",
              "cutoff.A", "cutoff.B", "cutoff.C",
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
  min.MSI <- as.double(tclvalue(this$vminMSI))
  max.MSI <- as.double(tclvalue(this$vmaxMSI))
  
  if (max.MSI < min.MSI) {
    tmp <- min.MSI 
    min.MSI <- max.MSI
    max.MSI <- tmp
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
  checkSHOWMINBIN <- tclvalue(this$cbtshowMinBins)
  
  if (this$working) printf("w: do bintriplot density=%s checkCALC=%s", density, checkCALC)
  
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
    cols <- this$col.rainbow
  }
  
  # colors for bins <mincount
  cols.pale <- this$col.rainbow.pale
  
  # legend title
  tmp <- unlist(strsplit(colnames(data)[3], "\\."))
  if (length(tmp) > 1 & all(lengths(tmp) > 1)) {
    if (cutoffs[3] > 0) legend.title <- sprintf("%s(%s)", tmp[1], cutoffs[3]) 
    else legend.title <- tmp[1]
  } else {
    if (cutoffs[3] > 0) legend.title <- sprintf("%s(%s)", colnames(data)[3], cutoffs[3]) 
    else legend.title <- colnames(data)[3]
  }
  
  # boolean for only grey plot 
  # if MSI(+) mode and there are no colorful bins to display
  grey.label <- TRUE
  my.LENGTHS <- FALSE
  
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
    
  ### construct bins for MSI(+)
  if (checkCALC == "MSI(+)") {
    fX.origin <- cut(data.origin[, 1], breaks=seq(xmin.val, xmax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
    fY.origin <- cut(data.origin[, 2], breaks=seq(ymin.val, ymax.val, by=binSize), include.lowest=TRUE, dig.lab=5)
    tab.origin <- table(fX.origin, fY.origin)
    colnames(tab.origin) <- seq(ymin.val, ymax.val - binSize, by=binSize)
    rownames(tab.origin) <- seq(xmin.val, xmax.val - binSize, by=binSize)
  } else {
    tab.origin <- tab
  }
    
  if (density) {
    # number of cells in bin
    my.calc <- aggregate(tdata[, 3], by=list(fXY), length)
  } else {
    # get means / median / freq
    if (grepl("MSI", checkCALC)) {
        my.calc <- aggregate(tdata[, 3], by=list(fXY), mean)
        #else if (checkCALC == "medianFI")  my.calc=aggregate(tdata[, 3], by=list(fXY), median)
    } else if (checkCALC == "SD") {
      my.calc <- aggregate(tdata[, 3], by=list(fXY), sd)
      cols <- this$col.blackwhite
    } else if (checkCALC == "SEM") {
      my.calc <- aggregate(tdata[, 3], by=list(fXY), function(x) {
        SEM <- sd(x) / sqrt(length(x))
        # if normally distributed, 95, 4 % of the cells should lie inside the interval mean + / - SEM
        #interval_min <- mean(x) - SEM
        #interval_max <- mean(x) + SEM 
        SEM
        })
      cols <- this$col.blackwhite
    } else if (checkCALC == "RSEM") {
      my.calc <- aggregate(tdata[, 3], by=list(fXY), function(x) {
        RSEM <- sd(x) / sqrt(length(x))
        RSEM / mean(x) * 100
      })
    } else if (checkCALC == "freq") {
        my.calc <- aggregate(tdata[, 3], by=list(fXY), function(x) {
          y <- round(100 * length(which(x >= cutoffs[3])) / length(x))
          return(y)
      })
    } 
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
    decim <- 1
    #range <- ""
    col.minmax <- "black"
    if (checkCALC == "freq" & !density) {
      max.range <- ymin.val + 3 * binSize + diff(c(ymin.val, ymax.val)) / 2
      
      label.steps <- seq(0, 100, by=10)
      
      # bin color factor
      my.calc.fac <- cut(my.calc$x, breaks=seq(0, 100, by=10), labels=1:10, include.lowest=TRUE)
      levels(my.calc.fac) <- c(0, levels(my.calc.fac), 11, 12)
      
      decim <- 0
    } else if (checkCALC == "RSEM" & !density) {
      #min.legend <- ymin.val
      #max.legend <- max.range <- (ymax.val-ymin.val) / 3
      min.legend <- ymin.val + 3 * binSize
      max.legend <- max.range <- ymin.val + 3 * binSize + diff(c(ymin.val, ymax.val)) / 3
      
      step <- round(diff(range(max.legend, min.legend)) / 6, 1)
      steps <- seq(min.legend, max.legend, by=step)
      label.steps <- seq(25, 55, by=5)
      
      label.steps[7] <- ""
      label.steps[6] <- " >= 50"
      label.steps[2] <- "<=25"
      label.steps[1] <- ""
      
      # bin color factor
      my.calc.fac <- cut(my.calc$x, breaks=seq(0, 50, by=5), labels=1:10, include.lowest=TRUE)
      levels(my.calc.fac) <- c(0, levels(my.calc.fac), 11, 12)
      this$my.calc.fac2 <- my.calc.fac
      for (i in 1:length(my.calc.fac)){
        if (!is.na(my.calc$x[i])) {
          if (my.calc$x[i] >= 50 & my.calc$ncells[i] >= mincells) {
            my.calc.fac[i] <- 11
          }
        }
      }
      cols <- this$col.blackred
      decim <- 0
    } else {
      # get steps for legend and plot
      if (density) {
        idx <- which(my.calc$ncells >= mincells)
        #idx <- idx[grep("NA", my.calc[idx, "Group.1"])]
        min.range <- floor(min(my.calc[idx, "x"]) * 10) / 10
        max.range <- max(tab)
        #max.range <- max(my.lengths[, 2])
        decim <- 0
        #printf("maxrange=%s", max.range)
        
        if (max.range < 200) col.minmax <- "red"
      } else if (checkDYNRANGE == "1") {
        idx <- which(my.calc$ncells >= mincells)
        #idx <- idx[grep("NA", my.calc[idx, "Group.1"])]
        min.range <- floor(min(my.calc[idx, "x"]) * 10) / 10
        max.range <- ceiling(max(my.calc[idx, "x"]) * 10) / 10
        
        # if dynamic range is too small
        if (grepl("MSI", checkCALC) & diff(c(min.range, max.range)) <= 0.5) col.minmax <- "red"
      } else {
        min.range <- min.MSI
        max.range <- max.MSI
        #range <- sprintf("Manual MSI range: %0.1f-%0.1f", min.range, max.range)
      }
      
      # get steps
      step <- round(diff(range(max.range, min.range)) / 10, 2) 
      steps <- seq(min.range, max.range, by=step)
      label.steps <- range.steps <- steps[1:11]
      range.step <- diff(c(range.steps[1], range.steps[2]))
      if (density | checkTRANS == "biex") {
        if (max.range > 500) label.steps <- round(label.steps,  - 2)
        else label.steps <- round(label.steps)
        if (density) label.steps[1] <- min.range
      } else if (checkDYNRANGE != "1") {
        if (label.steps[1] != "0") {
          label.steps[1] <- sprintf("<=%s", label.steps[1])
        }
        if (label.steps[11] != "100") {
          label.steps[11] <- sprintf(">=%s", label.steps[11])
        }
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
    }
    my.calc <- cbind(my.calc, fac=as.numeric(my.calc.fac) + 1)

    
    ### get rect steps
    rect.step <- round(diff(c(par()$usr[3], par()$usr[4])) / 37, 2)
    min.legend.y <- par()$usr[3] + 8 * rect.step
    max.legend.y <- par()$usr[3] + 18 * rect.step
    rect.steps <- seq(min.legend.y, max.legend.y, by = rect.step)
      
    ### get absolute bin range of all bins with mincells
    absRange <- round(diff(range(my.calc$x[which(my.calc$ncells >= mincells)])), 3)
    
    #this$my.calc.fac <- my.calc.fac
    this$my.calc <- my.calc
    
    this$bincount <- 0
    this$maxcells <- 0
    
    ##### plot bins
    for (x in rownames(tab)) {
      for (y in colnames(tab)) {
        brackets.open <- c("(", "[")
        brackets.idx.x <- brackets.idx.y <- 1
        if (x == 0) brackets.idx.x <- 2
        if (y == 0) brackets.idx.y <- 2
        
        # get label
        fact <- as.factor(paste(brackets.open[brackets.idx.x], x, ",", as.numeric(x) + binSize, "] ", brackets.open[brackets.idx.y], y, ",", as.numeric(y) + binSize, "]", sep=""))
        idx <- which(as.character(fact) == as.character(my.calc$Group.1))

        if (tab[x, y] >= mincells) {  
          # if enough cells
          rect(x, y, as.numeric(x) + binSize, as.numeric(y) + binSize, col=cols[my.calc[idx, "fac"]], border=NA)
          
          this$bincount <- this$bincount + 1                
          if (tab[x, y] > this$maxcells) {
            this$maxcells <- tab[x, y]
          }
        
        } else if (checkCALC == "MSI(+)" & tab.origin[x, y] >= mincells) {
          # else if MSI(+) and enough cells
          rect(x, y, as.numeric(x) + binSize, as.numeric(y) + binSize, col="gray", border=NA)
          
        } else if (tab[x, y] > 0 & checkSHOWMINBIN == "1"){
          # else if option "Bins<minCount" checked
          rect(x, y, as.numeric(x) + binSize, as.numeric(y) + binSize, col=cols.pale[my.calc[idx, "fac"]], border=NA)
        }
      }
    }
    
    ### add production line
    this$addProdline(cutoffs)
    
    ###### legend plot + label
    #if (density | checkCALC == "SEM" | checkCALC == "SD") {
    if (checkCALC != "RSEM" | density) {
      #min.legend <- ymin.val
      #max.legend <- (ymax.val-ymin.val) / 2 
      min.legend <- ymin.val + 3 * binSize
      max.legend <- ymin.val + 3 * binSize + diff(c(ymin.val, ymax.val)) / 2 
      
      step <- round(diff(range(max.legend, min.legend)) / 11, 1)
      steps <- seq(min.legend, max.legend + step, by=step)
      set.cex <- 1.2
    }
    
    ##### if its not background and not a png minimalistic picture, then print legend and label
    if ((!bg | density) & !png & this$plot.percentage) {
      if (cutoffs[1] > 0 & cutoffs[2] > 0 & cutoffs[3] > 0 & !density) {
        #if (this$working) print("w: add quadrant and product percentages on triplot")
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
        
        csv.content <- c(current.date, displayfile, this$current.cofactor, checkCALC,
        colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange, 
        xmin.val, xmax.val, ymin.val, ymax.val,
        cutoffs[1], cutoffs[2], cutoffs[3],
        round(this$q1.total, 1), round(this$q2.total, 1), round(this$q3.total, 1), round(this$q4.total, 1),
        round(this$q1.prodcells, 1), round(this$q2.prodcells, 1), round(this$q3.prodcells, 1), round(this$q4.prodcells, 1),
        round(this$q1.prodcellsplus, 1), round(this$q2.prodcellsplus, 1), round(this$q3.prodcellsplus, 1), round(this$q4.prodcellsplus, 1))
        
      } else if (cutoffs[1] > 0 & cutoffs[2]) {
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
        text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
        
        csv.content <- c(current.date, displayfile, this$current.cofactor, checkCALC,
        colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange,
        xmin.val, xmax.val, ymin.val, ymax.val,
        cutoffs[1], cutoffs[2], "NA", round(this$q1.total, 1), round(this$q2.total, 1), round(this$q3.total, 1), round(this$q4.total, 1))
        
      } else {
        ### no cutoffs are set, at least the absRange
        csv.content <- c(current.date, displayfile, this$current.cofactor, checkCALC,
        colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange,
        xmin.val, xmax.val, ymin.val, ymax.val,
        )
        
      }
      ### write in csv
      write.table(csv.content, metadatafile, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)
      printf("Quadrant percs written in %s", metadatafile)
      
      
      checkLEGEND <- tclvalue(this$cbtshowLegend)
      this$step <- step
      this$label.steps <- label.steps
      
      ### not for history png
      #if (!png) {
      if (checkLEGEND == "1") {
        ##### legend title
        if (density) legend.title <- "# cells "
        if (!bg) {
          text(x = par()$usr[2] + 0.02 * (par()$usr[2] - par()$usr[1]), 
            y  = rect.steps[10] + 3.0 * rect.step,
            label = legend.title, cex = 0.85 * set.cex, pos = 2)
        }
        label.pos.x <- par()$usr[2] - 0.12 * (par()$usr[2] - par()$usr[1])
        
        for (i in 1:11) {
          ## print legend rectangles
          if (i < 11) {
            rect(xleft = par()$usr[2] - 0.13 * (par()$usr[2] - par()$usr[1]), 
                 ybottom = rect.steps[i], 
                 xright = par()$usr[2] - 0.105 * (par()$usr[2] - par()$usr[1]), 
                 ytop = rect.steps[i] + rect.step, 
                 col=cols[i + 1], border=NA, pos=2)
          }
          
          display.label <- label.steps[i]
          
          if (checkCALC == "RSEM" & !density) {
            text(x=label.pos.x, y=rect.steps[i], label=display.label, col=col.minmax, cex=0.75 * set.cex, pos=4)
          } else if (i == 1 | i == 11) {
            text(x=label.pos.x, y=rect.steps[i], label=display.label, col=col.minmax, cex=0.75 * set.cex, pos=4)
          } else if (i == 6 & rect.step >= 0.3) {
            text(x=label.pos.x, y=rect.steps[i], label=display.label, col=col.minmax, cex=0.75 * set.cex, pos=4)
          } else if (rect.step >= 0.2) { 
            if (i == 3) {
              if (grepl(checkCALC, "MSI")) {
                display.label <- sprintf("%.1f", (as.numeric(label.steps[i]) + (range.step / 2)))
              }
              text(label.pos.x, rect.steps[i] + 0.5 * rect.step, label=display.label, col=col.minmax, cex=0.75 * set.cex, pos=4)
            }
            if (i == 9) {
              if (grepl(checkCALC, "MSI")) {
                display.label <- sprintf("%.1f", (as.numeric(label.steps[i]) - (range.step / 2)))
              }
              text(label.pos.x, rect.steps[i] - 0.5 * rect.step, label=display.label, col=col.minmax, cex=0.75 * set.cex, pos=4)
            }
          } 
        }
      }
    }
  }
  
  # if there are no bins to display in MSI(+) mode
  if (grey.label) {
    text(par()$usr[2], ymin.val + 10 * binSize, label=legend.title, cex=0.85 * set.cex, pos=2)
    
    ### quadrant left lower
    text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
    text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.08 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q1.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
    
    ### quadrant right lower
    text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.03 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[3] + 0.08 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q2.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    
    ### quadrant right upper
    text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.total), col=quadrants.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    text(par()$usr[2] + 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q3.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=2, xpd=TRUE)
    
    ### quadrant left upper
    text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.04 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.total), col=quadrants.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
    text(par()$usr[1] - 0.01 * (par()$usr[2] - par()$usr[1]), par()$usr[4] - 0.09 * (par()$usr[4] - par()$usr[3]), label=sprintf("%0.1f%%", this$q4.prodcells), col=prodcells.color, cex=1.00 * set.cex, pos=4, xpd=TRUE)
    
    csv.content <- c(current.date, displayfile, this$current.cofactor, checkCALC,
    colnames(data)[1], colnames(data)[2], colnames(data)[3], absRange, 
      xmin.val, xmax.val, ymin.val, ymax.val,
      cutoffs[1], cutoffs[2], cutoffs[3],
      round(this$q1.total, 1), round(this$q2.total, 1), round(this$q3.total, 1), round(this$q4.total, 1),
      round(this$q1.prodcells, 1), round(this$q2.prodcells, 1), round(this$q3.prodcells, 1), round(this$q4.prodcells, 1))
    
    write.table(csv.content, metadatafile, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)
    #printf("Quadrant percs written in %s", metadatafile)
    
    if (!bg & !png & !overview) {
      tkmessageBox(title = "Insufficient cell count.", message = sprintf("No bins to display (ncells=%s). Lower cutoff(z).", ncells), icon = "info", type = "ok")
    }
  }
  
  # in mode triploT: if autorect was selected
  if (tclvalue(this$cbtautoRect) == "1") {
    this$addRectInfo(setcex=set.cex)
    this$addCellInfo(setcex=set.cex)
  } 
}