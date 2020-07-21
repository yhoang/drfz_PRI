#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020


### plot residual functions (densities, histograms, legend) ---------------------------
fcs$plotDensities <- function(plotter, pdf) {
  this <- fcs

  this$refreshPlotters()

  if (!pdf) tkconfigure(this$tt, cursor = "watch")

  printf("w: do plotDensities: plotter=%s, pdf=%s", plotter, pdf)
  len <- length(this$selected.vars)
  dev.label <- paste("density", plotter, sep=".")

  if (plotter  == "tri") {
    # mode plotter == triploT
    dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
    
    if (length(which(dev.label == names(devList()))) == 0) {            
      ncol <- as.numeric(tclvalue(this$vncol))
      nrow <- as.numeric(tclvalue(this$vnrow))
      this$plotter.tri.num <- this$plotter.tri.num  +  1
      dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
      devNew(type="x11", title="Densities for triploTs", width=ncol * 4, height=nrow * 4, label=dev.label)
      # mar in points, mai in inches
      # oma adds title lines
      # order: bottom, left, top, and right
      par(mfrow=c(nrow, ncol), oma=c(1, 1, 3, 1), mar=c(3, 3, 5, 2))
      this$plot.windows <- c(this$plot.windows, dev.label)
    } else {
      devSet(devList()[which(dev.label == names(devList()))])
    }
    
    file <- tclvalue(tkget(this$tkchoosefile))
    displayfile <- this$shortenFilename(file)
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
    var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
    tkset(this$cbvar2, var2)
    vars <- c(var1, var2)
    
    ### if features are not in sample
    if (length(var1) == 0){
      tkmessageBox(title = "An error has occured!", 
                  message = "Check your features.")
      stop(sprintf("Feature A=%s is not existent in that file.", tclvalue(tkget(this$cbvar1))))
    } else {
      tkset(this$cbvar1, var1)
    }
    if (length(var2) == 0){
      tkmessageBox(title = "An error has occured!", 
                  message = "Check your features.")
      stop(sprintf("Feature B=%s is not existent in that file.", tclvalue(tkget(this$cbvar2))))
    } else {
      tkset(this$cbvar2, var2)
    }
    
    
    var1.idx <- which(this$selected.vars == var1)
    var2.idx <- which(this$selected.vars == var2)
    
    cutoff1 <- this$checkDigits(cutoff_id=var1.idx)
    cutoff2 <- this$checkDigits(cutoff_id=var2.idx)
    cutoffs <- c(cutoff1, cutoff2)
    colvec <- c(var1.idx, var2.idx)
    len.colvec <- length(colvec)
    
    cbperc1 <- tclvalue(this$cbcutoffperc[[var1.idx]])
    cbperc2 <- tclvalue(this$cbcutoffperc[[var2.idx]])
    cbperc <- c(cbperc1, cbperc2)
  } else if (plotter == "triover") {
    # mode triploT overview
    file <- tclvalue(tkget(this$tkchoosefile))
    
    if (this$OverviewGate) {
      displayfile <- this$shortenFilename(this$plot.attr[[1]]$file.name)
    } else {
      displayfile <- this$shortenFilename(file)
    }
    
    # column and cutoff number vector
    vars <- vector()
    colvec <- vector()
    cbperc <- vector()
    cutoffs <- vector()
    for (i in 1:len) {
      if (tclvalue(this$cbVal[[i]]) == "1") {
        colvec <- c(colvec, i)
        cutoffs <- c(cutoffs, this$checkDigits(cutoff_id=i))
        cbperc <- c(cbperc, tclvalue(this$cbcutoffperc[[i]]))
        vars <- c(vars, this$selected.vars[i])
      }
    }
    len.colvec <- length(colvec)
    
    if (len.colvec < 1) {
      tkmessageBox(title = "An error has occured!", 
                  message = "Please select at least one markers.", icon = "error", type = "ok")
      stop("Missing values")
    }
    
    if (pdf) {
      print(paste0("Creating page 2/", len.colvec + 2, ".."))
      
      if (len.colvec > 8) par(mfrow=c(len.colvec - 1, len.colvec - 2), oma=c(2, 1, 3, 1), mar=c(3.5, 4, 5, 2.5))
      else par(mfrow=c(8, 6), oma=c(2, 1, 3, 1), mar=c(3.5, 4, 5, 2.5))
    } else if (length(which(dev.label == names(devList()))) == 0) {
      devNew(type="x11", title="Densities for triploTs overview", width=3 * 4, height=3 * 3.8, label=dev.label)
      par(mfrow=c(3, 3), oma=c(1, 1, 3, 1), mar=c(3, 3, 5, 1))
      #devSet(grep(dev.label, names(devList())) + 1)
      this$plot.windows <- c(this$plot.windows, dev.label)
    } else {
      #devSet(grep(dev.label, names(devList())) + 1)
      devSet(devList()[which(dev.label == names(devList()))])
    }
  }

  if (pdf & this$OverviewGate) {
    table <- this$temptable.name[this$temp.num]
    file.idx <- this$selected.filenum
  } else if (grepl("^temp\\d\\d", file)) {
    table <- file
    file.idx <- 1
  } else {
    table <- this$current.project
    file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  }

  if (pdf & this$OverviewGate) table <- this$temptable.name[this$temp.num]

  timeSTART <- Sys.time()
  if (is.null(this$data) | this$current.project != table | this$current.filenum != file.idx |
      this$current.cofactor != as.numeric(tclvalue(this$rbasinh))) {
    if (this$working) print("Time loading data:")
    this$getFile(table, file.idx)
    if (this$working) print(Sys.time() - timeSTART)
  }

  checkTRANS <- tclvalue(this$rbtrans)
  checkGATED <- tclvalue(this$cbtgateData)
  checkGRID <- tclvalue(this$cbtshowGrid)
  checkTRIMMING <- tclvalue(this$cbttrimming)
  checkCALC <- "density"

  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))

  data <- this$data[colvec]

  if (checkTRANS == "asinh") {
    scale <- this$asinh$scale
    label <- this$asinh$label
    grid.step <- this$asinh$step
  } else {
    scale <- this$biex$scale
    label <- this$biex$label
    grid.step <- this$biex$step
  } 

  ### check cutoffs
  for (i in 1:len.colvec) {
    if (cbperc[i] == "1") {
      tdata <- data[, i]
      cutoffs[i] <- this$calcCUTOFF(tdata, cutoffs[i], vars[i], colvec[i])
    }
  }

  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  #cutoffs[3]=0
  t <- 0

  for (v1 in 1:len.colvec) {
    for (v2 in (v1 + 1):len.colvec) {
      
      ## skip loop if one of the axes are the same
      if (v1 == v2 | v2 > len.colvec) next
      
      
      #### big start
      set.cex <- 1.1
      set.cex.axes <- 1.0
      set.mgp <- c(1.9, 0.7, 0)
      if (cutoffs[1] > 0) title.axis <- sprintf("%s (%s)", v1, cutoffs[1])
      else title.axis <- v1
      if (cutoffs[2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", v2, cutoffs[2]))
      else title.axis <- c(title.axis, v2)
      
      plot(1, type="n", frame.plot=FALSE, xlim=c(xminval, xmaxval + 10 * binSize), axes=FALSE, 
          ylim=c(yminval - 2.5 * binSize, ymaxval + 5 * binSize), xlab=title.axis[1], ylab=title.axis[2], cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
      box(lwd=0.8, col="darkgrey")
      
      ### draw axis on the bottom and on the left
      axis(side=1, at=scale, labels=labels, las=1, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
      axis(side=2, at=scale, labels=labels, las=3, cex.axis=set.cex.axes, mgp=set.mgp, col="darkgrey")
      
      ### add grid
      if (checkGRID == "1") {
        xgrid.steps <- seq(0, (xmaxval), by=grid.step)
        ygrid.steps <- seq(0, (ymaxval), by=grid.step)
        abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
      }
      
      #select columns to plot
      tdata <- as.matrix(data[, c(v1, v2, v2)])
      
      ### calc quadrants in total
      ncells <- nrow(tdata)
      # q1 Quadrant unten links
      # q2 Quadrant unten rechts
      # q3 Quadrant oben rechts
      # q4 Quadrant oben links
      if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
        tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[v1] &  tdata[, 2] < cutoffs[v2]), 3]
        tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[v1] &  tdata[, 2] < cutoffs[v2]), 3]
        tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[v1] &  tdata[, 2] >= cutoffs[v2]), 3]
        tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[v1] &  tdata[, 2] >= cutoffs[v2]), 3]
        
        this$q1.total <- abs(100 * length(tdata.q1) / ncells)
        this$q2.total <- abs(100 * length(tdata.q2) / ncells)
        this$q3.total <- abs(100 * length(tdata.q3) / ncells)
        this$q4.total <- abs(100 - this$q1.total - this$q2.total - this$q3.total)
      }
      
      # get only the cells which are greater than 0
      tdata.zero <- tdata[tdata[, 1] >= 0 & tdata[, 2] >= 0, ]
      ncells.zero <- nrow(tdata.zero)
      
      ### calc quadrants with only positive values
      # q1 Quadrant unten links
      # q2 Quadrant unten rechts
      # q3 Quadrant oben rechts
      # q4 Quadrant oben links
      if (cutoffs[v1] > 0 & cutoffs[v2] > 0) {
        q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[v1] &  tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)      
        q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] &  tdata.zero[, 2] < cutoffs[v2])) / ncells.zero)                    
        q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[v1] &  tdata.zero[, 2] >= cutoffs[v2])) / ncells.zero)                    
        q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
      }
      
      if (checkGATED != "1") {
        this$origin.ncells <- ncells
        this$coords <- list()
      }
      
      this$bintriplot(tdata, c(cutoffs[c(v1, v2)], 0), binSize=binSize, mincells=mincount, density=TRUE)
      
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
      #if (checkGATED == "1") firstLine=sprintf("%s(%0.1f%%): density", displayfile, this$ncell.perc)
      if (this$OverviewGate) firstLine <- sprintf("%s * (%0.1f%%): density/cof=%s", displayfile, this$ncell.perc, this$current.cofactor)
      else firstLine <- sprintf("%s: density/cof=%s", displayfile, this$current.cofactor)
      title(main=firstLine, line=3.2, cex.main=0.9, adj=0)
      
      secondLine <- sprintf("cells(min/max)=%s/%s", mincount, this$maxcells)
      title(main=secondLine, line=2.4, cex.main=0.9, adj=0)
      
      thirdLine <- sprintf("%s-%s(%0.1f%%); binSize=%s, #bins=%s", 
                        ncells, ncells.zero, (ncells.zero / ncells * 100), binSize, this$bincount)
      
      
      fifthLine <- ""
      if (checkGATED == "1" | grepl("^temp\\d + ", file)) {
        if (length(this$coords.info) > 2) {
          fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
          fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
        } else {
          fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
        }
        title(main=fourthLine, line=0.9, cex.main=0.6, adj=0)
        title(main=fifthLine, line=0.2, cex.main=0.6, adj=0)
      }
      
      tkconfigure(this$tt, cursor = "left_ptr")
      
      t <- t  +  1
    }
    #}
    if (plotter == "triover" & (t == 1)) break
  }
  #devSet(devList()[which(dev.label == names(devList()))])

  if (pdf) {
    #add title for page in 3D-Overview tab
    #title=as.character(tclvalue(tkget(this$title, "1.0", "end-1c")))
    #mtext(title, outer = TRUE, cex = 1.5, line=1.3, pos=2)
    #if (length(this$coords.info > 0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse="; ")), outer=TRUE, cex = 0.8)
    #dev.off()
    print("w: Done plotting densities in PDF.")
  }

  ############# start plot history 
  if (plotter == "tri") {
    ### only in plotter = "tri"
    
    this$plot.num <- this$plot.num  +  1
    ### push plot attributes one down
    for (i in this$plot.num:1) {
      this$plot.attr[i + 1] <- this$plot.attr[i]
    }
    
    tdata <- as.matrix(data[, c(1, 2, 2)])
    ### pngs for history
    png(file=this$png.file, width=140, height=145, bg="transparent")
    # mar in points, mai in inches
    # oma adds title lines
    # order: bottom, left, top, and right
    par(mar=c(1.1, 1.1, 1.3, 0.3))
    if (cutoffs[1] > 0 & cutoffs[2] > 0) title.axis <- c(sprintf("%s (%s)", vars[1], cutoffs[1]), sprintf("%s (%s)", vars[2], cutoffs[2]))
    else title.axis <- c(vars[1], vars[2])
    plot(1, type="n", frame.plot=FALSE, xlim=c(xminval, xmaxval + 10 * binSize), axes=FALSE, 
        ylim=c(yminval - 2.5 * binSize, ymaxval + 5 * binSize), xlab=title.axis[1], ylab=title.axis[2], cex.lab=set.cex * 0.9, cex.axis=0.5 * set.cex.axes, mgp=c(0.1, 0, 0))
    box(lwd=0.8, col="darkgrey")
    
    ### add grid
    if (checkGRID == "1") {
      xgrid.steps <- seq(0, (xmaxval), by=grid.step)
      abline(h=ygrid.steps, v=xgrid.steps, col="grey", lty=3)
    }
    
    this$bintriplot(tdata, c(cutoffs[c(v1, v2)], 0), set.cex=0.8, set.cex.axes=0.2, set.mgp=c(0.2, 0, 0), binSize=binSize, png=TRUE, density=TRUE)
    title(main=sprintf("%s: %s", displayfile, checkCALC), line=0.5, cex.main=0.8, adj=0)
    dev.off()
    ###
    
    ### plot history
    this$images[[this$plot.num]] <- tclVar()
    tkimage.create("photo", this$images[[this$plot.num]], file=this$png.file)
    ### save FI ranges for this transformation type
    this$changeFI.range(mode=3)
    
  }
  ############# stop plot history

  tkconfigure(this$tt, cursor = "left_ptr")
}

fcs$plotHistograms <- function(plotter, pdf) {
  this <- fcs
  printf("w: do plotHistograms: plotter=%s, pdf=%s", plotter, pdf)
  
  if (plotter == "preproc") {
    file <- tclvalue(tkget(this$current.filePrep))
  } else {
    file <- tclvalue(tkget(this$tkchoosefile))
  }
  len <- length(this$selected.vars)
  
  dev.label <- paste("histogram", plotter, sep=".")
  
  ### get column vector
  colvec <- vector()
  cutoffs <- vector()
  cbperc <- vector()
  vars <- vector()
  for (i in 1:len) {
    if (tclvalue(this$cbVal[[i]]) == "1") {
      colvec <- c(colvec, i)
      cutoffs <- c(cutoffs, this$checkDigits(cutoff_id=i))
      cbperc <- c(cbperc, tclvalue(this$cbcutoffperc[[i]]))
      vars <- c(vars, this$selected.vars[i])
    }
  }
  len.colvec <- length(colvec)
  
  if (plotter != "preproc") {
    this$refreshPlotters()
    this$refreshCutoffs(current=TRUE)
    
    if (plotter == "di") {
      var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1di)))
      var1.idx <- which(this$selected.vars == var1)
      tkset(this$cbvar1di, var1)
      var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2di)))
      var2.idx <- which(this$selected.vars == var2)
      tkset(this$cbvar2di, var2)
      
      ### if features are not in sample
      if (length(var1) == 0 | length(var2) == 0){
        tkmessageBox(title = "An error has occured!", 
                     message = "Check your features.")
        stop("One of the features are not existent.")
      }
      
      ### remove double features
      double.idx <- which(colvec == var1.idx | colvec == var2.idx)
      colvec <- colvec[-double.idx]
      cutoffs <- cutoffs[-double.idx]
      
      colvec <- c(var1.idx, var2.idx, colvec)
      cutoffs <- c(
        this$checkDigits(cutoff_id=var1.idx), 
        this$checkDigits(cutoff_id=var2.idx), 
        cutoffs
     )
    } else if (plotter == "tri") {
      var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
      var1.idx <- which(this$selected.vars == var1)
      tkset(this$cbvar1, var1)
      var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
      var2.idx <- which(this$selected.vars == var2)
      tkset(this$cbvar2, var2)
      var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))
      var3.idx <- which(this$selected.vars == var3)
      tkset(this$cbvar3, var3)
      
      ### if features are not in sample
      if (length(var1) == 0 | length(var2) == 0 | length(var3) == 0){
        tkmessageBox(title = "An error has occured!", 
                     message = "Check your features.")
        stop("One of the features are not existent.")
      }
      
      ### remove double features
      double.idx <- which(colvec == var1.idx | colvec == var2.idx | colvec == var3.idx)
      colvec <- colvec[-double.idx]
      cutoffs <- cutoffs[-double.idx]
      
      colvec <- c(
        var1.idx, 
        var2.idx, 
        var3.idx, 
        colvec
     )
      cutoffs <- c(
        this$checkDigits(cutoff_id=var1.idx), 
        this$checkDigits(cutoff_id=var2.idx), 
        this$checkDigits(cutoff_id=var3.idx), 
        cutoffs
     )
    } else if (plotter == "quadru") {
      var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1quad)))
      var1.idx <- which(this$selected.vars == var1)
      tkset(this$cbvar1quad, var1)
      var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2quad)))
      var2.idx <- which(this$selected.vars == var2)
      tkset(this$cbvar2quad, var2)
      var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3quad)))
      var3.idx <- which(this$selected.vars == var3)
      tkset(this$cbvar3quad, var3)
      var4 <- this$checkMarker(tclvalue(tkget(this$cbvar4quad)))
      var4.idx <- which(this$selected.vars == var4)
      tkset(this$cbvar4quad, var4)
      
      ### if features are not in sample
      if (length(var1) == 0 | length(var2) == 0 | length(var3) == 0 | length(var4) == 0){
        tkmessageBox(title = "An error has occured!", 
                     message = "Check your features. One of the features are not existent.")
        stop("One of the features are not existent.")
      }
      
      ### remove double features
      double.idx <- which(colvec == var1.idx | colvec == var2.idx | colvec == var3.idx | colvec == var4.idx)
      colvec <- colvec[-double.idx]
      cutoffs <- cutoffs[-double.idx]
      
      colvec <- c(
        var1.idx, 
        var2.idx, 
        var3.idx, 
        var4.idx, 
        colvec
     )
      cutoffs <- c(
        this$checkDigits(cutoff_id=var1.idx), 
        this$checkDigits(cutoff_id=var2.idx), 
        this$checkDigits(cutoff_id=var3.idx), 
        this$checkDigits(cutoff_id=var4.idx),
        cutoffs
     )
    }
    
    len.colvec <- length(colvec)
  }
  
  
  if (length(which(dev.label == names(devList()))) != 0) {
    devSet(devList()[which(dev.label == names(devList()))])
  } else if (!pdf) {
    if (len.colvec > 36) {
      devNew(type="x11", title="Histograms", width=7 * 2.8, height=ceiling(len.colvec / 7) * 2.0, label=dev.label)
      par(mfrow=c(ceiling(len.colvec / 7), 7), oma=c(1.5, 1, 1.5, 1), mar=c(2, 2.4, 1.5, 1), mgp=c(1.3, 0.4, 0))
    } else if (len.colvec > 25) {
      devNew(type="x11", title="Histograms", width=6 * 2.5, height=ceiling(len.colvec / 6) * 2.0, label=dev.label)
      par(mfrow=c(ceiling(len.colvec / 6), 6), oma=c(1.5, 1, 1.5, 1), mar=c(2, 2.4, 1.5, 1), mgp=c(1.3, 0.4, 0))
    } else if (len.colvec > 16) {
      devNew(type="x11", title="Histograms", width=5 * 3, height=ceiling(len.colvec / 5) * 2.3, label=dev.label)
      par(mfrow=c(ceiling(len.colvec / 5), 5), oma=c(1.5, 1, 1.5, 1), mar=c(2, 2.4, 1.5, 1), mgp=c(1.3, 0.4, 0))
    } else if (len.colvec > 9) {
      devNew(type="x11", title="Histograms", width=4 * 4, height=ceiling(len.colvec / 4) * 2.5, label=dev.label)
      par(mfrow=c(ceiling(len.colvec / 4), 4), oma=c(1.5, 1, 1.5, 1), mar=c(2, 2.4, 1.5, 1), mgp=c(1.3, 0.4, 0))
    } else {
      devNew(type="x11", title="Histograms", width=3 * 4, height=ceiling(len.colvec / 3) * 3, label=dev.label)
      par(mfrow=c(ceiling(len.colvec / 3), 3), oma=c(1.5, 1, 1.5, 1), mar=c(2, 2.4, 1.5, 1), mgp=c(1.3, 0.4, 0))
    }
    this$plot.windows <- c(this$plot.windows, dev.label)
  }
  
  set.cex <- 1.1
  if (pdf) {
    if (FALSE) {
      if (len.colvec / 5 > 2) {
        #print("5")
        par(mfrow=c(7, ceiling(len.colvec / 7)), oma=c(3, 3, 4, 3), mar=c(5, 5, 7, 5))
        set.cex <- 2.5
      } else if (len.colvec / 4 > 2) {
        #print("4")
        par(mfrow=c(6, ceiling(len.colvec / 6)), oma=c(3, 3, 4, 3), mar=c(5, 5, 7, 5))
        set.cex <- 2.5
      } else if (len.colvec / 3 > 2) {
        #print("3")
        par(mfrow=c(5, ceiling(len.colvec / 5)), oma=c(3, 3, 4, 3), mar=c(5, 5, 7, 5))
        set.cex <- 2
      } else {
        par(mfrow=c(4, ceiling(len.colvec / 4)), oma=c(3, 3, 4, 3), mar=c(3, 3, 5, 3))
        set.cex <- 1.5
      }
    }
    print("hallo")
  } else if (length(which(dev.label == names(devList()))) == 0) {
    devNew(type="x11", title=sprintf("Histograms for %sploTs", plotter), width=3 * 4, height=ceiling(len.colvec / 3) * 3.1, label=dev.label)
    if (len.colvec / 5 > 2) par(mfrow=c(ceiling(len.colvec / 5), 5), oma=c(2.5, 2, 2.5, 2), mar=c(3, 2, 3, 2))
    else if (len.colvec / 4 > 2) par(mfrow=c(ceiling(len.colvec / 4), 4), oma=c(2.5, 2, 2.5, 2), mar=c(3, 2, 3, 2))
    else  par(mfrow=c(ceiling(len.colvec  /  3), 3), oma=c(2.5, 2, 2.5, 2), mar=c(3, 2, 3, 2))
    this$plot.windows <- c(this$plot.windows, dev.label)
  } else {
    devSet(devList()[which(dev.label == names(devList()))])
  }
  
  if (pdf & this$OverviewGate) {
    table <- this$temptable.name[this$temp.num]
    file.idx <- this$selected.filenum
  } else if (grepl("^temp\\d\\d", file)) {
    table <- file
    file.idx <- 1
  } else {
    table <- this$selected.project
    file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  }
  
  timeSTART <- Sys.time()
  
  if (is.null(this$data) | this$current.project != table | this$current.filenum != file.idx |
       this$current.cofactor != as.numeric(tclvalue(this$rbasinh))) {
    if (this$working) print("Time loading data:")
    this$getFile(table, file.idx)
    if (this$working) print(Sys.time() - timeSTART)
  }
  
  data <- this$data
  for (i in colvec) {
    xminval <- as.numeric(tkget(this$minvalX))
    xmaxval <- as.numeric(tkget(this$maxvalX))
    
    tdata <- data[, i]
    d <- density(tdata)
    
    if (min(d$x) > xminval) xminval <- min(d$x)
    if (max(d$x) < xmaxval) xmaxval <- max(d$x)
    
    ### check for inifinity
    if (is.infinite(xminval) | is.na(xminval)) xminval <- 0
    if (is.infinite(xmaxval) | is.na(xminval)) xmaxval <- 8
    
    if (max(d$y) > 10) {
      # if density too narrow
      # plot frequency histogram instead of density
      hist(tdata, freq=FALSE, breaks=50, main=paste(colnames(data)[i]))
    } else {
      hist(tdata, freq=FALSE, breaks=50, main=paste(colnames(data)[i]), xlab="", col = "#66bd63", lwd=0.5)
      lines(density(tdata), lwd = 2, col = "#b2182b")
    }
    # vertical lines
    for (x in ceiling(xminval) : floor(xmaxval)) {
      abline(v=x, col="darkgrey")
    }
    
    ### cutoff line and percentage
    if (this$selected.cutoffs[i] != 0) {
      abline(v=this$selected.cutoffs[i], lty=2, lwd=2)
      tperc <- round(length(tdata[which(tdata >= this$selected.cutoffs[i])]) / length(tdata) * 100, 1)
      ### display percentage of production
      text(this$selected.cutoffs[i], max(d$y) - 0.05 * max(d$y), label=sprintf("%0.1f%%", tperc), cex=set.cex, pos=4, xpd=TRUE)
      ### display asinh-value of cutoff
      text(this$selected.cutoffs[i], min(d$y) + 0.05 * max(d$y), label=sprintf("[%s]", this$selected.cutoffs[i]), cex=set.cex, pos=4, xpd=TRUE)
    }
  }
  
  if (pdf) {
    #add title for page in 3D-Overview tab
    title <- as.character(tclvalue(tkget(this$title, "1.0", "end-1c")))
    mtext(title, outer = TRUE, cex = 1.5, line=1.0, pos=2)
    if (length(this$coords.info > 0)) mtext(sprintf("(%s)", paste(this$coords.info, collapse=";")), outer=TRUE, cex = set.cex, xpd=TRUE)
    print("w: Done plotting histograms in PDF.")
  } else {
    devSet(devList()[which(dev.label == names(devList()))])
  }
}

fcs$plotLegend <- function () {
  this <- fcs
  
  dev.label <- "legend"
  dev.cur <- dev.cur()
  if (length(which(dev.label == names(devList()))) != 0) {
    devOff(devList()[which(dev.label == names(devList()))])
  } 
  
  devNew(type="x11", title="Color legend", width=4, height=0.7, label=dev.label)
  par(oma=c(1.3, 0.5, 1.0, 0.5), mar=c(0.5, 0, 0, 0))
  plot(1, type="n", frame.plot=FALSE, xlim=c(1, 11), axes=FALSE, 
       ylim=c(0, 1), xlab="Legend colors", ylab="", cex.lab=1.0, cex.axis=1.0)
  axis(side=1, at=c(1.35, 6, 10.55), 
       labels=c("low", "medium", "high"), las=1, cex.axis=1.0, tick=FALSE, mgp=c(1.0, 0.3, 0))
  for (i in 1:10) {
    rect(i, 0, i + 1, 1, col = this$col.rainbow[i + 1], border = FALSE)
    rect(i, 1.2, i + 1, 2.2, col = this$col.green[i + 1], border = FALSE)
  }
  
  devSet(dev.cur) 
}

fcs$refreshPlotters <- function() {
  ### function to delete plotter names if window is closed
  this <- fcs
  
  if (FALSE) {
    printf("w: current dev names: %s", paste(names(devList()), collapse=" "))
    printf("w: old plotter names: %s", paste(this$plot.windows, collapse=" "))
  }
  if (length(this$plot.windows) > 0) {
    dev.names <- names(devList)
    dev.del.idx <- vector()
    for (i in 1:length(this$plot.windows)) {
      if (length(which(this$plot.windows[i] == names(devList()))) == 0) {
        dev.del.idx <- c(dev.del.idx, i)
      }
    }
    if (length(dev.del.idx > 0)) this$plot.windows <- this$plot.windows[-dev.del.idx]
  }
}

