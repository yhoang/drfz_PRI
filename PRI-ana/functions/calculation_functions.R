#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020


fcs$preprocData <- function(mode="removDub") {
  this <- fcs
  
  table <- this$selected.project
  file <- tclvalue(tkget(this$tkchoosefile))
  file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  
  ### if this$data doesn't exists or current file index is not selected file index
  if (!exists("data", env=fcs) | is.null(this$data) | this$current.project  !=  table | this$current.filenum != file.idx |
       this$current.cofactor  !=  as.numeric(tclvalue(this$rbasinh))) {
    this$getFile(table, file.idx)
  }
  
  if (this$working) printf("w: do preprocData: project=%s fileindex=%s", table, file.idx)
  data <- data.frame(this$data, check.names=TRUE)
  
  if (mode == "removDub") {
    ### if "Gate data" is checked, break
    checkGATED <- tclvalue(this$cbtgateData)
    if (checkGATED == "1") {
      tkmessageBox(title = "An error has occured!",
                   message = "Doublets removal only on ungated data. Please uncheck \"Gate data\".", icon = "error", type = "ok")
      stop("Please uncheck \"Gate data\".")
    }
    
    ### if FSC-A and FSC-H are available
    ## doublets can be calculated
    if (length(data$FSC.A) > 0 & length(data$FSC.H) > 0) {
      fsc.median <- median(data$FSC.A / data$FSC.H)
      fsc.stdev2 <- 2 * sd(data$FSC.A / data$FSC.H) 
      
      flag <- mapply(this$flagDoublets, data$FSC.A, data$FSC.H, fsc.median, fsc.stdev2)
      
      data <- data[-(which(flag == TRUE)), ]
      
      if (this$working) {
        printf("Doublets removed: -%s cells", length(which(flag == TRUE)))
      } else {
        tkmessageBox(title <- "Doublets removed.",
                     message = sprintf("Doublets removed: -%s cells.", length(which(flag == TRUE))), icon = "info", type = "ok")
      }
      
      this$ncell.sel <- nrow(data)
      this$ncell.perc <- round(this$ncell.sel / this$origin.ncells * 100, 2)
      tkconfigure(this$ncell.sel.gui, text=as.character(this$ncell.sel))
      tkconfigure(this$ncell.perc.gui, text=as.character(this$ncell.perc))
    } else {
      print("No FSC-A and FSC-H to estimate doublets.")
    }
  } else if (mode == "trim") {
    if (this$data.trimmed){
      print("Data already trimmed.")
    } else{
      printf("Original data dimension: %s ", paste(dim(data), collapse=" x "))
      cells <- 0
      for (t in 1:ncol(data)) {
        data <- data[order(data[, t]), ]
        trim <- floor(nrow(data) * (1 - this$trim.num))
        cells <- cells + 2 * (nrow(data) - trim) - 1
        data <- data[(nrow(data) - trim):trim, ]
        
        # trim.idx <- which(data[,t] > quantile(data[,t],c(1-this$trim.num)))
        #   data <- data[-trim.idx,]
      }
      printf("Data trimmed: -%s cells", cells)
      this$data.trimmed <- TRUE
    }
    
  }
  
  this$data <- data
  colnames(this$data) <- this$selected.vars
}

fcs$flagDoublets <- function(fsc.a, fsc.h, median, sd2) {
  flag <- ifelse((fsc.a / fsc.h) > (median + sd2), TRUE, FALSE)
  
  flag
}

fcs$calcPartition <- function(data) {
  this <- fcs
  
  data <- as.matrix(data)
  
  min <- floor(10 * min(data)) / 10
  max <- ceiling(10 * max(data)) / 10
  range <- max - min
  partition.size <- round(as.numeric(tkget(this$partition.size)) / 100 * range, 1)
  
  cnames <- colnames(data)
  fX <- cut(data[, 1], breaks=seq(min, max, by=partition.size), include.lowest=TRUE)
  fY <- cut(data[, 2], breaks=seq(min, max, by=partition.size), include.lowest=TRUE)
  tab <- table(fX, fY)
  colnames(tab) <- seq(min, max - partition.size, by=partition.size)
  rownames(tab) <- seq(min, max - partition.size, by=partition.size)
  
  # get means
  fXY <- as.factor(paste(fX, fY))
  my.calc <- aggregate(data[, 3], by=list(fXY), mean)
  my.lengths <- aggregate(data[, 3], by=list(fXY), length)
  my.calc <- cbind(my.calc, ncells=my.lengths$x)
  
  # binplot
  for (x in colnames(tab)) {
    for (y in rownames(tab)) {
      #if (tab[x,y]>mincells) {
      fact <- as.factor(paste("(", x, ",", as.numeric(x) + partition.size, "] ", "(", y, ",", as.numeric(y) + partition.size, "]", sep=""))
      idx <- which(as.character(fact) == as.character(my.calc$Group.1))
      rect(x, y, as.numeric(x) + partition.size, as.numeric(y) + partition.size, col=cols[my.calc[idx, "fac"]], border=NA)
      
      this$bincount <- this$bincount + 1
      #}
    }
  }
}

fcs$clearHistory <- function(){
  this <- fcs
  
  imgTemplate <- tclVar()
  tkimage.create("photo", imgTemplate, file=this$template.file)
  
  i <- 0
  for (frame in strsplit(tclvalue(tkwinfo("children", this$frm)), " ")[[1]]) {
    for (item in strsplit(tclvalue(tkwinfo("children", frame)), " ")[[1]]) {
      if (tclvalue(tcl(item, "cget", "-image")) != "") {
        tcl(item, "configure", "-image", imgTemplate)
      }
    }
  }
  this$plot.num <- 0
  this$images <- list()
  this$plot.attr <- list()
}
