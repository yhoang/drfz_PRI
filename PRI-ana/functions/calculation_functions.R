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

### automatic cutoff calulation and interactive gating function--------------------------------------------------
fcs$autoSetCutoff <- function() {
	this=fcs

	if(this$working) printf("Calculating cutoffs start.")
	
	table = this$selected.project
	file=tclvalue(tkget(this$tkchoosefile))
	file.idx=this$current.filetable[which(this$current.filetable[,2]==file),1]
	this$selected.filenum = file.idx

	if ( this$current.project != table | this$current.filenum != file.idx | 
	     length(this$data)==0  | this$current.cofactor != as.numeric(tclvalue(this$rbasinh))) {
	  this$getFile(table,file.idx)
	}
	
	checkTRIMMING = tclvalue(this$cbttrimming)
	checkDOUBLETS = tclvalue(this$cbtremovedub)

	vars= this$selected.vars
	len = length(this$selected.vars)

	# column and cutoff number vector
	colvec=vector()
	for (i in 1:len) {
		if ( tclvalue(this$cbVal[[i]]) == "1") colvec=c(colvec,i)
	}
	len.colvec = length(colvec)

	### if trimming or remove doublets desired
	if ( checkDOUBLETS =="1" ) this$preprocData()
  	if ( checkTRIMMING =="1" ) this$preprocData(mode="trim")

	### select columns to plot
	data = this$data[colvec]

	### open plot window
	dev.label="histogram.autogate"
	if ( length(which(dev.label==names(devList()))) != 0 ) {
		devSet(devList()[which(dev.label==names(devList()))])
	} else {
		if ( len.colvec>36 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=7*2.8,height=ceiling(len.colvec/7)*2.0,label=dev.label)
			par(mfrow=c(ceiling(len.colvec/7),7),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else if ( len.colvec>25 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=6*2.5,height=ceiling(len.colvec/6)*2.0,label=dev.label)
			par(mfrow=c(ceiling(len.colvec/6),6),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else if ( len.colvec>16 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=5*3,height=ceiling(len.colvec/5)*2.3,label=dev.label)
			par(mfrow=c(ceiling(len.colvec/5),5),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else if ( len.colvec>9 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=4*4,height=ceiling(len.colvec/4)*2.5,label=dev.label)
			par(mfrow=c(ceiling(len.colvec/4),4),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else {
			devNew(type="x11",title="Histograms for cutoffs",width=3*4,height=ceiling(len.colvec/3)*3,label=dev.label)
			par(mfrow=c(ceiling(len.colvec/3),3),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		}
		this$plot.windows = c(this$plot.windows,dev.label)
	}
	
	this$cutoff_list = list()
	xmaxval = vector()
	xminval = vector()
	for ( i in 1:len.colvec ) {

		tdata=data[,i]

		d=density(tdata)
		x=d$x
		y=d$y

		### look only at higher levels of y-axis
		# index where y starts at position greater than 1/20th of max(d$y)
		start.y = 1/10*max(y)
		id_y = which(y>start.y)
		### skip compensation problems/unbinded positions
		# index where x is positive
		start.x = 1/10*max(x)
		id_x = which(x>start.x)
		# move 5 positions earlier
		xminval[i] = x[intersect(id_y,id_x)[1]-5]
		xmaxval[i] = round(max(tdata))

		
		stepsize= 6:3
		for ( step in 1:length(stepsize) ) {
			incline_list = list()
			incline_x = vector()
			idx=1
			skip=FALSE
			cutoff=0
			for ( range in 1:(length(y)-stepsize[step]) ) {
				tmp = min(y[range:(range+stepsize[step])])
				diff = diff(c(y[range],y[range+stepsize[step]]))

				# if min(y) in range is more than 1/20th of peak and if x>2.5
				if (tmp > start.y & x[range]>2.5) {
					### incline_list
					# [1] step id
					# [2] x-value from step
					# [3] incline difference in range (x[range],x[range+step])
					### incline_x:  x-value from step
					incline_list[[idx]]=range
					incline_list[[idx]][2]=round(x[range],3)
					incline_list[[idx]][3]=diff
					incline_x[idx]=x[range]
					idx = idx + 1
				}
			}

			if (idx==1) {
				for ( range in 1:(length(y)-stepsize[step]) ) {
					tmp = min(y[range:(range+stepsize[step])])
					diff = diff(c(y[range],y[range+stepsize[step]]))

					# if min(y) in range is more than 1/20th of peak and if x>2.5
					if (tmp > start.y & x[range]>start.x) {
						### incline_list
						# [1] step id
						# [2] x-value from step
						# [3] incline in range (x[range],x[range+step])
						### incline_x:  x-value from step
						incline_list[[idx]]=round(range,2)
						incline_list[[idx]][2]=round(x[range],3)
						incline_list[[idx]][3]=diff
						incline_x[idx]=x[range]
						idx = idx + 1
					}
				}
			}

			if (idx==1) skip=TRUE


			if (!skip) {
				idx.minima = idx.maxima = idx.shoulder = vector()
				for ( inc in 1: (length(incline_x)-1) ) {
					#printf("inc=%s list[3]=%s",inc,incline_list[[inc]][3])
				
					### check if there is at least one minima
				  	# difference at position inc should be -, and at position inc+1 should be +
					if (incline_list[[inc]][3] < 0 & incline_list[[inc+1]][3] > 0) {
						#printf("-turn inc[%s]=%0.5f inc[%s]=%0.5f x=%s",inc,incline_list[[inc]][3],inc+1,incline_list[[inc+1]][3],incline_list[[inc]][2])
						idx.minima=c(idx.minima,inc)
					}
					
					### check if there is at least one maxima
				  # difference at position inc should be +, and at position inc+1 should be -
					if (incline_list[[inc]][3] > 0 & incline_list[[inc+1]][3] < 0) {
						#printf("+turn inc[%s]=%0.5f inc[%s]=%0.5f x=%s",inc,incline_list[[inc]][3],inc+1,incline_list[[inc+1]][3],incline_list[[inc]][2])
						idx.maxima=c(idx.maxima,inc)
					}
					
					### check if there is a shoulder
					if ( abs(incline_list[[inc]][3]) < 0.01) {
						#printf("shoulder inc[%s]=%0.5f x=%s",inc,incline_list[[inc]][3],incline_list[[inc]][2])
						idx.shoulder=c(idx.shoulder,inc)
					}
					
				}
				this$inc=incline_x
				this$inc_list=incline_list
				
				idx=vector()
				### substract all maxima indices
				# remove maxima indices near minima
				minus.minima=intersect(idx.maxima,idx.minima)
				if (length(minus.minima) > 0) {
					for ( j in (minus.minima-3):(minus.minima+3)) {
						idx = c(idx,which(idx.minima==j))
					}
					idx.minima = idx.minima[-idx]
				}
				# remove shoulder indices near minima x+-0.3
				minus.shoulder=intersect(idx.maxima,idx.shoulder)
				if (length(minus.shoulder) > 0) {
					
					x.max=incline_x[minus.shoulder]
					for (k in idx.shoulder) {
						#print(incline_x[k])
						if (abs(x.max-incline_x[k])<=0.3) idx=c(idx,which(idx.shoulder==k))
						# if (abs(x.max-incline_x[k])<=0.3) printf("k=%s x[k]=%s x.max=%s abs=%s",k,incline_x[k],x.max,abs(x.max-incline_x[k]))
						#idx = c(idx, which(incline_x[k]x.max-incline_x[k])<=0.25))
					}
					#if (this$working) printf("w: idx.shoulder=%s",paste(idx.shoulder,collapse=" "))
					#if (this$working) printf("W. x.max=%s -idx=%s",x.max,paste(idx,collapse=" "))
					idx.shoulder = idx.shoulder[-idx]
				}

				#printf("length minima=%s",length(idx.minima))
				col.idx = colvec[i]
				if ( length(idx.minima)>0 ) {
				  # if there is a minima
					tmp=round(length(idx.minima)/2)
					if (tmp==0) tmp=idx.minima[1]
					else tmp=idx.minima[round(length(idx.minima)/2)]
					tclvalue(this$vcutoffs[[col.idx]]) = incline_list[[tmp]][2]
					cutoff = as.numeric(tclvalue(this$vcutoffs[[col.idx]]))
					if (this$working) printf("w: tmp=%s cutoff[%s]=%s (min)",tmp,vars[colvec[i]],cutoff)
				} else if ( length(idx.shoulder)>0 ) {
				  # if there is a shoulder
					tmp=round(length(idx.shoulder)/2)
					if (tmp==0) tmp=idx.shoulder[1]
					else tmp = idx.shoulder[round(length(idx.shoulder)/2)]
					if (this$working) printf("w: tmp=%s idx.shoulder=%s",tmp,paste(idx.shoulder,collapse=" "))
					tclvalue(this$vcutoffs[[col.idx]]) = incline_list[[tmp]][2]
					cutoff = as.numeric(tclvalue(this$vcutoffs[[col.idx]]))
					if (this$working) printf("w: cutoff[%s]=%s (shoulder)",vars[colvec[i]],cutoff)
				} else {
				  # if there is nothing found, than do 20% quantile
					tclvalue(this$vcutoffs[[col.idx]]) = this$calcCUTOFF(tdata,20,vars[i],col.idx)
					cutoff = as.numeric(tclvalue(this$vcutoffs[[col.idx]]))
					if (this$working) printf("w: cutoff[%s]=%s (max20)",vars[colvec[i]],cutoff)
				}

			}

			if (step == 1) {
				this$cutoff_list[[i]] = cutoff
			} else {
				this$cutoff_list[[i]][step] = cutoff
			}
		}
	}

	this$xminval = xminval
	this$xmaxval = xmaxval
	this$selected.cutoffs = vector() 
	for ( i in 1:len.colvec ) {
		tdata=data[,i]
		d=density(tdata)

		cutoff = round(median(this$cutoff_list[[i]]),1)
		col.idx = colvec[i]

		tclvalue(this$vcutoffs[[col.idx]]) = cutoff
		this$selected.cutoffs[colvec[i]] = cutoff
  
		### check for inifinity
		if (is.infinite(xminval[i]) | is.na(xminval[i])) xminval[i] = 0
		if (is.infinite(xmaxval[i]) | is.na(xminval[i])) xmaxval[i] = 8
		
		
		### plot histogram
		#plot(d,main=paste(colnames(data)[i]),xlim=c(xminval[i],xmaxval[i]),xlab="")
		#polygon(d, col="palegreen2", border="palegreen2")
		hist(tdata,freq=F,breaks=50,main=paste(colnames(data)[i]),xlab="",
			col = "palegreen2", lwd=0.5)
		lines(density(tdata), lwd = 2, col = "brown")

		# vertical lines
		for ( x in ceiling(xminval[i]) : floor(xmaxval[i]) ) {
			abline(v=x,col="darkgrey")
		}
		if ( cutoff != 0 ) {
			abline(v=cutoff,lty=2,lwd=2)
			tperc = round( length(tdata[which(tdata>=cutoff)]) / length(tdata)*100,1)
			# display percentage of production
			text(cutoff, max(d$y)-0.05*max(d$y),label=sprintf("%0.1f%%",tperc),cex=1,pos=4,xpd=TRUE)
			# display asinh-value of cutoff
			text(cutoff, min(d$y)+0.05*max(d$y),label=sprintf("[%s]",cutoff),cex=1,pos=4,xpd=TRUE)
		}
	}
	this$new.cutoffs[[this$selected.filenum]] = this$selected.cutoffs
	if (this$working) printf("w: current cutoffs:%s",paste(this$selected.cutoffs,collapse=" "))
}

fcs$interactGate <- function() {
	this=fcs
  printf("w: do interactGate")
	
	table = this$selected.project
	file=tclvalue(tkget(this$tkchoosefile))
	file.idx=this$current.filetable[which(this$current.filetable[,2]==file),1]
	this$selected.filenum = file.idx
	
	if ( is.null(this$data) | this$current.project != table | this$current.filenum != file.idx |
	     this$current.cofactor != as.numeric(tclvalue(this$rbasinh)) ) {
	  this$getFile(table,file.idx)
	}
	
	checkTRIMMING = tclvalue(this$cbttrimming)
	checkDOUBLETS = tclvalue(this$cbtremovedub)

	vars= this$selected.vars
	len = length(this$selected.vars)

	# column and cutoff number vector
	cutoffs=vector()
	colvec=vector()
	for (i in 1:len) {
		if ( tclvalue(this$cbVal[[i]]) == "1") {
			cutoffs=c(cutoffs,this$selected.cutoffs[i])
			colvec=c(colvec,i);
		}
	}
	len.colvec = length(colvec)

	### if trimming or remove doublets desired
	if ( checkDOUBLETS =="1" ) this$preprocData()
  	if ( checkTRIMMING =="1" ) this$preprocData(mode="trim")

	### select columns to plot
	data = this$data[colvec]
	colnames(data)=colnames(this$data[colvec])
	print(colnames(data))

	### open plot window
	dev.label="histogram.interactivegate"
	if ( length(which(dev.label==names(devList()))) != 0 ) {
		devSet(devList()[which(dev.label==names(devList()))])
	} else {
		devNew(type="x11",title="Histograms for cutoff setting",width=6,height=5,label=dev.label)
		par(oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		this$plot.windows = c(this$plot.windows,dev.label)
	}

	for ( i in 1:len.colvec ) {
		tdata=data[,i]
		d=density(tdata)
		x=d$x
		y=d$y

		### index where y is greater than 1/35th of max(d$y)
		id_y = which((y>(1/35*max(y))))
		### index where x is positive
		id_x = which(x>0)

		xminval = x[intersect(id_y,id_x)[1]-5]
		xmaxval = round(max(tdata))

		### check for inifinity
		if (is.infinite(xminval) | is.na(xminval)) xminval = 0
		if (is.infinite(xmaxval) | is.na(xminval)) xmaxval = 8
		
		
		### plot histogram
		#plot(d,main=paste(colnames(data)[i]),xlim=c(xminval,xmaxval),xlab="")
		#polygon(d, col="palegreen2", border="palegreen2")
		hist(tdata,freq=F,breaks=50,main=paste(colnames(data)[i]),xlab="",
			col = "palegreen2", lwd=0.5)
		lines(density(tdata), lwd = 2, col = "brown")
		# vertical grid lines
		for ( x in ceiling(xminval) : floor(xmaxval) ) {
			abline(v=x,col="darkgrey")
		}
		# plot cutoffs if already entered
		if (cutoffs[i] != 0) {
			abline(v=cutoffs[i],lty=2,lwd=2)
			tperc = round( length(tdata[which(tdata>=cutoffs[i])]) / length(tdata)*100,1)
			# display percentage of production
			text(cutoffs[i], max(d$y)-0.05*max(d$y),label=sprintf("%0.1f%%",tperc),cex=1,pos=4,xpd=TRUE)
			# display asinh-value of cutoff
			text(cutoffs[i], min(d$y)+0.05*max(d$y),label=sprintf("[%s]",cutoffs[i]),cex=1,pos=4,xpd=TRUE)
		}

		loc=lapply(locator(1),round,1)
		abline(v=loc$x[1],col="darkgrey")
		printf("cutoff(%s) =%s", colnames(data)[i], loc$x[1])

		tclvalue(this$vcutoffs[[colvec[i]]]) = loc$x[1]
		this$selected.cutoffs[colvec[i]] = loc$x[1]
	}

	devOff(devList()[which(dev.label==names(devList()))])
	

	this$new.cutoffs[[this$selected.filenum]] = this$selected.cutoffs
	if (this$working) printf("w: current cutoffs:%s",paste(this$selected.cutoffs,collapse=" "))
}

fcs$calcCUTOFF <- function (data,cutoff,var,i) {
	this = fcs

	data = as.matrix(data)

	### calculate cutoff        
	data.sort=sort(data,decreasing=T)
	cutoff.new=round(data.sort[length(data)*cutoff/100],3)

	if (this$working) printf("w: do calcCUTOFF: cutoff.old=%s cutoff.new=%s var=%s idx=%s",cutoff,cutoff.new,var,i)

	### set cutoff in di- and triploT
	setcutoff =  eval(parse(text=paste0("this$vcutoffs[[",i,"]]")))
	tclvalue(setcutoff) = as.character(round(cutoff.new,3))
	setcbtperc = eval(parse(text=paste0("this$cbcutoffperc[[",i,"]]")))
	tclvalue(setcbtperc) = "0"

	### also set cutoff in overview tabs
	idx = which(this$selected.vars == var)
	setoverviewcutoff = eval(parse(text=paste("this$vcutoffs[[",idx,"]]",sep="")))
	tclvalue(setoverviewcutoff) = as.character(round(cutoff.new,3))

	return(cutoff.new)
}