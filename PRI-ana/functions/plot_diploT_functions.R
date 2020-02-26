#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020


### diploT functions -----------------------------------------------------

### Create diploT overview in PDF
fcs$dodiploTtotal <- function() {
  this=fcs
  printf("w: do dodiploTtotal")
  
  checkTRANS = tclvalue(this$rbtrans)
  checkGRID = tclvalue(this$cbtshowGrid)
  
  file=tclvalue(tkget(this$tkchoosefile))
  binSize=as.numeric(tkget(this$binSizedi))
  mincells=as.numeric(tkget(this$minCountDi))
  
  xminval=as.numeric(tkget(this$minvalX))
  xmaxval=as.numeric(tkget(this$maxvalX))
  
  len.vars = length(this$selected.vars)
  colvec = vector()
  for (i in 1:len.vars) {
    if ( tclvalue(this$cbVal[[i]]) == "1") colvec=c(colvec,i)
  }
  len.colvec = length(colvec)
  
  if ( len.colvec < 2 ) {
    tkconfigure(this$tt, cursor = "left_ptr")
    tkmessageBox(title = "An error has occured!",
                 message = "Please select at least two marker.", icon = "error", type = "ok")
    stop("Please select at least two marker.")
  }
  
  set.cex.axes = 0.75
  set.mgp = c(1.1, 0.3, 0)
  
  
  # if path didnt save yet
  if (!exists("lastpath",where=eval(parse(text="fcs")))){
    this$lastpath = getwd()
  }
  filename = this$shortenFilename(file)
  file = tkgetSaveFile(defaultextension=".pdf", initialdir= this$lastpath, initialfile=filename)
  
  ### remove file name to get file path
  filepath = unlist(strsplit(tclvalue(file),"/"))
  filepath = filepath[-length(filepath)]
  filepath = paste(filepath,collapse="/")
  
  if (this$working) printf("w: path=%s filename=%s file=%s",filepath,filename,file)
  
  ### create a PDF
  toPDF(filename, path=filepath, {
    if (len.colvec < 5) {
      par(mfrow=c(round(len.colvec/2),2),oma=c(0.5,0.5,2,0),mar=c(3,5,1,2))
      plot.num.per.page = round(len.colvec/2)*2
    } else if (len.colvec < 12) {
      par(mfrow=c(4,2),oma=c(0.5,0.5,2,0),mar=c(3,5,1,2))
      plot.num.per.page = 4*2
    } else if (len.colvec < 20) {
      par(mfrow=c(3,2),oma=c(0.5,0.5,2,0),mar=c(3,5,1,2))
      plot.num.per.page = 3*2
    } else {
      par(mfrow=c(2,2),oma=c(0.5,0.5,2,0),mar=c(3,5,1,2))
      plot.num.per.page = 2*2
    }
    
    cex.size = 12/len.colvec
    if (len.colvec <12) cex.size = 1
    
    it = 0
    for ( i in colvec ) {
      plot(1,type='n',frame.plot=FALSE,xlim=c(xminval-0.5,xmaxval+2),axes=FALSE,
           ylim=c(0,len.colvec),xlab=sprintf("%s (%s,binSize=%s/mincells=%s,cutoff=%s)",this$selected.vars[i],checkTRANS,binSize,mincells,tclvalue(this$vcutoffs[[i]])),
           ylab="",cex.lab=cex.size*0.7,cex.axis=cex.size,mgp=set.mgp)
      
      ### no box for now
      # box(lwd=0.8,col="darkgrey")
      
      if ( checkTRANS == "asinh") {
        scale = this$asinh$scale
        labels = this$asinh$label
        grid.step = this$asinh$step
      } else {
        scale = this$biex$scale
        labels = this$biex$label
        grid.step = this$biex$step
      } 
      
      ### draw axis on the bottom
      axis(side=1, at=scale,labels=labels,las=1,cex.axis=set.cex.axes,mgp=set.mgp,col="darkgrey")
      
      ### grid
      if(checkGRID=="1") {
        xgrid.steps=seq(0,(xmaxval),by=grid.step)
        abline(v=xgrid.steps,col="grey",lty=3)
      }
      
      this$distepy = 0
      this$axis.label = rep("",len.colvec+4)
      this$dodiploT(mode="overview",totalID=i,total.cex=0.7)
      
      it = it + 1
      if (it == plot.num.per.page) {
        this$addTitle()
        it = 0
      }
    }
    
    this$addTitle()	
    
  })
  
  if (this$working) print("Done.")
  else tkmessageBox(title = "PDF saved in.. ",
                    message = paste(file), icon = "info", type = "ok")
  
  this$lastpath = filepath
  
  printf("w: Done dodiploTtotal")
}


fcs$dodiploT <- function(mode,totalID=NA,total.cex=NA) {
  this=fcs
  printf("w: dodiploT: mode=%s totalID=%s",mode,totalID)
  
  checkTRANS = tclvalue(this$rbtrans)
  if (checkTRANS =="") checkTRANS = tclvalue(this$rbtrans) = "asinh"
  checkCALC = tclvalue(this$rbcalc)
  if (checkCALC == "") {
    checkCALC = tclvalue(this$rbcalc) = "MSI"
    this$rbcalc = tclVar("MSI")
  }
  checkGRID = tclvalue(this$cbtshowGrid)
  checkDYNRANGE = tclvalue(this$cbtdynRange)
  checkTRIMMING = tclvalue(this$cbttrimming)
  if ( checkTRIMMING == "1" ) {
    this$preprocData(mode="trim")
  }
  
  if (checkDYNRANGE != "1") {
    min.MSI=this$checkDigits(num=tkget(this$minMSI))
    max.MSI=this$checkDigits(num=tkget(this$maxMSI))
    if (max.MSI <= min.MSI) {
      tmp = min.MSI 
      min.MSI = max.MSI
      max.MSI = tmp
    }
  }
  
  if ( checkTRANS == "asinh" ) {
    scale = this$asinh$scale
    label = this$asinh$label
    grid.step = this$asinh$step
  } else {
    scale = this$biex$scale
    label = this$biex$label
    grid.step = this$biex$step
  } 
  
  file=tclvalue(tkget(this$tkchoosefile))
  binSize=as.numeric(tkget(this$binSizedi))
  
  xminval=as.numeric(tkget(this$minvalX))
  xmaxval=as.numeric(tkget(this$maxvalX))
  
  len.vars = length(this$selected.vars)
  if ( is.na(totalID) ) {
    var1=this$checkMarker(tclvalue(tkget(this$cbvar1di)))
    tkset(this$cbvar1di,var1)
    var1.num = which(this$selected.vars==var1)
    
    ### if features are not in sample
    if ( length(var1)==0 ){
      tkmessageBox(title = "An error has occured!",
                   message = "Check your features again.")
      stop("One of the features are not existent.")
    }
    # set plot window
    new.plot=TRUE
    this$refreshPlotters()
    dev.label=paste("plotter","di",this$plotter.di.num,sep=".")
    if ( length(which(dev.label==names(devList()))) != 0 ) {
      devSet(devList()[which(dev.label==names(devList()))])
      new.plot=FALSE
    }
  } else {
    var1=this$selected.vars[totalID]
    var1.num = totalID
    new.plot=FALSE
  }
  cutoff1=this$checkDigits(cutoff_id=var1.num)
  
  tkconfigure(this$tt, cursor = "watch")
  
  # column and cutoff number vector
  colvec=vector()
  cutoffs=vector()
  cbperc=vector()
  vars=vector()
  if (mode == "single") {
    var2=this$checkMarker(tclvalue(tkget(this$cbvar2di)))
    tkset(this$cbvar2di,var2)
    
    ### if features are not in sample
    if ( length(var2)==0 ){
      tkmessageBox(title = "An error has occured!",
                   message = "Check your features again.")
      stop("One of the features are not existent.")
    }
    
    var2.num = which(this$selected.vars==var2)
    cutoff2=this$checkDigits(cutoff_id=var2.num)
    
    colvec=c(var2.num,var1.num)
    cutoffs=c(cutoff2,cutoff1)
    
    cbperc1 = tclvalue(this$cbcutoffperc[[var1.num]])
    cbperc2 = tclvalue(this$cbcutoffperc[[var2.num]])
    cbperc = c(cbperc2,cbperc1)
    vars = c(var2,var1)
    len = 2
  } else if (mode =="overview") {
    # if plotting overview
    for (i in 1:len.vars) {
      if ( tclvalue(this$cbVal[[i]]) == "1") {
        colvec=c(colvec,i);
        cbperc=c(cbperc,tclvalue(this$cbcutoffperc[[i]]))
        cutoffs=c(cutoffs,this$checkDigits(cutoff_id=i))
        vars=c(vars,this$selected.vars[i])
      }
    }
    colvec=c(colvec,var1.num)
    cutoffs=c(cutoffs,cutoff1)
    vars=c(vars,var1)
    len=length(colvec)
    
    if ( len == 1 ) {
      tkconfigure(this$tt, cursor = "left_ptr")
      tkmessageBox(title = "An error has occured!",
                   message = "Please select at least one marker.", icon = "error", type = "ok")
      stop("Please select at least one marker.")
    }
  }
  
  if (this$working) printf("w: dodiploT vars=%s",paste(vars,collapse=" "))
  
  ### import data   
  table = this$selected.project
  if ( grepl("temp",file) ) {
    file.idx = 1
    table=file
  } else {
    file.idx=this$current.filetable[which(this$current.filetable[,2]==file),1]
    this$selected.filenum = file.idx
  }
  
  # do not trim and remove doublets if data is gated
  if (this$diploTGate) {
    # if data is gated, just recall
    data=this$getData(table,file.idx,columns=colvec)
  }  else if ( this$current.project==table & this$current.filenum==file.idx & !is.null(this$data) & 
               this$current.cofactor == as.numeric(tclvalue(this$rbasinh))) {
    data = this$data
  } else {
    this$getFile(table,file.idx)
    data=this$data
  }
  
  # new plot window if
  len.default=26
  if (len>len.default) len.default=len
  if (len.default>35) len.default=35
  if ( is.na(totalID) & (
    length(devList())==0
    | new.plot
    | (mode=="overview" & (this$distepy+len.default)>len.default+3) 
    | this$distepy>(len.default+3)
    | var1 != this$current.var1.diploT
    | !all(c(xminval,xmaxval) == this$current.xaxis)) 
  ) {
    this$newdiploT(len=len.default)
    this$current.xaxis=c(xminval,xmaxval)
    this$current.var1.diploT = var1
  }
  
  ### add grid
  if (FALSE) {
    if(checkGRID=="1") {
      xgrid.steps=seq(0,(xmaxval),by=grid.step)
      abline(v=xgrid.steps,col="grey",lty=3)
    }
  }
  
  
  ### check cutoffs
  for ( i in 1:(len-1) ) {
    if ( cbperc[i] == "1" ) cutoffs[i] = this$calcCUTOFF(data[,colvec[i]],cutoffs[i],vars[i],colvec[i])
  }
  
  ### plot bins
  # tmp.plot for a new plot window
  tmp.plot = FALSE
  pos.y = 0
  for ( i in 1:(len-1) ) {
    if (this$distepy > len.default) this$newdiploT(len.default)
    if (pos.y > len.default) {
      this$newdiploT(len.default)
      tmp.plot = TRUE
    }
    
    if (tmp.plot) {
      pos.y = i-len.default-3
      printf("TRUE pos.y = %s distepy=%s len.default=%s ",pos.y,this$distepy,len.default)
      #this$distepy = 0
    } else {
      pos.y = this$distepy+i-1
    }
    
    if (mode=="single") {
      this$bindiplot(data[,c(colvec[i],var1.num)],cutoffs[c(i,len)],bin.pos=this$distepy)
      this$distepy = this$distepy + 1
      
      this$origin.ncells = nrow(data)
      if (checkCALC != "freq") {
        this$ncell.sel = nrow(data[which(data[,1]> cutoffs[1]),])
        this$ncell.perc=round(this$ncell.sel/this$origin.ncells*100,2)
        tkconfigure(this$ncell.sel.gui.di,text=as.character(this$ncell.sel))
        tkconfigure(this$ncell.perc.gui.di,text=as.character(this$ncell.perc))
      }
      tkconfigure(this$ncell.gui.di,text=as.character(this$origin.ncells))
      
    } else {
      #pos.y = this$distepy+i-1
      #printf("pos.y=%s",pos.y)
      if ( !is.na(totalID) ) this$bindiplot(data[,c(colvec[i],var1.num)],cutoffs[c(i,len)],bin.pos=pos.y,totalcex=total.cex)
      else this$bindiplot(data[,c(colvec[i],var1.num)],cutoffs[c(i,len)],bin.pos=pos.y)
    }
  }
  
  if (mode=="overview") this$distepy = this$distepy+i
  
  tkconfigure(this$tt, cursor = "left_ptr")
  
  printf("w: Done dodiploT distepy=%s",this$distepy)
}

fcs$newdiploT <- function(len=26) {
  this=fcs
  
  checkTRANS = tclvalue(this$rbtrans)
  checkGRID = tclvalue(this$cbtshowGrid)
  checkDATE = tclvalue(this$cbtaddDate)
  
  xminval=as.numeric(tkget(this$minvalX))
  xmaxval=as.numeric(tkget(this$maxvalX))
  binSize=as.numeric(tkget(this$binSizedi))
  mincells=as.numeric(tkget(this$minCountDi))
  len.vars=length(this$selected.vars)
  var1=this$checkMarker(tclvalue(tkget(this$cbvar1di)))
  var1.idx=which(this$selected.vars==var1)
  
  ### if features are not in sample
  if ( length(var1)==0 ){
    tkmessageBox(title = "An error has occured!",
                 message = "Check your features.")
    stop("One of the features are not existent.")
  }
  
  
  
  # start plot
  # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
  # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
  set.cex.axes = 0.75
  set.mgp = c(1.3, 0.2, 0)
  
  this$plotter.di.num = this$plotter.di.num + 1
  dev.label=paste("plotter","di",this$plotter.di.num,sep=".")
  this$plot.windows = c(this$plot.windows,dev.label)
  
  devNew(type="x11",title=sprintf("n-diploTs  x-range=[%.1f,%.1f] x-axis=%s",xminval,xmaxval,var1),width=8,height=len*0.28,label=dev.label)
  par(oma=c(0,0.5,2,0),mar=c(2.5,11,1,2))
  plot(1,type='n',frame.plot=FALSE,xlim=c(xminval-0.5,xmaxval+2),axes=FALSE,
       ylim=c(0.5,len),xlab=sprintf("%s (%s,binSize=%s/mincells=%s,cutoff=%s)",var1,checkTRANS,binSize,mincells,tclvalue(this$vcutoffs[[var1.idx]])),ylab="",cex.lab=1.0,cex.axis=0.9,mgp=set.mgp)
  
  ### no box for now
  # box(lwd=0.8,col="darkgrey")
  
  if ( checkTRANS == "asinh") {
    scale = this$asinh$scale
    labels = this$asinh$label
    grid.step = this$asinh$step
  } else {
    scale = this$biex$scale
    labels = this$biex$label
    grid.step = this$biex$step
  } 
  
  ### draw axis on the bottom and on the left
  axis(side=1, at=scale,labels=labels,las=1,cex.axis=set.cex.axes,mgp=set.mgp,col="darkgrey")
  
  ### grid
  if(checkGRID=="1") {
    xgrid.steps=seq(0,(xmaxval),by=grid.step)
    abline(v=xgrid.steps,col="grey",lty=3)
  }
  
  ### add date
  if ( checkDATE=="1" ) {
    date = gsub("-","",Sys.Date())
    title(main=date,outer=T,line=1,cex.main=1.0,adj=1)
  }
  
  this$distepy = 0
  this$axis.label = rep("",len+4)
  
  printf("w: Done newdiploT: distepy=%s var1=%s var1.idx=%s total plots=%s",this$distepy,var1,var1.idx,len)
}

fcs$dodiGraph <- function(mode, total.cex=NA, set.zero=FALSE) {
  this=fcs
  printf("w: dodiGraph: mode=%s set.zero=%s",mode,set.zero)
  
  ### checkbutton options
  checkTRANS = tclvalue(this$rbtrans)
  if (checkTRANS == "") {
    checkTRANS = "asinh"
    tclvalue(this$rbtrans) = "asinh"
  }
  
  if ( checkTRANS == "asinh") {
    scale = this$asinh$scale
    labels = this$asinh$label
    grid.step = this$asinh$step
  } else {
    scale = this$biex$scale
    labels = this$biex$label
    grid.step = this$biex$step
  } 
  
  checkGRID = tclvalue(this$cbtshowGrid)
  checkGATED = tclvalue(this$cbtgateData)
  checkDATE = tclvalue(this$cbtaddDate)
  checkCALC = tclvalue(this$rbcalc)
  if (checkCALC == "") {
    checkCALC = "MSI"
    tclvalue(this$rbcalc) = "MSI"
  }
  checkADDFILE = tclvalue(this$cbtaddFilename)
  if (checkADDFILE == "") {
    checkADDFILE = "1"
    tclvalue(this$cbtaddFilename) = "1"
  }
  checkSHOWMINBIN = tclvalue(this$cbtshowMinBins)
  
  checkDYNRANGE = tclvalue(this$cbtdynRange)
  if (checkDYNRANGE == "") {
    checkDYNRANGE = "1"
    tclvalue(checkDYNRANGE) = "1"
  }
  if (checkDYNRANGE != "1") {
    min.MSI=this$checkDigits(num=tkget(this$minMSI))
    max.MSI=this$checkDigits(num=tkget(this$maxMSI))
    if (max.MSI <= min.MSI) {
      tmp = min.MSI 
      min.MSI = max.MSI
      max.MSI = tmp
    }
  }
  
  file=tclvalue(tkget(this$tkchoosefile))
  xminval=as.numeric(tkget(this$minvalX))
  xmaxval=as.numeric(tkget(this$maxvalX))
  binSize=as.numeric(tkget(this$binSizedi))
  mincells=as.numeric(tkget(this$minCountDi))
  
  tkconfigure(this$tt, cursor = "watch")
  
  
  len.vars=length(this$selected.vars)
  var1=this$checkMarker(tclvalue(tkget(this$cbvar1di)))
  var1.idx=which(this$selected.vars==var1)
  ### if features are not in sample
  if ( length(var1)==0 ){
    tkmessageBox(title = "An error has occured!",
                 message = "Check your Feature A.")
    stop("One of the features are not existent.")
  }
  
  cutoff1=this$checkDigits(cutoff_id=var1.idx)
  
  
  # column and cutoff number vector
  colvec=vector()
  cutoffs=vector()
  cbperc=vector()
  vars=vector()
  if (mode == "single") {
    var2=this$checkMarker(tclvalue(tkget(this$cbvar2di)))
    ### if features are not in sample
    if ( length(var2)==0 ){
      tkmessageBox(title = "An error has occured!",
                   message = "Check your feature Y.")
      stop("One of the features are not existent.")
    }
    
    tkset(this$cbvar2di,var2)
    var2.num = which(this$selected.vars==var2)
    cutoff2=this$checkDigits(cutoff_id=var2.num)
    
    colvec=c(var2.num,var1.idx)
    cutoffs=c(cutoff2,cutoff1)
    
    cbperc1 = tclvalue(this$cbcutoffperc[[var1.idx]])
    cbperc2 = tclvalue(this$cbcutoffperc[[var2.num]])
    cbperc = c(cbperc2,cbperc1)
    vars = c(var2,var1)
    len = 2
  } else if (mode =="overview") {
    # if plotting overview
    for (i in 1:len.vars) {
      if ( tclvalue(this$cbVal[[i]]) == "1") {
        colvec=c(colvec,i);
        cbperc=c(cbperc,tclvalue(this$cbcutoffperc[[i]]))
        cutoffs=c(cutoffs,this$checkDigits(cutoff_id=i))
        vars=c(vars,this$selected.vars[i])
      }
    }
    
    ### import data   
    table = this$selected.project
    if ( grepl("temp",file) ) {
      file.idx = 1
      table=file
    } else {
      file.idx=this$current.filetable[which(this$current.filetable[,2]==file),1]
      this$selected.filenum = file.idx
    }
    
    # do not trim and remove doublets if data is gated
    if (this$diploTGate) {
      # if data is gated, just recall
      data=this$getData(table,file.idx,columns=colvec)
    }  else if ( this$current.project==table & this$current.filenum==file.idx & !is.null(this$data)  
                 & this$current.cofactor == as.numeric(tclvalue(this$rbasinh))) {
      data = this$data
    } else {
      this$getFile(table,file.idx)
      data=this$data
    }
    ncells = nrow(data)
    
    
    ### check cutoffs
    for ( i in 1:length(cbperc) ) {
      if ( cbperc[i] == "1" ) cutoffs[i] = this$calcCUTOFF(data[,colvec[i]],cutoffs[i],vars[i],colvec[i])
    }
    
    if (any(var1.idx==colvec)) {
      idx.col = which(colvec==var1.idx)
      colvec = colvec[-idx.col]
      cutoffs = cutoffs[-idx.col]
      vars = vars[-idx.col]
    }
    
    colvec=c(colvec,var1.idx)
    cutoffs=c(cutoffs,cutoff1)
    vars=c(vars,var1)
    len=length(colvec)
    colors = rainbow(len-1)
    
    if ( len == 1 ) {
      tkconfigure(this$tt, cursor = "left_ptr")
      tkmessageBox(title = "An error has occured!",
                   message = "Please select at least one marker.", icon = "error", type = "ok")
      stop("Please select at least one marker.")
    }
  }
  
  if (this$working) printf("w: vars=%s",paste(vars,collapse=" "))
  
  
  if (checkGATED != "1") {
    this$origin.ncells=ncells
    this$ncell.sel=this$origin.ncells
    this$ncell.perc=round(this$ncell.sel/this$origin.ncells*100,2)
    tkconfigure(this$ncell.gui,text=as.character(this$origin.ncells))
    tkconfigure(this$ncell.sel.gui,text=as.character(this$ncell.sel))
    tkconfigure(this$ncell.perc.gui,text=as.character(this$ncell.perc))
    tkconfigure(this$ncell.gui.di,text=as.character(this$origin.ncells))
    tkconfigure(this$ncell.sel.gui.di,text=as.character(this$ncell.sel))
    tkconfigure(this$ncell.perc.gui.di,text=as.character(this$ncell.perc))
  } else {
    this$ncell.sel=ncells
    this$ncell.perc=round(this$ncell.sel/this$origin.ncells*100,2)
    tkconfigure(this$ncell.sel.gui,text=as.character(this$ncell.sel))
    tkconfigure(this$ncell.perc.gui,text=as.character(this$ncell.perc))
    tkconfigure(this$ncell.sel.gui.di,text=as.character(this$ncell.sel))
    tkconfigure(this$ncell.perc.gui.di,text=as.character(this$ncell.perc))
  }
  
  
  ### create plot table
  my.plot = vector()
  
  
  ### fX divides the range of data[,2] (x-axis) into intervals and codes the values in data[,2] according to which interval they fall. 
  # The leftmost interval corresponds to level one, the next leftmost to level two and so on.
  fX=cut(data[,colvec[len]],breaks=seq(xminval,xmaxval,by=binSize),include.lowest=TRUE,dig.lab=5)
  
  for ( i in 1:(len-1) ) {
    
    ### if cutoff is set, cut data from cutoff 
    #if (cutoffs[i]>0 & checkCALC == "MSI+") {
    #	data = data[which(data[,i] > cutoffs[i]),]
    #}
    
    ### if data is empty, only display 'empty data'
    if ( nrow(data) != 0 ) {
      
      ### set negative values of colnum 1 to zero
      #if ( checkCALC != "density" ) data[which(data[,colvec[i]]<0),1] = 0
      
      if ( checkCALC == "density") {
        ### number of cells in bin
        my.calc=aggregate(data[,colvec[i]],by=list(fX),length)
      } else if ( checkCALC == "SD" ) {
        my.calc=aggregate(data[,colvec[i]],by=list(fX),sd)
        cols=this$col.blackwhite
      } else if ( checkCALC == "SEM" ) {
        my.calc=aggregate(data[,colvec[i]],by=list(fX),function(x) {
          SEM = sd(x)/sqrt(length(x))
          ### if normally distributed, 95,4 % of the cells should lie inside the interval mean +/- SEM
          # interval_min = mean(x) - SEM
          # interval_max = mean(x) + SEM 
          SEM
        })
        cols=this$col.blackwhite
      } else if ( checkCALC == "RSEM" ) {
        my.calc=aggregate(data[,colvec[i]],by=list(fX),function(x) {
          RSEM = sd(x)/sqrt(length(x))
          RSEM/mean(x)*100
        })
        cols=this$col.blackred
      } else if ( checkCALC == "freq" ) {
        my.calc = aggregate(data[,colvec[i]],by=list(fX),function(x) {
          y= round( 100 * length(which(x >= cutoffs[1])) / length(x))
          return(y)
        })
      } else {
        # MSI
        if (set.zero) {
          printf("%s::%s values set to zero.",vars[i],length(which(data[,colvec[i]]<0)))
          data[which(data[,colvec[i]]<0),colvec[i]] = 0
        }
        my.calc=aggregate(data[,colvec[i]],by=list(fX),mean)
      } 
      
    }
    
    if (i==1) my.plot = my.calc
    else my.plot = cbind(my.plot,my.calc[,2])
    colnames(my.plot)[i+1] = vars[i]
  }
  
  my.lengths=aggregate(data[,1],by=list(fX),length)
  my.plot=cbind(my.plot,ncells=my.lengths$x)
  
  this$my.plot = my.plot
  
  
  ### parameters for plotting frame
  idx.ncells = which(my.plot$ncells>=mincells)
  this$idx.ncells = idx.ncells
  
  xmin = strsplit(as.character(my.plot$Group.1[idx.ncells[1]]),",")[[1]][1]
  if ( grepl("^\\(",xmin) ) xmin = as.numeric(strsplit(xmin,"\\(")[[1]][2])
  else xmin = as.numeric(strsplit(xmin,"\\[")[[1]][2])
  xmin = xmin - 2*binSize
  xmax = strsplit(as.character(my.plot$Group.1[idx.ncells[length(idx.ncells)]]),",")[[1]][1]
  xmax = as.numeric(strsplit(xmax,"\\(")[[1]][2])
  xmax = xmax + 2*binSize
  
  
  # start plot
  # mgp: A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. 
  # The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
  set.cex.axes = 0.75
  set.mgp = c(1.3, 0.4, 0)
  
  this$plotter.di.graph = this$plotter.di.graph + 1
  dev.label=paste("plotter","digraph",this$plotter.di.graph,sep=".")
  this$plot.windows = c(this$plot.windows,dev.label)
  
  devNew(type="x11",title=sprintf("n-diGraphs  x-range=[%.1f,%.1f] x-axis=%s",xmin,xmax,var1),width=9,height=8,label=dev.label)
  par(oma=c(0,1,2,0),mar=c(3,3,1,21))
  
  ### get x ticks for line graphs
  x.tick = vector()
  for ( i in 1:dim(my.plot)[1] ) {
    sub1=strsplit(as.character(my.calc$Group.1[i]),",")[[1]][1]
    if (grepl("^\\(",sub1)) sub2=as.numeric(strsplit(sub1,"\\(")[[1]][2])
    else sub2=as.numeric(strsplit(sub1,"\\[")[[1]][2])
    x.tick = c(x.tick,sub2)	
  }
  
  ### plot line graphs
  legend.width = legend.type = legend.col = legend.point = legend.var =  legend.label = legend.range = vector()
  for ( i in 2:len ) {
    if (diff(range(my.plot[idx.ncells,i]))<0.5) {
      width = 0.5
      type = 3
      point = 3 
    } else {
      width = 1.5
      type = 1
      point = 1
    }
    yrange = round(diff(range(my.plot[idx.ncells,i])),2)
    #lines(x.tick[idx.ncells],my.plot[idx.ncells,i], type="o",lty=type,col=colors[i-1],lwd=width,pch=point,cex=0.5)
    
    legend.width = c(legend.width,width)
    legend.type = c(legend.type,type)
    #legend.col = c(legend.col,colors[i-1])
    legend.point = c(legend.point,point)
    legend.range = c(legend.range,yrange)
    legend.var = c(legend.var,names(my.plot)[i])
    legend.label = c(legend.label,sprintf("%s (%s)",names(my.plot)[i],yrange))
  }
  legend.table = data.frame(
    var = legend.var
    ,label = legend.label
    ,range = legend.range
    #,col = legend.col
    ,width = legend.width
    ,type = legend.type
    ,point = legend.point
    ,stringsAsFactors=FALSE
  )
  
  plot.len = 10
  if (dim(legend.table)[1]<plot.len) plot.len = dim(legend.table)[1]
  
  
  legend.table = legend.table[order(legend.table$range,decreasing=TRUE),]
  legend.table = legend.table[1:plot.len,]
  colors = rainbow(plot.len+2)
  legend.table = cbind(legend.table,col=colors[1:(length(colors)-2)])
  
  y_range = range(my.plot[idx.ncells,c(legend.table$var)])
  
  ### plot frame
  plot(1,type='n',frame.plot=FALSE,axes=FALSE
       ,xlim=c(xmin-10*binSize,xmax)
       ,ylim=c(y_range[1],y_range[2])
       ,xlab=sprintf("%s (%s,binSize=%s/mincells=%s,cutoff=%s)",var1,checkTRANS,binSize,mincells,tclvalue(this$vcutoffs[[var1.idx]]))
       ,ylab=checkCALC,cex.lab=1.0,cex.axis=0.7,mgp=set.mgp)
  
  ### no box for now
  # box(lwd=0.8,col="darkgrey")
  
  ### draw axis on the bottom and on the left
  axis(side=1, at=scale,labels=labels,las=1,cex.axis=set.cex.axes,mgp=set.mgp,col="darkgrey")
  axis(side=2, at=scale,labels=labels,las=3,cex.axis=set.cex.axes,mgp=set.mgp,col="darkgrey")
  
  ### add grid
  if (FALSE) {
    if(checkGRID=="1") {
      xgrid.steps=seq(0,(xmaxval),by=grid.step)
      abline(v=xgrid.steps,col="grey",lty=3)
    }
  }
  
  ### add date
  if ( checkDATE=="1" ) {
    date = gsub("-","",Sys.Date())
    title(main=date,outer=T,line=1,cex.main=1.0,adj=1)
  }
  
  this$legend.table = legend.table
  this$x.tick = x.tick
  ### ṕlot line graphs
  for ( i in 1:plot.len ) {
    lines(x = x.tick[idx.ncells]
          ,y=my.plot[idx.ncells,legend.table$var[i]]
          ,lty=legend.table$type[i]
          ,lwd=legend.table$width[i]
          ,pch=legend.table$point[i]
          ,col=as.character(legend.table$col[i])
          ,type="o"
          ,cex=0.5
    )
    
    if ( (i%%2)==0 ) {
      text(x=x.tick[idx.ncells[1]]
           ,y=my.plot[idx.ncells[1],legend.table$var[i]]
           ,labels=legend.table$lab[i]
           ,col=as.character(legend.table$col[i])
           ,pos=2
           ,xpd=TRUE
      )
    } else {
      text(x=x.tick[idx.ncells[length(idx.ncells)]]
           ,y=my.plot[idx.ncells[length(idx.ncells)],legend.table$var[i]]
           ,labels=legend.table$lab[i]
           ,col=as.character(legend.table$col[i])
           ,pos=4
           ,xpd=TRUE
      )
    }
    
  }
  
  ### create legend
  legend(
    x = par()$usr[2] + 3*0.1*(par()$usr[2]-par()$usr[1]),
    y = par()$usr[4],
    legend.table$lab,
    col=as.character(legend.table$col),
    lty=legend.table$type,
    lwd=legend.table$width,
    pch=legend.table$point,
    xpd=TRUE
  )
  
  
  tkconfigure(this$tt, cursor = "left_ptr")
  
  printf("w: Done dodiGraph")
}

fcs$bindiplot <- function(
  data,              # table with two columns, col01=Feat. B, col02=Feat. A!
  cutoffs,           # vector with two values, cutoffs[1] for Feat. B, cutoffs[2] for Feat. A
  bin.pos,           # y-position where to plot the diplot
  set.cex=1.1,
  set.cex.axes=1.0,
  totalcex=1.0
){ 
  this=fcs
  
  #if (this$working) printf("w: do bindiplot: plot #%s, cutoffs=%s set.cex=%s",y,paste(cutoffs,collapse=" "),set.cex)
  empty=FALSE
  
  data.lengths=nrow(data)
  data.old = data
  
  ### get variables
  xmin.val=as.numeric(tkget(this$minvalX))
  xmax.val=as.numeric(tkget(this$maxvalX))
  min.MSI=this$checkDigits(num=tkget(this$minMSI))
  max.MSI=this$checkDigits(num=tkget(this$maxMSI))
  binSize=as.numeric(tkget(this$binSizedi))
  mincells=as.numeric(tkget(this$minCountDi))
  len.vars=length(this$selected.vars)
  set.mgp = c(1.1, 0.3, 0)
  
  ### checkbutton options
  checkTRANS = tclvalue(this$rbtrans)
  checkCALC = tclvalue(this$rbcalc)
  checkADDFILE = tclvalue(this$cbtaddFilename)
  checkDYNRANGE = tclvalue(this$cbtdynRange)
  checkSHOWMINBIN = tclvalue(this$cbtshowMinBins)
  checkDISPLAYA = tclvalue(this$cbtdisplayA)
  
  if (checkTRANS=="asinh") scipen = this$asinh$scipen
  else scipen = this$biex$scipen
  
  options(scipen=scipen)
  
  # legend from blue to red
  cols = this$col.rainbow
  cols.pale = this$col.rainbow.pale
  
  ### file name
  title.text=as.character(tclvalue(tkget(this$title,"1.0","end-1c")))
  displayfile=this$shortenFilename(title.text)
  
  ### write metadata in csv table
  metadatafile = sprintf("%s_PRIdivis_metadata.csv",this$current.project)
  if(!file.exists(metadatafile)) {
    header = c("date","sample", "cofactor","calc"
               ,"feat.A", "feat.B"
               ,"minVal.B","maxVal.B","absRange.B"
               ,"cutoff.A","cutoff.B"
               ,"neg.total","pos.total"
               ,"neg.prodcells","pos.prodcells")
    write.table(t(header), metadatafile, sep = "\t", row.names=F, col.names=F)
  }
  current.date = format(Sys.Date(),"%y%m%d")
  
  ### if cutoff is set for Feat. B, cut data from cutoff first
  if (cutoffs[1]>0 & checkCALC!="freq") {
    data=data[which(data[,1]> cutoffs[1]),]
    printf(">>>>>>>>>>>>>>>>> DATA CUTTED to %s",nrow(data))
  }
  
  ### if data is empty, only display 'empty data'
  if ( nrow(data) == 0 ) {
    if (this$distepy==0) text(x=xmax.val*1/2,y=bin.pos+0.2,labels="[no bins to display]"
                              ,cex=0.5*set.cex,adj=1,mgp=set.mgp,xpd=TRUE)
    else text(x=this$current.diploT.x-1,y=bin.pos+0.2,labels="[no bins to display]"
              ,cex=0.5*set.cex,adj=1,mgp=set.mgp,xpd=TRUE)
    empty=TRUE
  } else {
    ### set negative values of colnum 1 to zero
    if ( checkCALC != "density" ) data[which(data[,1]<0),1] = 0
    
    ### fX divides the range of data[,2] into intervals and codes the values in data[,2] according to which interval they fall. 
    # The leftmost interval corresponds to level one, the next leftmost to level two and so on.
    fX=cut(data[,2],breaks=seq(xmin.val,xmax.val,by=binSize),include.lowest=TRUE,dig.lab=5)
    
    if ( checkCALC == "density") {
      ### number of cells in bin
      my.calc=aggregate(data[,2],by=list(fX),length)
    } else if ( checkCALC == "SD" ) {
      my.calc=aggregate(data[,1],by=list(fX),sd)
      cols=this$col.blackwhite
    } else if ( checkCALC == "SEM" ) {
      my.calc=aggregate(data[,1],by=list(fX),function(x) {
        SEM = sd(x)/sqrt(length(x))
        ### if normally distributed, 95,4 % of the cells should lie inside the interval mean +/- SEM
        # interval_min = mean(x) - SEM
        # interval_max = mean(x) + SEM 
        SEM
      })
      cols=this$col.blackwhite
    } else if ( checkCALC == "RSEM" ) {
      my.calc=aggregate(data[,1],by=list(fX),function(x) {
        RSEM = sd(x)/sqrt(length(x))
        RSEM/mean(x)*100
      })
      cols=this$col.blackred
    } else if ( checkCALC == "freq" ) {
      my.calc = aggregate(data[,1],by=list(fX),function(x) {
        y= round( 100 * length(which(x >= cutoffs[1])) / length(x))
        return(y)
      })
    } else {#} if ( grepl("MSI",checkCALC) ) {
      my.calc=aggregate(data[,1],by=list(fX),mean)
    } 
    
    my.lengths=aggregate(data[,1],by=list(fX),length)
    my.calc=cbind(my.calc,ncells=my.lengths$x)
    
    ### get color steps and color "factors" my.calc.fac
    idx=which(my.calc$ncells>=mincells)
    idx.lower=which(my.calc$ncells<mincells & my.calc$ncells>0)
    range=""
    col.minmax="black"
    if (checkCALC == "freq") {
      ### bin color factor
      my.calc.fac=cut(my.calc$x,breaks=seq(0,100,by=10),labels=1:10,include.lowest=TRUE)
      levels(my.calc.fac)=c(0,levels(my.calc.fac),11,1)
    } else if (checkCALC == "RSEM") {
      ### bin color factor
      my.calc.fac=cut(my.calc$x,breaks=seq(0,50,by=5),labels=1:10,include.lowest=TRUE)
      levels(my.calc.fac)=c(0,levels(my.calc.fac),11,12)
      this$my.calc.fac2 = my.calc.fac
      for ( i in 1:length(my.calc.fac) ){
        if ( !is.na(my.calc$x[i]) ) {
          if (my.calc$x[i]>=50 & my.calc$ncells[i]>=mincells) {
            my.calc.fac[i] = 11
          }
        }
      }
    } else {
      min.y=floor(min(my.calc[idx,'x'])*10)/10
      max.y=ceiling(max(my.calc[idx,'x'])*10)/10
      
      ### IF NO BINS TO DISPLAY
      if ( is.infinite(min.y) ) {
        printf("I AM EMPTY, min.y = %s",min.y)
        empty=TRUE
        if (this$distepy==0) text(x=xmax.val*1/2,y=bin.pos+0.2,labels="[no bins to display]",cex=0.5*set.cex,adj=1,mgp=set.mgp,xpd=TRUE)
        else text(x=this$current.diploT.x-1,y=bin.pos+0.2,labels="[no bins to display]",cex=0.5*set.cex,adj=1,mgp=set.mgp,xpd=TRUE)
      } else {
        if ( grepl("MSI",checkCALC) & diff(c(min.y,max.y)) <= 0.5 ) col.minmax="red"
        if ( checkCALC=="density" & max.y < 100 ) col.minmax="red"
        
        ### get steps for legend and plot
        if ( checkCALC == "density" | checkDYNRANGE=="1" ) {
          min.range=min.y
          max.range=max.y
        } else {
          min.range=min.MSI
          max.range=max.MSI
          range=sprintf(", man range: %0.2f-%0.2f",min.range,max.range)
        }
      }
      
      if (!empty) {
        ### get steps
        step=round(diff(range(max.range,min.range))/10,2) 
        steps=seq(min.range,max.range,length.out=11)
        
        ### bin color factor
        # printf("steps=%s",paste(steps))
        my.calc.fac=cut(my.calc$x,breaks=steps,labels=2:11,include.lowest=TRUE)
        
        levels(my.calc.fac)=c(0,levels(my.calc.fac),11,12)
        ### if x < min.range
        my.calc.fac[which(my.calc$x<steps[1] & my.calc$ncells>=mincells)]=0
        ### if x > max.range
        my.calc.fac[which(my.calc$x>steps[11] & my.calc$ncells>=mincells)]=11
      }
    }
    
    ### if there are bins to display
    if (!empty) {
      my.calc=cbind(my.calc,fac=as.numeric(my.calc.fac)+1)
      
      ### get min, max and absolute bin range of feat. B in all bins with mincells
      range.B = range(my.calc$x[which(my.calc$ncells>=mincells)])
      minVal.B = round(range.B[1],3)
      maxVal.B  = round(range.B[2],3)
      absRange = round(diff(range.B),3)
      
      this$my.calc.fac = my.calc.fac
      this$my.calc = my.calc
      
      max.cells = 0
      min.cells = 10000
      maxX = 0
      minX = 10000
      
      ############ bindiplot
      plotting = TRUE
      if (checkDISPLAYA=="1") plotting = FALSE
      for ( i in min(idx,idx.lower):max(idx,idx.lower) ) {
        sub1=strsplit(as.character(my.calc$Group.1[i]),",")[[1]][1]
        if (grepl("^\\(",sub1)) sub2=as.numeric(strsplit(sub1,"\\(")[[1]][2])
        else sub2=as.numeric(strsplit(sub1,"\\[")[[1]][2])
        
        ### display only bins for Feat. A+
        if ( (sub2+binSize)>cutoffs[2]) {
          plotting=TRUE
          if (!exists("plot.xmin")) plot.xmin = sub2
          if (i == idx[length(idx)]) plot.xmax=sub2
        }
        
        ### bin configs
        # if i is in idx.lower then there were not enough cells in the bin
        # so plot the bin in pale
        bin.color = bin.border = bin.lwd = NA
        bin.border = NA
        if (any(i==idx.lower)) {
          if (checkSHOWMINBIN =="1") {
            if ( any(checkCALC==c("SEM","RSEM","SD")) ) {
              bin.color = cols[my.calc$fac[i]]
              bin.border = "white"
              bin.lwd = 3
            } else {
              bin.color = cols.pale[my.calc$fac[i]]
              bin.border = "white"
              bin.lwd = 3
            }
          }
        } else {
          bin.color = cols[my.calc$fac[i]]
          bin.border = "darkgrey"
          bin.lwd = 0.05
        }
        
        ### plot bin
        if (plotting) rect(sub2,bin.pos,sub2+binSize,bin.pos+0.5,col=bin.color,border=bin.border,lwd=bin.lwd)
        
        if (my.calc$ncells[i]>max.cells) max.cells=my.calc$ncells[i]   
        if (my.calc$ncells[i]<min.cells) min.cells=my.calc$ncells[i]   
        
        if (my.calc$x[i]>maxX) maxX=my.calc$x[i]   
        if (my.calc$x[i]<minX) minX=my.calc$x[i]   
        
        if (i == idx[1]) plot.filename = sub2
      }
      ################
      this$current.diploT.x = sub2
      
      ### set position where to text the percentages
      if (plot.xmin < xmin.val) {plot.xmin=xmin.val}
      
      ### draw line sigments between pairs of points
      segments(cutoffs[2], bin.pos-0.12, x1 = cutoffs[2], y1 = bin.pos+0.55)
      ### get percentage of cells left and right from cutoff x
      data.left = data.old[which(data.old[,2]<cutoffs[2]),]
      data.right = data.old[which(data.old[,2]>=cutoffs[2]),]
      cells.left = 100*(nrow(data.left))/data.lengths
      cells.right = 100-cells.left
      
      ### and plot the percentages of cells left and right from cutoff x
      text(x=plot.xmin-binSize,y=bin.pos+0.63+this$legend.space.di,label=sprintf("%0.1f%%",cells.left),cex=0.48*set.cex,pos=4)
      text(x=plot.xmax+binSize+0.2,y=bin.pos+0.63+this$legend.space.di,label=sprintf("%0.1f%%",cells.right),cex=0.48*set.cex,pos=2)
      
      is.csv.content = FALSE
      if ( cutoffs[2] != 0 & checkCALC == "freq") {
        ### plot cutoff line Feature A+/- and cell frequency
        
        ### get percentage of cells which are productive on left and right side
        prod.left = 100 * (nrow(data.left[which(data.left[,1]>=cutoffs[1]),])/nrow(data.left))
        prod.right = 100 * (nrow(data.right[which(data.right[,1]>=cutoffs[1]),])/nrow(data.right))
        
        text(x=plot.xmin+binSize,y=bin.pos+0.63+this$legend.space.di,label=sprintf("%0.1f%%",prod.left),cex=0.48*set.cex,col="red",pos=2)
        text(x=plot.xmax+binSize-0.2,y=bin.pos+0.63+this$legend.space.di,label=sprintf("%0.1f%%",prod.right),cex=0.48*set.cex,col="red",pos=4)
        
        csv.content <- cbind(current.date,displayfile,this$current.cofactor,checkCALC
                             ,colnames(data)[2],colnames(data)[1]
                             ,minVal.B,maxVal.B,absRange
                             ,cutoffs[1],cutoffs[2]
                             ,round(cells.left,2),round(cells.right,2)
                             ,round(prod.left,2 ),round(prod.right,2))
        write.table(csv.content, sprintf("%s_PRIdivis_metadata.csv",this$current.project), sep = "\t", col.names = F, row.names = F, append = T)
        printf("w: diploT meta1 written in %s.",sprintf("%s_PRIdivis_metadata.csv",this$current.project))
        
      }	else if ( cutoffs[2] != 0 & checkCALC!="density") {
        ### display production percentage of Feature B+/- on each side of cutoff line
        if ( cutoffs[1] != 0 ) {
          prod.left = 100 * (nrow(data.left[which(data.left[,1]>=cutoffs[1]),])/data.lengths)
          prod.right = 100 * (nrow(data.right[which(data.right[,1]>=cutoffs[1]),])/data.lengths)
          
          text(x=plot.xmin+binSize,y=bin.pos+0.63+this$legend.space.di,label=sprintf("%0.1f%%",prod.left),cex=0.48*set.cex,col="chartreuse4",pos=2)
          text(x=plot.xmax+binSize-0.2,y=bin.pos+0.63+this$legend.space.di,label=sprintf("%0.1f%%",prod.right),cex=0.48*set.cex,col="chartreuse4",pos=4)
          
          csv.content <- cbind(current.date,displayfile,this$current.cofactor,checkCALC
                               ,colnames(data)[2],colnames(data)[1]
                               ,minVal.B,maxVal.B,absRange
                               ,cutoffs[1],cutoffs[2]
                               ,round(cells.left,2),round(cells.right,2)
                               ,round(prod.left,2 ),round(prod.right,2))
          write.table(csv.content, sprintf("%s_PRIdivis_metadata.csv",this$current.project), sep = "\t", col.names = F, row.names = F, append = T)
          printf("w: diploT meta2 written in %s.",sprintf("%s_PRIdivis_metadata.csv",this$current.project))
        } 
      } else {
        csv.content <- cbind(current.date,displayfile,this$current.cofactor,checkCALC
                             ,colnames(data)[2],colnames(data)[1]
                             ,minVal.B,maxVal.B,absRange)
        write.table(csv.content, sprintf("%s_PRIdivis_metadata.csv",this$current.project), sep = "\t", col.names = F, row.names = F, append = T)
        printf("w: diploT meta3 written in %s.",sprintf("%s_PRIdivis_metadata.csv",this$current.project))
      }
      
      ### print min/max value
      if ( any(checkCALC ==c("density","freq","RSEM")) ) print.minmax=sprintf("min/max=%0.0f/%0.0f",minX,maxX)
      else print.minmax = sprintf("min/max=%0.2f/%0.2f%s",minX,maxX,range)
      # print to the right of plot area
      text(x=sub2+0.8,y=bin.pos-0.03,label=print.minmax,col=col.minmax,cex=0.48*set.cex*totalcex,pos=4,xpd=TRUE)
    }
  }
  
  ### print calculation method and cutoff if available
  print.label = checkCALC
  if (checkCALC != "freq" & cutoffs[1] > 0) {
    print.label=sprintf("%s(+), cutoff=%s",print.label,cutoffs[1])
  } else if (cutoffs[1] > 0) {
    print.label = sprintf("%s, cutoff=%s",print.label,cutoffs[1])
  }
  
  if (empty) {
    if (this$distepy=="1") text(x=xmax.val*2/3,y=bin.pos+0.21,label=print.label,cex=0.6*set.cex*totalcex,pos=4,col="red",xpd=TRUE)
    else text(x=this$current.diploT.x,y=bin.pos+0.34,label=print.label,cex=0.6*set.cex*totalcex,pos=4,col="red",xpd=TRUE)
  } else {
    text(x=sub2+0.8,y=bin.pos+0.34,label=print.label,cex=0.6*set.cex*totalcex,pos=4)
  }
  
  ### print second feature and file name on y axis horizontal
  if (checkADDFILE=="1") {
    this$axis.label[bin.pos+1]=sprintf("%s: ",displayfile)
  } 
  
  if ( cutoffs[1]>0 | checkCALC != "density" ) {
    # if ( all(checkCALC != c("density","SD","RSEM","SEM")) ) {
    this$axis.label[bin.pos+1]=paste0(this$axis.label[bin.pos+1],colnames(data)[1])
  } 
  
  text(x=plot.filename,y=bin.pos+0.2,labels=this$axis.label[bin.pos+1],
       cex=0.6*set.cex,pos=2,mgp=set.mgp,xpd=TRUE)
  
}

