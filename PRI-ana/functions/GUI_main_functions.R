#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020

### GUI functions ----------------------------------------
### Main frame of PRI-ana
fcs$GUImain <- function () {
  this <- fcs
  this$total.projects <- dbListTables(this$conn)
  
  this$dataframes.name <- vector()
  this$dataframes.name[1] <- "Rectangle_Data_Raw"
  this$dataframes.name[2] <- "Rectangle_Data_Transformed"
  
  # sort metadata from tables first ---------------------------
  # index for metadata
  idx <- grep("markerIdentity|colnameIndex|fileIdentity|fileIndex|UserHistory|Classification|equipmentInfo|fileComments|SPILL", this$total.projects)
  
  for (i in idx){
    this$df.num <- this$df.num + 1 
    
    nam <- paste("dataframes", this$df.num, sep="")
    df <- this$getDFtable(this$total.projects[i])
    assign(nam, df, envir=fcs) 
    this$dataframes.name[this$df.num] <- this$total.projects[i]
  }
  
  this$total.projects <- this$total.projects[-idx]
  this$current.project <- this$total.projects[1]
  
  ### get indices for filenames and table
  idx.table <- grep(paste0("^", this$total.projects[1], "_"), this$dataframes.name)
  idx.file <- grep("fileIdentity|fileIndex", this$dataframes.name)
  if (grepl("Identity", this$dataframes.name[idx.file[1]])) this$fileid_name <- "_fileIdentity"
  else this$fileid_name <- "_fileIndex"
  idx.stain <- grep("markerIdentity|colnameIndex", this$dataframes.name)[1]
  if (grepl("Identity", this$dataframes.name[idx.stain])) this$markerid_name <- "_markerIdentity"
  else this$markerid_name <- "_colnameIndex"
  
  
  ### get filenames for table
  this$current.filetable <- this$getDFtable(paste0(this$current.project, this$fileid_name))
  this$current.filenames <- this$current.filetable[, 2]
  
  ### find longest filename to display in combobox
  this$max.nchar <- 40
  for (i in this$current.filenames) {
    this$max.nchar <- max(nchar(i), this$max.nchar)
  }
  this$max.nchar <- this$max.nchar + 12
  
  this$current.staintable <- this$getDFtable(paste0(this$current.project, this$markerid_name))
  this$current.filenum <- this$current.filetable[which(this$current.filetable[, 2] == this$current.filenames[1]), 1]
  this$selected.filenum <- this$current.filenum
  this$selected.vars <- this$getVariables()
  this$current.vars <- this$selected.vars
  
  ### initiate lists -----------------------------------------------------------
  this$new.cutoffs <- this$new.checks <- vector(mode="list", length=nrow(this$current.filetable))
  if (ncol(this$current.staintable) == 6) {
    ### if cutoffs and checks were already saved
    # get all cutoffs and checks
    for (file.id in unique(this$current.filetable[, 1])) {
      this$new.cutoffs[[file.id]] <- this$current.staintable[which(this$current.staintable[, 1] == file.id), 5]
      this$new.checks[[file.id]] <- this$current.staintable[which(this$current.staintable[, 1] == file.id), 6]
    }
  } else if (ncol(this$current.staintable) == 5) {
    ### if only cutoffs were saved
    # get all cutoffs
    for (file.id in unique(this$current.filetable[, 1])) {
      this$new.cutoffs[[file.id]] <- this$current.staintable[which(this$current.staintable[, 1] == file.id), 5]
      this$new.checks[[file.id]] <- rep("0", nrow(this$current.staintable[which(this$current.staintable[, 1] == file.id), ]))
    }
  } else {
    ### no cutoffs listed in this project
    # initiate cutoff and check list
    for (file.id in unique(this$current.filetable[, 1])) {
      this$new.cutoffs[[file.id]] <- rep("0", nrow(this$current.staintable[which(this$current.staintable[, 1] == file.id), ]))
      this$new.checks[[file.id]] <- rep("0", nrow(this$current.staintable[which(this$current.staintable[, 1] == file.id), ]))
    }
  }
  this$saved.cutoffs <- this$new.cutoffs
  this$saved.checks <- this$new.checks
  
  ### get current cutoffs for our current file
  this$selected.cutoffs <- this$new.cutoffs[[this$current.filenum]]
  this$current.cutoffs <- this$selected.cutoffs
  this$current.checks <- this$new.checks[[this$current.filenum]]
  this$recent.checks <- this$current.checks
  
  ### start GUI
  this$tt <- tktoplevel()
  
  ### set window title
  tkwm.title(this$tt, "PRI-ana")
  
  ### create topMenu
  topMenu <- tkmenu(this$tt)
  tkconfigure(this$tt, menu=topMenu)
  if (this$working) {
    tkadd(topMenu, "command", label="Refresh", command=this$exitandstart)
    tkadd(topMenu, "command", label="Quit", command=this$exit)
    #tkadd(topMenu, "command", label="Debugger", command=this$debug)
    tkadd(topMenu, "command", label=sprintf("Open project (%s)", length(this$total.projects)), command=this$openProject)
    tkadd(topMenu, "command", label="Open new database", command=this$openDB)
    fileMenu2 <- tkmenu(topMenu)
      tkadd(fileMenu2, "command", label = "Cutoffs calculation", command = function() {
          this$GUIpreproc(mode = 1)
      })
      tkadd(fileMenu2, "command", label = "Normalization", command = function() {
          this$GUIpreproc(mode = 2)
      })
      tkadd(fileMenu2, "command", label = "Show legend colors", command = function() {
          this$plotLegend()
      })
    tkadd(topMenu, "cascade", label="Preprocession", menu=fileMenu2)
  } else {
    fileMenu <- tkmenu(topMenu)
    tkadd(fileMenu, "command", label="Open project", command=this$openProject)
    tkadd(fileMenu, "command", label="Open database", command=this$openDB)
    tkadd(fileMenu, "command", label="Quit", command=this$exit)
      tkadd(fileMenu, "command", label = "Show legend colors", command = function() {
          this$plotLegend()
      })
    tkadd(topMenu, "cascade", label=sprintf("File (%s)", length(this$total.projects)), menu=fileMenu)
    
    fileMenu2 <- tkmenu(topMenu)
      tkadd(fileMenu2, "command", label = "Cutoffs calculation", command = function() {
          this$GUIpreproc(mode = 1)
      })
      tkadd(fileMenu2, "command", label = "Normalization", command = function() {
          this$GUIpreproc(mode = 2)
      })
    tkadd(topMenu, "cascade", label="Preprocession", menu=fileMenu2)
  }
  tkadd(topMenu, "command", label=sprintf("%s - DB: %s", this$version, sub(".sqlite3", "", this$db.name)))
  
  
  len.vars <- length(this$selected.vars)
  this$max.nchar.vars <- max(nchar(this$selected.vars))
  
  this$width.panel <- 315 + this$max.nchar.vars
  
  # start frame windows 
  ttf_main <- tkframe(this$tt)
  
  # top frame with file names and tabs
  ttf_maintop <- tkframe(ttf_main)
  
  ### select/deselect buttons
  ttfButtons <- tkframe(ttf_maintop)
  ### set label frame select markers of interest
  ttfselButtons <- tkframe(ttfButtons)
  btselAll <- tkbutton(ttfselButtons, text="Select all vars", command=function() {
    for (i in 1:len.vars) {
      tclvalue(this$cbVal[[i]]) <- "1"
    }
  })
  btdeselAll <- tkbutton(ttfselButtons, text="Deselect all vars", command=function() {
    for (i in 1:len) {
      tclvalue(this$cbVal[[i]]) <- "0"
    }
  })
  tkgrid(btselAll, btdeselAll, padx=6, pady=2, sticky="w")
  tkgrid(ttfselButtons)
  ###
  
  ### cutoff buttons
  ttfloadsave <- tkframe(ttfButtons)
    btuseSavedCutoffs <- tkbutton(ttfloadsave, text = "Load cutoffs", command = function() {
        this$refreshCutoffs(saved = TRUE)
    })
    btsaveCutoffs2all <- tkbutton(ttfloadsave, text = "Save cutoffs to all", command = function() {
        this$saveCutoffsToAll()
    })
  tkgrid(btuseSavedCutoffs, btsaveCutoffs2all, padx=5, sticky="we")
  tk2tip(btuseSavedCutoffs, "Load from database. Only if available.")
  tk2tip(btsaveCutoffs2all, "Save cutoffs to all files in project.")
  tkgrid(ttfloadsave, pady=2, sticky="we")
    btresetCutoffs <- tkbutton(ttfButtons, text = "Reset cutoffs", command = function() {
        this$refreshCutoffs(reset = TRUE)
    })
  tkgrid(btresetCutoffs, pady=2, padx=5, sticky="we")
  
  # select files --------------------------------------------------------
  ttf_filenames_asinh <- tkframe(ttf_maintop)
  ttf_filenames <- tkframe(ttf_filenames_asinh)
  ttfchoosefile <- tkframe(ttf_filenames, height=20)
  this$tkchoosefile <- ttkcombobox(ttfchoosefile, values=this$current.filenames, width=this$max.nchar - 4)
  tkset(this$tkchoosefile, this$current.filenames[1])
    btrefreshPanel <- tkbutton(ttfchoosefile, text = "GUI", command = function() {
        this$refreshPanel()
    })
  tkgrid(btrefreshPanel, tklabel(ttfchoosefile, text="File:"), this$tkchoosefile, padx=5, sticky="w")
  tk2tip(btrefreshPanel, "Refresh the marker panel to make it scrollable again.")
    tkbind(this$tkchoosefile, "<<ComboboxSelected>>", function() {
        this$refreshComboboxVars(tclvalue(tkget(this$tkchoosefile)))
    })
  tkgrid(ttfchoosefile, pady=1)
  
  ttlfcomment <- ttklabelframe(ttf_filenames, text="Title text")
  this$title <- tk2text(ttlfcomment, width=this$max.nchar - 15, height=2)
  displayfile <- this$shortenFilename(this$current.filenames[1], title=TRUE)
  tkinsert(this$title, "1.0", displayfile)
    btaddtitle <- tkbutton(ttlfcomment, text = "Add title", command = function() {
        this$addTitle(mode = "tri")
    })
    btadddate <- tkbutton(ttlfcomment, text = "Add date", command = function() {
        this$addTitle(mode = "date")
    })
  tkgrid(this$title, btaddtitle, btadddate, padx=6, pady=2)
  tkgrid(ttlfcomment, pady=2)
  
  # asinh radiobuttons --------------------------------------------------------
  ttf_radioasinh <- tkframe(ttf_filenames_asinh)
  tkgrid(tklabel(ttf_radioasinh, text="asinh cof"), sticky="e")
  tk2tip(ttf_radioasinh, "arcsinh() cofactor")
  tfradios <- tkframe(ttf_radioasinh)
  rb1 <- tkradiobutton(tfradios, variable=this$rbasinh, value="0.1")
  rb2 <- tkradiobutton(tfradios, variable=this$rbasinh, value="0.2")
  rb3 <- tkradiobutton(tfradios, variable=this$rbasinh, value="1")
  rb4 <- tkradiobutton(tfradios, variable=this$rbasinh, value="5")
  rb5 <- tkradiobutton(tfradios, variable=this$rbasinh, value="150")
  tkgrid(rb1, tklabel(tfradios, text="1/10"))
  tkgrid(rb2, tklabel(tfradios, text="1/5"))
  tkgrid(rb3, tklabel(tfradios, text="1"), sticky="we")
  tkgrid(rb4, tklabel(tfradios, text="5"), sticky="we")
  tkgrid(rb5, tklabel(tfradios, text="150"), sticky="we")
  tk2tip(rb1, "asinh(x*10)")
  tk2tip(rb2, "asinh(x*5)")
  tk2tip(rb3, "asinh(x)")
  tk2tip(rb4, "asinh(x/5)")
  tk2tip(rb5, "asinh(x/150)")
  tkgrid(tfradios)
  
  tkgrid(ttf_filenames, ttf_radioasinh, sticky="we", padx=2)
  tkgrid(ttfButtons, ttf_filenames_asinh, padx=10, sticky="w")
  tkgrid(ttf_maintop, sticky="w")
  ###
  
  ### ------------- bottom frame 
  ttf_mainbottom <- tkframe(ttf_main)
  this$ttf_markers <- tkframe(ttf_mainbottom)
  ### scrollable frame
  this$sw <- tkwidget(this$ttf_markers, "ScrolledWindow", relief="sunken", borderwidth=2)
  scrollFrame <- tkwidget(this$sw, "ScrollableFrame", height=630, width=this$width.panel)
  tcl(this$sw, "setwidget", scrollFrame)
  subfID <- tclvalue(tcl(scrollFrame, "getframe"))
  
  # label project name
  this$project.lab <- tcl("label", paste(subfID, ".lab", sep=""), text=sprintf(">>%s", this$shortenFilename(this$current.project)))
  tkgrid(this$project.lab, sticky="w", padx=5)
  
  i <- j <- 1
  while (i < (len.vars * 4)) {
    ### initiate
    this$vcutoffs[[j]] <- tclVar(this$selected.cutoffs[j])
    this$cbVal[[j]] <- tclVar(this$current.checks[j])
    this$cbcutoffperc[[j]] <- tclVar("0")
    
    cutoffcb <- tcl("checkbutton", paste(subfID, ".", i, sep=""), 
    variable=this$cbVal[[j]], text=this$selected.vars[j])
    cutofflabel <- tcl("label", paste(subfID, ".", i + 1, sep=""), text="cutoff: ")
    cutoffentry <- tcl("entry", paste(subfID, ".", i + 2, sep=""), width=6, textvariable=this$vcutoffs[[j]])
    cutoffperccb <- tcl("checkbutton", paste(subfID, ".", i + 3, sep=""), variable=this$cbcutoffperc[[j]], text="in %")
    tkgrid(cutoffcb, cutofflabel, cutoffentry, cutoffperccb, sticky="w", pady=1)
    i <- i + 4
    j <- j + 1
  }
  tkgrid(this$sw)
  ###
  
  # notebook panels --------------------------------------------------------
  ttf_tabs <- tkframe(ttf_mainbottom)
  this$panelTabs <- ttknotebook(ttf_tabs)
  tab1 <- ttkframe(this$panelTabs)
  tab2 <- ttkframe(this$panelTabs)
  tab3 <- ttkframe(this$panelTabs)
  tab4 <- ttkframe(this$panelTabs)
  tab5 <- ttkframe(this$panelTabs)
  
  tkadd(this$panelTabs, tab1, text="n-diploTs")
  tkadd(this$panelTabs, tab2, text="n-triploTs")
  tkadd(this$panelTabs, tab3, text="n-quadruploTs")
  tkadd(this$panelTabs, tab4, text="table info")
  tkadd(this$panelTabs, tab5, text="log")
  tkbind(this$panelTabs, "<<NotebookTabChanged>>", this$clickTab)
  
  # tab: diploT
  this$GUIdiploT(tab1)
  # tab: triploT
  this$GUItriploT(tab2)
  # tab: quadruplot
  this$GUIquadruploT(tab3)
  # tab: table info
  this$GUIdataframe(tab4)
  # tab: log
  this$GUIlog(tab5)
  tkgrid(this$panelTabs, sticky="w", padx=5)
  ###
  
  tkgrid(this$ttf_markers, ttf_tabs)
  tkgrid(ttf_mainbottom, pady=2, sticky="ns")
  tkgrid(ttf_main, pady=2)
  
  this$refreshComboboxStart(1)
  
  this$openProjectYet <- TRUE
  
  ### if there are more than one project, chose a project first
  if (length(this$total.projects) > 1) {
    this$openProjectYet <- FALSE
    printf("Chose your project first.")
    this$openProject()
    tkraise(this$ttprojects)
    
    ### if not clicked on a project yet -------------------------------------------
    # but clicked on main window already
    # bind mouse click with function
    if (!this$openProjectYet) {
      tkbind(this$tt, "<Button-1>", function(...) {
        tkmessageBox(title="Chose project", message="Chose your project first.", icon="info", type="ok")
        tkraise(this$ttprojects)
      })
      tkraise(this$ttprojects)
    } 
  } else {
    tkraise(this$tt)
  }
}

### quadruploT tab
# tab     tab frame, no default
fcs$GUIquadruploT <- function(tab) {
  this <- fcs
  
  if (this$working) printf("w: GUIquadruploT")

  # set frame and content of combobox with file, mincount, binSize
  ttftopframe <- tkframe(tab)
    ttfcombo <- tkframe(ttftopframe)
      # content comboboxleft
      var.length <- 39
      if (this$max.nchar.vars > var.length) var.length <- this$max.nchar.vars
      
      this$minCountQuad <- ttkcombobox(ttfcombo, values=this$min.counts, width=var.length)
      tkset(this$minCountQuad, this$min.counts[param$minCountQuadPos])
      this$binSizeQuad <- ttkcombobox(ttfcombo, values=this$binSizes, width=var.length)
      tkset(this$binSizeQuad, this$binSizes[param$binSizesQuadPos])
      
      this$cbvar1quad <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar1quad, this$selected.vars[param$currVarQuad1])
      this$cbvar2quad <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar2quad, this$selected.vars[param$currVarQuad2])
      this$cbvar3quad <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar3quad, this$selected.vars[param$currVarQuad3])
      this$cbvar4quad <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar4quad, this$selected.vars[param$currVarQuad4])
    # call comboboxleft
    tkgrid(tklabel(ttfcombo, text="binSize:"), this$binSizeQuad, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="minCount:"), this$minCountQuad, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature A:"), this$cbvar1quad, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature B:"), this$cbvar2quad, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature C1:"), this$cbvar3quad,  sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature C2:"), this$cbvar4quad,  sticky="e", padx=5)
  
    #### rect buttons "Plot"
    ttfbut <- tkframe(ttftopframe)
      btplot <- tkbutton(ttfbut, text="Plot quadruploT", command=this$doquadruploT)
      bthist <- tkbutton(ttfbut, text = "Plot histograms", command = function(){
        this$plotHistograms(plotter = "tri", pdf = FALSE)
        })
    
      tkgrid(btplot, sticky="snwe", padx=2, pady=1)
      tkgrid(bthist, sticky="snwe", padx=2, pady=1)
      tkconfigure(btplot, height=2, width=25)
    tkgrid(ttfcombo, ttfbut, padx=5, sticky="w")
  tkgrid(ttftopframe)

  ##### set rect frame ttfvals with additional options min/max FI values  
  ttfvals <- tkframe(tab)
    # set label frame min/max FI in rect frame "ttfvals"
    ttlfFI <- ttklabelframe(ttfvals, text = "Plot options")
    
      # checkbox axes range label -------------------------------------------
      ttfRange <- tkframe(ttlfFI)
        ttfRangeLabel <- tkframe(ttfRange)
        tkgrid(tklabel(ttfRangeLabel, text="Range x-axis (min/max):"), padx=10, pady=1)
        tkgrid(tklabel(ttfRangeLabel, text="Range y-axis (min/max):"), padx=10, pady=1)
        cbtdynrange <- tkcheckbutton(ttfRangeLabel, variable=this$cbtdynRange, text="Dynamic frequency range")
        tkgrid(cbtdynrange, sticky="ews", padx=10, pady=10)
        tk2tip(cbtdynrange, "Uncheck for manual input")
        # checkbox axes range value -------------------------------------------
        ttfRangeValue <- tkframe(ttfRange)
        # X axis
        this$minvalX <- tkentry(ttfRangeValue, width=6, textvariable=this$vminvalX)
        this$maxvalX <- tkentry(ttfRangeValue, width=6, textvariable=this$vmaxvalX)
        tkgrid(this$minvalX, this$maxvalX, padx=5, pady=1, sticky="e")
        # Y axis
        this$minvalY <- tkentry(ttfRangeValue, width=6, textvariable=this$vminvalY)
        this$maxvalY <- tkentry(ttfRangeValue, width=6, textvariable=this$vmaxvalY)
        tkgrid(this$minvalY, this$maxvalY, padx=5, pady=1, sticky="e")
        # Z range
        this$minfreq <- tkentry(ttfRangeValue, width=4, textvariable=this$vminfreq)
        this$maxfreq <- tkentry(ttfRangeValue, width=4, textvariable=this$vmaxfreq)
        tkgrid(tklabel(ttfRangeValue, text="min(freq):"), this$minfreq, padx=5, pady=1, sticky="e")
        tkgrid(tklabel(ttfRangeValue, text="max(freq):"), this$maxfreq, padx=5, pady=1, sticky="e")
        tkgrid(ttfRangeLabel, ttfRangeValue, sticky="w")
        tkgrid(ttfRange)
        
      # checkbox show grid --------------------------------------------------
      ttfcheckPlot <- tkframe(ttlfFI)
        cbtshowgrid <- tkcheckbutton(ttfcheckPlot, variable=this$cbtshowGrid, text="Grid")
        tk2tip(cbtshowgrid, "Check to plot grid lines")
        cbtpercentage <- tkcheckbutton(ttfcheckPlot, variable=this$cbtshowPercentage, text="Percentages")
        tk2tip(cbtpercentage, "Check to plot percentages")
        cbtshowlegend <- tkcheckbutton(ttfcheckPlot, variable=this$cbtshowLegend, text="Legend")
        tk2tip(cbtshowlegend, "Uncheck to hide legend on plot")
        cbtaddDate <- tkcheckbutton(ttfcheckPlot, variable=this$cbtaddDate, text="Date")
        tkgrid(cbtshowgrid, cbtpercentage, cbtshowlegend, cbtaddDate, padx=5, pady=8, sticky="we")
      tkgrid(ttfcheckPlot)
    ### Done option panel
  
    # radio buttons for transformation type -----------------------------------
    ttlfinfo <- ttklabelframe(ttfvals, text="Calculation options")
      ttfradios <- tkframe(ttlfinfo)
      ttfradiosright1 <- tkframe(ttfradios)
      rb1 <- tkradiobutton(ttfradiosright1, variable=this$rbtrans, value="asinh")
      rb2 <- tkradiobutton(ttfradiosright1, variable=this$rbtrans, value="none")
      tkgrid(rb1, tklabel(ttfradiosright1, text="asinh"), 
            rb2, tklabel(ttfradiosright1, text="none"), sticky="we")
      tkgrid(tklabel(ttfradios, text="Transformation:"), ttfradiosright1, sticky="w")
        tkbind(rb1, "<Button-1>", function() {
            this$changeFI.range(1)
        })
      
      # add radio buttons for calculation type
      ttfradiosright2 <- tkframe(ttfradios)
        rb1 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="density")
        rbl1 <- tklabel(ttfradiosright2, text="density")
        rb2 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="freq")
        rbl2 <- tklabel(ttfradiosright2, text="frequency")
        tkgrid(rb1, rbl1, rb2, rbl2, sticky="w")
        tk2tip(rb1, "Cell count")
        tk2tip(rbl1, "Cell count")
        tk2tip(rb2, "Frequency of Feature C1/C2 cells")
        tk2tip(rbl2, "Frequency of Feature C1/C2 cells")
      tkgrid(tklabel(ttfradios, text = "Statistical method:"), ttfradiosright2, sticky = "nw")

      ttfradiospopC1 <- tkframe(ttfradios)
        rb1 <- tkradiobutton(ttfradiospopC1, variable = this$popC1, value = "pos")
        rbl1 <- tklabel(ttfradiospopC1, text = "pos")
        rb2 <- tkradiobutton(ttfradiospopC1, variable = this$popC1, value = "neg")
        rbl2 <- tklabel(ttfradiospopC1, text = "neg")
        tkgrid(rb1, rbl1, rb2, rbl2, sticky = "w")
        tk2tip(rb1, "Choose neg/pos population.")
        tk2tip(rbl1, "Choose neg/pos population.")
        tk2tip(rb2, "Choose neg/pos population.")
        tk2tip(rbl2, "Choose neg/pos population.")
      tkgrid(tklabel(ttfradios, text = "Population C1:"), ttfradiospopC1, sticky = "nw")
  

      
      tkgrid(ttfradios, padx = 10)
      
      ttfbuttons <- tkframe(ttlfinfo)
        btremovdub <- tkbutton(ttfbuttons, text = "Remove Doublets", command = function() {
            this$preprocData()
        })
        tk2tip(btremovdub, "Only possible with FSH/SSH channels")
        cbttrimming <- tkcheckbutton(ttfbuttons, variable=this$cbttrimming, text="Trim first")
        tk2tip(cbttrimming, sprintf("Trims %s of each column.", this$trim.num))
        cbtshowMinBins <- tkcheckbutton(ttfbuttons, variable=this$cbtshowMinBins, text="Bins<minCount")
        tk2tip(cbtshowMinBins, "Show bins in pale color.")
        tkgrid(btremovdub, cbttrimming, cbtshowMinBins, padx=5, pady=1, sticky="we")
      tkgrid(ttfbuttons, sticky="w")
    tkgrid(ttlfFI, ttlfinfo, padx=5, pady=1, sticky="nsew")
  tkgrid(ttfvals)
  #####
  
}



### triploT tab
# tab     tab frame, no default
fcs$GUItriploT <- function(tab){
  this <- fcs
  
  if (this$working) printf("w: GUItriploT")
  
  # set frame and content of combobox with file, mincount, binSize
  ttftopframe <- tkframe(tab)
    ttfcombo <- tkframe(ttftopframe)
      # content comboboxleft
      var.length <- 39
      if (this$max.nchar.vars > var.length) var.length <- this$max.nchar.vars
      this$minCountTri <- ttkcombobox(ttfcombo, values=this$min.counts, width=var.length)
      tkset(this$minCountTri, this$min.counts[param$minCountTriPos])
      this$binSize <- ttkcombobox(ttfcombo, values=this$binSizes, width=var.length)
      tkset(this$binSize, this$binSizes[param$binSizesTriPos])
      
      this$cbvar1 <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar1, this$selected.vars[param$currVarTri1])
      this$cbvar2 <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar2, this$selected.vars[param$currVarTri2])
      this$cbvar3 <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
      tkset(this$cbvar3, this$selected.vars[param$currVarTri3])
      cbtfeatA <- tkcheckbutton(ttfcombo, variable=this$cbtfeatA)
      cbtfeatB <- tkcheckbutton(ttfcombo, variable=this$cbtfeatB)
      cbtfeatC <- tkcheckbutton(ttfcombo, variable=this$cbtfeatC)
    # call comboboxleft
    tkgrid(tklabel(ttfcombo, text="binSize:"), this$binSize, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="minCount:"), this$minCountTri, tklabel(ttfcombo, text="Fix"), sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature A:"), this$cbvar1, cbtfeatA, sticky="e", padx=5)
    tk2tip(cbtfeatA, "Check to fix this feature for triploT-Overview (not working yet)")
    tkgrid(tklabel(ttfcombo, text="Feature B:"), this$cbvar2, cbtfeatB, sticky="e", padx=5)
    tk2tip(cbtfeatB, "Check to fix this feature for triploT-Overview (not working yet)")
    tkgrid(tklabel(ttfcombo, text="Feature C:"), this$cbvar3, cbtfeatC, sticky="e", padx=5)
    tk2tip(cbtfeatC, "Check to fix this feature for triploT-Overview (not working yet)")
  
    #### rect buttons "Plot"
    ttfbut <- tkframe(ttftopframe)
      btplot <- tkbutton(ttfbut, text="Plot triploT", command=this$dotriploT)
      btplot2 <- tkbutton(ttfbut, text="Plot for all files", command=this$dotriploTfiles)
      yhbtplot2 <- tkbutton(ttfbut, text="Read Table", command=this$dotriploTtable)
      bthist <- tkbutton(ttfbut, text = "Plot histograms", command = function() {
        this$plotHistograms(plotter = "tri", pdf = FALSE)
        })
    
      tkgrid(btplot, sticky="snwe", padx=2, pady=1)
      tkgrid(btplot2, sticky="snwe", padx=2, pady=1)
      tkgrid(yhbtplot2, sticky="snwe", padx=2, pady=1)
      tkgrid(bthist, sticky="snwe", padx=2, pady=1)
      tkconfigure(btplot, height=2, width=25)
    tkgrid(ttfcombo, ttfbut, padx=5, sticky="w")
  tkgrid(ttftopframe)
  
  
  ##### set rect frame ttfvals with additional options min/max FI values  
  ttfvals <- tkframe(tab)
    # set label frame min/max FI in rect frame "ttfvals"
    ttlfFI <- ttklabelframe(ttfvals, text="Plot options")
      # checkbox axes range label -------------------------------------------
      ttfRange <- tkframe(ttlfFI)
        ttfRangeLabel <- tkframe(ttfRange)
        tkgrid(tklabel(ttfRangeLabel, text="Range x-axis (min/max):"), padx=10, pady=1)
        tkgrid(tklabel(ttfRangeLabel, text="Range y-axis (min/max):"), padx=10, pady=1)
        cbtdynrange <- tkcheckbutton(ttfRangeLabel, variable=this$cbtdynRange, text="Dynamic MSI range")
        tkgrid(cbtdynrange, sticky="ews", padx=10, pady=10)
        tk2tip(cbtdynrange, "Uncheck for manual input")
        # checkbox axes range value -------------------------------------------
        ttfRangeValue <- tkframe(ttfRange)
        # X axis
        this$minvalX <- tkentry(ttfRangeValue, width=6, textvariable=this$vminvalX)
        this$maxvalX <- tkentry(ttfRangeValue, width=6, textvariable=this$vmaxvalX)
        tkgrid(this$minvalX, this$maxvalX, padx=5, pady=1, sticky="e")
        # Y axis
        this$minvalY <- tkentry(ttfRangeValue, width=6, textvariable=this$vminvalY)
        this$maxvalY <- tkentry(ttfRangeValue, width=6, textvariable=this$vmaxvalY)
        tkgrid(this$minvalY, this$maxvalY, padx=5, pady=1, sticky="e")
        # Z range
        this$minMSI <- tkentry(ttfRangeValue, width=4, textvariable=this$vminMSI)
        this$maxMSI <- tkentry(ttfRangeValue, width=4, textvariable=this$vmaxMSI)
        tkgrid(tklabel(ttfRangeValue, text="min(MSI):"), this$minMSI, padx=5, pady=1, sticky="e")
        tkgrid(tklabel(ttfRangeValue, text="max(MSI):"), this$maxMSI, padx=5, pady=1, sticky="e")
        tkgrid(ttfRangeLabel, ttfRangeValue, sticky="w")
        tkgrid(ttfRange)
        
      # checkbox show grid --------------------------------------------------
      ttfcheckPlot <- tkframe(ttlfFI)
        cbtshowgrid <- tkcheckbutton(ttfcheckPlot, variable=this$cbtshowGrid, text="Grid")
        tk2tip(cbtshowgrid, "Check to plot grid lines")
        cbtpercentage <- tkcheckbutton(ttfcheckPlot, variable=this$cbtshowPercentage, text="Percentages")
        tk2tip(cbtpercentage, "Check to plot percentages")
        cbtshowlegend <- tkcheckbutton(ttfcheckPlot, variable=this$cbtshowLegend, text="Legend")
        tk2tip(cbtshowlegend, "Uncheck to hide legend on plot")
        cbtaddDate <- tkcheckbutton(ttfcheckPlot, variable=this$cbtaddDate, text="Date")
        tkgrid(cbtshowgrid, cbtpercentage, cbtshowlegend, cbtaddDate, padx=5, pady=8, sticky="we")
      tkgrid(ttfcheckPlot)
  ### Done option panel
  
  # radio buttons for transformation type -----------------------------------
    ttlfinfo <- ttklabelframe(ttfvals, text="Calculation options")
      ttfradios <- tkframe(ttlfinfo)
      ttfradiosright1 <- tkframe(ttfradios)
      rb1 <- tkradiobutton(ttfradiosright1, variable=this$rbtrans, value="asinh")
      rb2 <- tkradiobutton(ttfradiosright1, variable=this$rbtrans, value="none")
      tkgrid(rb1, tklabel(ttfradiosright1, text="asinh"), 
            rb2, tklabel(ttfradiosright1, text="none"), sticky="we")
      tkgrid(tklabel(ttfradios, text="Transformation:"), ttfradiosright1, sticky="w")
        tkbind(rb1, "<Button-1>", function() {
            this$changeFI.range(1)
        })
      
      # add radio buttons for calculation type
      ttfradiosright2 <- tkframe(ttfradios)
        rb1 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="MSI")
        rbl1 <- tklabel(ttfradiosright2, text="MSI")
        rb2 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="MSI(+)")
        rbl2 <- tklabel(ttfradiosright2, text="MSI(+)")
        rb3 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="density")
        rbl3 <- tklabel(ttfradiosright2, text="density")
        rb4 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="freq")
        rbl4 <- tklabel(ttfradiosright2, text="frequency")
        rb5 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="SD")
        rbl5 <- tklabel(ttfradiosright2, text="SD")
        rb6 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="SEM")
        rbl6 <- tklabel(ttfradiosright2, text="SEM")
        rb7 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="RSEM")
        rbl7 <- tklabel(ttfradiosright2, text="RSEM")
        tkgrid(rb1, rbl1, rb2, rbl2, sticky="w")
        tk2tip(rb1, "Mean signal intensity")
        tk2tip(rbl1, "Mean signal intensity")
        tk2tip(rb2, "Mean signal intensity of Feature C producing cells")
        tk2tip(rbl2, "Mean signal intensity of Feature C producing cells")
        tkgrid(rb3, rbl3, rb4, rbl4, sticky="w")
        tk2tip(rb3, "Cell count")
        tk2tip(rbl3, "Cell count")
        tk2tip(rb4, "Frequency of Feature C producing cells")
        tk2tip(rbl4, "Frequency of Feature C producing cells")
        tkgrid(rb5, rbl5, rb6, rbl6, sticky="w")
        tk2tip(rb5, "Standard deviation")
        tk2tip(rbl5, "Standard deviation")
        tk2tip(rb6, "Standard error of mean")
        tk2tip(rbl6, "Standard error of mean")
        tkgrid(rb7, rbl7, sticky="w")
        tk2tip(rb7, "Relative standard error of mean")
        tk2tip(rbl7, "Relative standard error of mean")
        tkgrid(tklabel(ttfradios, text="Statistical method:"), ttfradiosright2, sticky="nw")
      tkgrid(ttfradios, padx = 10)
      
      ttfbuttons <- tkframe(ttlfinfo)
        btremovdub <- tkbutton(ttfbuttons, text = "Remove Doublets", command = function() {
            this$preprocData()
        })
        tk2tip(btremovdub, "Only possible with FSH/SSH channels")
        cbttrimming <- tkcheckbutton(ttfbuttons, variable=this$cbttrimming, text="Trim first")
        tk2tip(cbttrimming, sprintf("Trims %s of each column.", this$trim.num))
        cbtshowMinBins <- tkcheckbutton(ttfbuttons, variable=this$cbtshowMinBins, text="Bins<minCount")
        tk2tip(cbtshowMinBins, "Show bins in pale color.")
        tkgrid(btremovdub, cbttrimming, cbtshowMinBins, padx=5, pady=1, sticky="we")
      tkgrid(ttfbuttons, sticky="w")
    tkgrid(ttlfFI, ttlfinfo, padx=5, pady=1, sticky="nsew")
  tkgrid(ttfvals)
  #####
  
  ##### set rect frame ttfgraph with Graphics/Comment and Set Rectangle   
  ttfgraph <- tkframe(tab)
    ##### set rect frame ttfgraphcomment with Graphics and Comment
    ttfgraphcomment <- tkframe(ttfgraph)
      # frame: Graphics
      btOverviewXY <- tkbutton(ttfgraphcomment, text = "Plot triploT-Overview with fixed Features A and B", command = function() {
          this$dotriploTOverviewXY()
          })
      btOverviewX <- tkbutton(ttfgraphcomment, text = "Plot triploT-Overview with fixed Feature A", command = function() {
          this$dotriploTOverviewX()
          })
      btOverview <- tkbutton(ttfgraphcomment, text = "Plot triploT-Overview total", command = function() {
          this$dotriploTOverview()
          })
      tkgrid(btOverviewXY, sticky="snwe", pady=1, padx=5)
      tkconfigure(btOverviewXY, height=2)
      tk2tip(btOverviewX, "Select your Feature A and Y above and check all features to the left which you would like to plot as heat bins.")
      tkgrid(btOverviewX, sticky="snwe", pady=1, padx=5)
      tkconfigure(btOverviewX, height=2)
      tk2tip(btOverviewX, "Select your Feature A above and check all features to the left which you would like to plot.")
      tkgrid(btOverview, sticky="snwe", pady=1, padx=5)
      tkconfigure(btOverview, height=2)
      tk2tip(btOverview, "Check your features to the left. If more than 16 markers are selected, several PDFs will be created.")
      
      ttlfgraph <- ttklabelframe(ttfgraphcomment, text="Graphics")
        ttfrowcol <- tkframe(ttlfgraph)
        this$ncol <- tkentry(ttfrowcol, width=2, textvariable=this$vncol)
        this$nrow <- tkentry(ttfrowcol, width=2, textvariable=this$vnrow)
        # call buttons and frame Graphics
        tkgrid(tklabel(ttfrowcol, text="Rows:"), this$nrow, padx=10, sticky="e")
        tkgrid(tklabel(ttfrowcol, text="Columns:"), this$ncol, padx=10, sticky="e")
        tkgrid(tkbutton(ttfrowcol, text="New plot window", command=this$newPlotWindow))
        ttfgridsave <- tkframe(ttlfgraph)
          ttfsave <- tkframe(ttfgridsave)
            btsavePNG <- tkbutton(ttfsave, text = "Save last plot as PNG", command = function() {
                this$savePNG(single = TRUE)
            })
            btsavePDF <- tkbutton(ttfsave, text = "Save active window", command = function() {
                this$saveWindow(type = "triploT")
              })
            tkgrid(btsavePNG, pady=1, sticky="we")
            tkgrid(btsavePDF, pady=1, sticky="we")
            tk2tip(btsavePDF, "Save as pdf")
            tk2tip(btsavePNG, "Save as jpg")
          tkgrid(ttfsave, padx=3)
        tkgrid(ttfrowcol, ttfgridsave, padx = 5, pady = 1)
      tkgrid(ttlfgraph, padx=5, pady=1)
  
      ### set label frame "Set Rectangle"
      ttlfRect <- ttklabelframe(ttfgraph, text="Set rectangle")
        ttfdef <- tkframe(ttlfRect)
          butdef <- tkbutton(ttfdef, text="Click rectangle", command=this$doRect)
          # checkbox manual rectangle
          cbtmanrect <- tkcheckbutton(ttfdef, variable=this$cbtmanRect, text="Manual rectangle")
          tkgrid(butdef, cbtmanrect, padx=10, pady=1)
          tk2tip(butdef, "Define rectangle by mouse clicks")
          tk2tip(cbtmanrect, "Define rectangle with coordinates below")
        tkgrid(ttfdef)

        ttfcoords <- tkframe(ttlfRect)
          # choose coords from x axis
          ttfcoords.x <- tkframe(ttfcoords)
            x1 <- tkentry(ttfcoords.x, width=4, textvariable=this$vx1)
            x2 <- tkentry(ttfcoords.x, width=4, textvariable=this$vx2)
            tkgrid(tklabel(ttfcoords.x, text="x1:"), x1, padx=6, sticky="e", pady=1)
            tkgrid(tklabel(ttfcoords.x, text="x2:"), x2, padx=6, sticky="e", pady=1)
          # choose coords from y ayis
          ttfcoords.y <- tkframe(ttfcoords)
            y1 <- tkentry(ttfcoords.y, width=4, textvariable=this$vy1)
            y2 <- tkentry(ttfcoords.y, width=4, textvariable=this$vy2)
            tkgrid(tklabel(ttfcoords.y, text="y1:"), y1, padx=8, sticky="e", pady=1)
            tkgrid(tklabel(ttfcoords.y, text="y2:"), y2, padx=8, sticky="e", pady=1)
          tkgrid(ttfcoords.x, ttfcoords.y)
        tkgrid(ttfcoords)
        #
        ttfcheckboxes <- tkframe(ttlfRect)
          # checkbox tmp.data
          cbttmpdata <- tkcheckbutton(ttfcheckboxes, variable=this$cbtgateData, text="Gate data")
          #ä#tkgrid(, padx=5, pady=1, sticky="n")
          tk2tip(cbttmpdata, "Save gated data temporarily")
          # call button "Rect" and "Redo Rect"
          ttfaddInfo <- tkframe(ttfcheckboxes)
            # checkbox auto rectangle
            cbtautorect <- tkcheckbutton(ttfaddInfo, variable=this$cbtautoRect, text="Auto add info")
            tk2tip(cbtautorect, "Add rect and cell info automatically")
            tkgrid(cbtautorect, padx=5, sticky="n")
            tkgrid(tkbutton(ttfaddInfo, text = "Add rect info", command = function() {
                this$addRectInfo(setcex = 1.0)
                }), pady=1)
            tkgrid(tkbutton(ttfaddInfo, text = "Add cell info", command = function() {
                this$addCellInfo(setcex = 1.0)
                }))
          tkgrid(cbttmpdata, ttfaddInfo, padx=5)
        tkgrid(ttfcheckboxes, padx=5, pady=1, sticky="nsew")
        ##
        btplotrect <- tkbutton(ttlfRect, text="Plot rectangle data only", command=this$dotriploTRectData)
        tkgrid(btplotrect, sticky="we", pady=5, padx=5)
        tkconfigure(btplotrect, height = 2)
        
    tkgrid(ttfgraphcomment, ttlfRect, padx=5, sticky="we")
    ## 
  tkgrid(ttfgraph, padx=5)
  ##### set rect frame ttfgraph with Graphics and rectangle end
  
  # frame: Data Info
  lfinfo <- ttklabelframe(tab, text="Data info")
    this$ncell.gui <- tklabel(lfinfo, text=as.character(this$origin.ncells))
    this$ncell.sel.gui <- tklabel(lfinfo, text=as.character(this$ncell.sel))
    this$ncell.perc.gui <- tklabel(lfinfo, text=as.character(this$ncell.perc))
    tkgrid(tklabel(lfinfo, text="# of total cells:"), this$ncell.gui, sticky="w", padx=5)
    tkgrid(tklabel(lfinfo, text="# of selected cells:"), this$ncell.sel.gui, tklabel(lfinfo, text="("), this$ncell.perc.gui, tklabel(lfinfo, text="%)"), sticky="w", padx=5)
    # call frame Data Info
  tkgrid(lfinfo, columnspan=2, padx=5, sticky="we")
  tkgrid.configure(lfinfo, sticky="ew")  
}

### diploT tab
# tab     tab frame, no default
fcs$GUIdiploT <- function(tab) {
  this <- fcs
  
  if (this$working) printf("w: GUIdiploT")
  
  var.length <- 38
  if (this$max.nchar.vars > var.length) var.length <- this$max.nchar.vars
  
  # set frame and content of combobox with file, mincount, binSize ---------------------
  ttftopframe <- tkframe(tab)
    ttfcombo <- tkframe(ttftopframe)
    # combobox: binSize
    this$binSizedi <- ttkcombobox(ttfcombo, values=this$binSizes, width=var.length)
    tkset(this$binSizedi, this$binSizes[param$binSizesDiPos])
    # combobox: mincount cells
    this$minCountDi <- ttkcombobox(ttfcombo, values=this$min.counts.diploT, width=var.length)
    tkset(this$minCountDi, this$min.counts.diploT[param$minCountDiPos])
    # combobox: param
    this$cbvar1di <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
    tkset(this$cbvar1di, this$selected.vars[param$currVarDi1])
    this$cbvar2di <- ttkcombobox(ttfcombo, values=this$selected.vars, width=var.length)
    tkset(this$cbvar2di, this$selected.vars[param$currVarDi2])
    # call comboboxleft 
    tkgrid(tklabel(ttfcombo, text="binSize:"), this$binSizedi, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="minCount:"), this$minCountDi, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature A:"), this$cbvar1di, sticky="e", padx=5)
    tkgrid(tklabel(ttfcombo, text="Feature B:"), this$cbvar2di, sticky="e", padx=5)
  
    # plot button ---------------------------------------------------
    ttfbut <- tkframe(ttftopframe)
      btplot <- tkbutton(ttfbut, text = "Plot diploT", command = function() {
        this$dodiploT(mode = "single")
      })
      tkgrid(btplot, sticky="snwe", padx=10, pady=1)
      tkconfigure(btplot, height=2, width=25)
      bthist <- tkbutton(ttfbut, text = "Plot histograms", command = function() {
        this$plotHistograms(plotter = "di", pdf = FALSE)
      })
      tkgrid(bthist, sticky="we", padx=10, pady=1)
    tkgrid(ttfcombo, ttfbut, padx=5, pady=1, sticky="w")
  tkgrid(ttftopframe)
  
  ##### set rect frame ttfvals with additional options min/max FI values  ----------
  ttfvals <- tkframe(tab)
    # set label frame min/max FI in rect frame "ttfvals"
    ttlfFI <- ttklabelframe(ttfvals, text="Plot options")
      ttfrange <- tkframe(ttlfFI)
        # dynamic range -------------------------------------------------
        ttfdyn <- tkframe(ttfrange)
          ttfdynL <- tkframe(ttfdyn)
            tkgrid(tklabel(ttfdynL, text="Range x-axis (min/max):", pady=1, padx=15))
            cbtdynrange <- tkcheckbutton(ttfdynL, variable=this$cbtdynRange, text="Dynamic MSI range")
            tkgrid(cbtdynrange, sticky="nws", pady=10, padx=15)
            tk2tip(cbtdynrange, "Uncheck for manual input")
          # second param--------------------------------------------------
          ttfdynR <- tkframe(ttfdyn)
            minvalXdi <- tkentry(ttfdynR, width=6, textvariable=this$vminvalX)
            maxvalXdi <- tkentry(ttfdynR, width=6, textvariable=this$vmaxvalX)
            tkgrid(minvalXdi, maxvalXdi, padx=5, pady=1, sticky="w")
            minMSI <- tkentry(ttfdynR, width=4, textvariable=this$vminMSI)
            maxMSI <- tkentry(ttfdynR, width=4, textvariable=this$vmaxMSI)
            tkgrid(tklabel(ttfdynR, text="min(MSI):"), minMSI, padx=5, pady=1, sticky="e")
            tkgrid(tklabel(ttfdynR, text="max(MSI):"), maxMSI, padx=5, pady=1, sticky="e")
          tkgrid(ttfdynL, ttfdynR)
        tkgrid(ttfdyn)
      tkgrid(ttfrange)
      
      # add checkbuttons for plot attributes ------------------------------
      ttfplot <- tkframe(ttlfFI)
        ttfgridnew <- tkframe(ttfplot)
          cbtgrid <- tkcheckbutton(ttfgridnew, variable=this$cbtshowGrid, text="Grid")
          tkgrid(cbtgrid, sticky="w")
          tk2tip(cbtgrid, "Check to add grid.")
          cbtaddfilename <- tkcheckbutton(ttfgridnew, variable=this$cbtaddFilename, text="File name")
          tkgrid(cbtaddfilename, sticky="w")
          tk2tip(cbtaddfilename, "Check to add file name at every diploT.")
          cbtaddDate <- tkcheckbutton(ttfgridnew, variable=this$cbtaddDate, text="Date")
          tkgrid(cbtaddDate, sticky="w")
          cbtdisplayA <- tkcheckbutton(ttfgridnew, variable=this$cbtdisplayA, text="Hide A-")
            tk2tip(cbtdisplayA, "Check if no distraction of (Feature A-) is desired.")
            tkgrid(cbtdisplayA, sticky="w")
        ttfsave <- tkframe(ttfplot)
          btnewplot <- tkbutton(ttfsave, text = "New diploT window", command = function() {
            this$newdiploT(26)
          })
          tkgrid(btnewplot, padx=5)
          btsavePDF <- tkbutton(ttfsave, text = "Save active window", command = function() {
            this$saveWindow()
          })
          tkgrid(btsavePDF, padx=5, pady=1)
          tk2tip(btsavePDF, "Save as pdf")
        tkgrid(ttfgridnew, ttfsave, padx=10)
      tkgrid(ttfplot)
        
    # option panel ---------------------------------------------------
    ttlfinfo <- ttklabelframe(ttfvals, text="Calculation options")
      # add radio buttons for transformation type
      ttfradios <- tkframe(ttlfinfo)
        ttfradiosright1 <- tkframe(ttfradios)
          rb1 <- tkradiobutton(ttfradiosright1, variable=this$rbtrans, value="asinh")
          rb2 <- tkradiobutton(ttfradiosright1, variable=this$rbtrans, value="none")
          tkgrid(rb1, tklabel(ttfradiosright1, text = "asinh"), rb2, tklabel(ttfradiosright1, text = "none"))
          tkbind(rb1, "<Button-1>", function() {
          this$changeFI.range(1)
      })
      tkgrid(tklabel(ttfradios, text="Transformation:"), ttfradiosright1, sticky="w", pady=1)
      
      # add radio buttons for calculation type -----------------------------
      ttfradiosright2 <- tkframe(ttfradios)
        rb1 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="MSI")
        rbl1 <- tklabel(ttfradiosright2, text="MSI")
        rb2 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="freq")
        rbl2 <- tklabel(ttfradiosright2, text="frequency")
        rb3 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="density")
        rbl3 <- tklabel(ttfradiosright2, text="density")
        rb4 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="SD")
        rbl4 <- tklabel(ttfradiosright2, text="SD")
        rb5 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="SEM")
        rbl5 <- tklabel(ttfradiosright2, text="SEM")
        rb6 <- tkradiobutton(ttfradiosright2, variable=this$rbcalc, value="RSEM")
        rbl6 <- tklabel(ttfradiosright2, text="RSEM")
        tkgrid(rb1, rbl1, rb2, rbl2, sticky="w")
        tk2tip(rb1, "Mean fluorescence intensity: If cutoff is set, it only considers producing cells of feature Y")
        tk2tip(rbl1, "Mean fluorescence intensity: If cutoff is set, it only considers producing cells of feature Y")
        tk2tip(rb2, "Frequency of producing cells")
        tk2tip(rbl2, "Frequency of producing cells")
        tkgrid(rb3, rbl3, rb4, rbl4, sticky="w")
        tk2tip(rb3, "Cell count")
        tk2tip(rbl3, "Cell count")
        tk2tip(rb4, "Standard deviation")
        tk2tip(rbl4, "Standard deviation")
        tkgrid(rb5, rbl5, rb6, rbl6, sticky="w")
        tk2tip(rb5, "Standard error of mean")
        tk2tip(rbl5, "Standard error of mean")
        tk2tip(rb6, "Relative standard error of mean")
        tk2tip(rbl6, "Relative standard error of mean")
      tkgrid(tklabel(ttfradios, text="Statistical method:"), ttfradiosright2, sticky="nw", pady=1)
      tkgrid(ttfradios, padx=10, pady=1)
  
      # frame for additional options ---------------------------------------
      ttfbuttons <- tkframe(ttlfinfo)
        btremovdub <- tkbutton(ttfbuttons, text = "Remove Doublets", command = function() {
          this$preprocData()
        })
        ttfcheckbuttons <- tkframe(ttfbuttons)
          cbttrimming <- tkcheckbutton(ttfcheckbuttons, variable=this$cbttrimming, text="Trim first")
          tk2tip(cbttrimming, sprintf("Trims %s of each column.", this$trim.num))
          cbtshowMinBins <- tkcheckbutton(ttfcheckbuttons, variable=this$cbtshowMinBins, text="Bins <minCount")
          tk2tip(cbtshowMinBins, "Show bins in pale color.")
          tkgrid(cbttrimming, sticky="w", pady=1)
          tkgrid(cbtshowMinBins, sticky="w")
        tkgrid(btremovdub, ttfcheckbuttons, padx=5, pady=1, sticky="we")
      tkgrid(ttfbuttons, sticky="w")
    tkgrid(ttlfFI, ttlfinfo, padx = 5, pady = 1, sticky = "wens")
  tkgrid(ttfvals, pady=1)
  
  # plot button "overview" ------------------------
  ttfplotten <- tkframe(tab)
    btplot1 <- tkbutton(ttfplotten, text = "Plot diploT-Overview with fixed Feature A", command = function() {
        this$dodiploT(mode = "overview")
      })
    tk2tip(btplot1, "max. 35 diploTs")
    btplot2 <- tkbutton(ttfplotten, text = "Plot diGraph-Overview with fixed Feature A", command = function() {
        this$dodiGraph(mode = "overview")
      })
    tk2tip(btplot2, "max. 10 diGraphs")
    tkgrid(btplot1, btplot2, sticky="snwe", padx=15, pady=5)
    tkconfigure(btplot1, height=2)
    btplot3 <- tkbutton(ttfplotten, text = "Plot diploT-Overview total", command = function() {
        this$dodiploTtotal()
      })
    btplot4 <- tkbutton(ttfplotten, text = "Plot diGraph-Overview0 with fixed Feature A", command = function() {
        this$dodiGraph(mode = "overview", set.zero = TRUE)
      })
    tk2tip(btplot4, "max. 10 diGraphs")
    tkgrid(btplot3, btplot4, sticky="snwe", padx=15, pady=5)
    tkconfigure(btplot3, height=2)
  tkgrid(ttfplotten)
  
  # frame: Data Info ---------------------------
  lfinfo <- ttklabelframe(tab, text="Data info")
    this$ncell.gui.di <- tklabel(lfinfo, text=as.character(this$origin.ncells))
    this$ncell.sel.gui.di <- tklabel(lfinfo, text=as.character(this$ncell.sel))
    this$ncell.perc.gui.di <- tklabel(lfinfo, text=as.character(this$ncell.perc))
    tkgrid(tklabel(lfinfo, text="# of total cells:"), this$ncell.gui.di, sticky="w", padx=5)
    tkgrid(tklabel(lfinfo, text="# of selected cells:"), this$ncell.sel.gui.di, tklabel(lfinfo, text="("), this$ncell.perc.gui.di, tklabel(lfinfo, text="%)"), sticky="w", padx=5)
    
  tkgrid(lfinfo, sticky="s", columnspan=2, padx=5)
  tkgrid.configure(lfinfo, sticky="swe", pady=5, padx=5)  
}

### Dataframe tab
# tab     tab frame, no default
fcs$GUIdataframe <- function(tab) {
  this <- fcs
  
  # create paneled windows
  # resize the window and move the panel separator with the mouse
  this$pw2 <- tk2panedwindow(tab, orient="horizontal")
  
  ##### add tree widget
  # create scrolling function
  xScr <- tkscrollbar(this$pw2, command=function(...)
    tkxview(this$treeDF, ...), orient="horizontal")
  yScr <- tkscrollbar(this$pw2, command=function(...)
    tkyview(this$treeDF, ...), orient="vertical")
  
  # create tree grid
  this$treeDF <- tkwidget(this$pw2, "Tree", 
                          xscrollcommand=function(...) tkset(xScr, ...), 
                          yscrollcommand=function(...) tkset(yScr, ...), 
                          width=15, height =20, 
                          opencmd = function(...) {
                              tkopentree(this$treeDF, ...)
                            }, 
                          closecmd = function(...) {
                              tkclosetree(this$treeDF, ...)
                            }
  )
  
  # call treewidget
  tkgrid(this$treeDF, yScr)
  tkgrid.configure(this$treeDF, stick="nsew")
  tkgrid.configure(yScr, stick="nsw")
  tkgrid(xScr)
  
  # insert tree content
  for (i in 1:length(this$parent.env)) {
    children <- ls(eval(parse(text=paste(this$parent.env[i]))))
    
    for (j in 1:length(children)) {
      if ((class(eval(parse(text=paste("this$", children[j], sep=""))))[1] == "data.frame") & (grepl("dataframes", children[j]))) {
        # if class of children is a data.frame 
        # AND
        # if data.frame named dataframes
        df.num <- as.numeric(strsplit(children[j], "[^0-9]+")[[1]][2])
        tkinsert(this$treeDF, "end", "root", children[j], text=this$dataframes.name[df.num], image=imageMatrix)
      }
    }
  }
  
  # add tree widget to panel
  tkadd(this$pw2, this$treeDF)
  
  # bind mouse click with function
  tkbindtext(this$treeDF, "<Button-1>", function(...) {
      this$clickinsertDF(this$treeDF, ...)
  })
  ##### tree widget stop
  
  ######## add table widget to the right
  this$treetable <- ttktreeview(tab, columns=5, show="headings")
  
  # create scroll bar
  yscr <- tkscrollbar(this$treetable, command=function(...) tkyview(this$treetable, ...))
  tkconfigure(this$treetable, yscrollcommand=function(...) tkset(yscr, ...))
  tkpack(yscr, side="right", fill="both")
  
  xscr <- tkscrollbar(this$treetable, command=function(...) tkxview(this$treetable, ...), orient="horizontal")
  tkconfigure(this$treetable, xscrollcommand=function(...) tkset(xscr, ...))
  tkpack(xscr, side="bottom", fill="both")
  
  tkadd(this$pw2, this$treetable)
  ##### tree table stop
  
  # show text paneled window
  tkpack(this$pw2, fill="both", expand=TRUE)
  
  # add buttons
  lfbut <- ttkframe(tab)
    btn1 <- ttkbutton(lfbut, text = "Save Rect_Data_Raw", command = function() {
        this$saveDF(1)
    }, width=25)
    tk2tip(btn1, "Save as csv table.")
    btn2 <- ttkbutton(lfbut, text = "Save Rect_Data_Transformed", command = function() {
        this$saveDF(2)
    }, width=25)
    tk2tip(btn2, "Save as csv table.")
    tkpack(btn1, btn2, padx=5, pady=1, side="left")
  tkpack(lfbut, side="top", fill="y", expand=FALSE)
}

### Update log tab
# tab     tab frame, no default
fcs$GUIlog <- function(tab) {
  this <- fcs
  
  # set frame and content of combobox with file, mincount, binSize
  ttfmain <- tkframe(tab)
  
  text <- tk2text(ttfmain, width=88, height=35)
  log <- readLines("PRIanalyzer.log")
  for (i in log) {
    tkinsert(text, "end", i)
    tkinsert(text, "end", "\n")
  }
  tkgrid(text)
  tkconfigure(text, state="disabled")
  tkgrid(ttfmain)
}

### Preprocession pop-up window size
# mode    chose the mode, 1 for automatic gating window, 2 for normalization window
fcs$GUIpreproc <- function(mode) {
  this <- fcs
  
  if (exists("ttpreproc", where = eval(parse(text = "fcs")))) {
      tkdestroy(this$ttpreproc)
  }
  
  this$ttpreproc <- tktoplevel()
  
  ### mode 1: automatic gating
  ### mode 2: normalization
  if (mode == 1) {
    mode.title <- "Calculating cutoffs"
    mode.size <- "1200x800"
  } else if (mode == 2) {
    mode.title <- "Normalization"
    mode.size <- "1100x600"
  }
  
  # set window title
  tkwm.title(this$ttpreproc, sprintf("Data preprocession - %s", mode.title))
  #set window size
  tkwm.geometry(this$ttpreproc, mode.size)
  
  # create topMenu
  topMenu <- tkmenu(this$ttpreproc)
  tkconfigure(this$ttpreproc, menu=topMenu)
  fileMenu <- tkmenu(topMenu, tearoff=FALSE)
  tkadd(topMenu, "command", label = "Close", command = function() {
      this$closePreproc(mode)
  })
  
  ### insert preprocession mode
  this$GUIpreprocMode(mode)
}

### Preprocession pop-up window content
# mode    chose the mode, 1 for automatic gating window, 2 for normalization window
fcs$GUIpreprocMode <- function(mode) {
  this <- fcs
  printf("w: GUI PreProcTab :: mode %s", mode)
  
  ### mode 1: automatic gating
  ### mode 2: normalization
  if (mode == 1) {
    # create info panel
    this$ttlfautogate <- tkframe(this$ttpreproc)
    
    ### combobox choose file
    ttfchoosefile <- tkframe(this$ttlfautogate)
    this$current.filePrep <- ttkcombobox(ttfchoosefile, values=this$current.filenames, width=this$max.nchar)
    tkset(this$current.filePrep, this$current.filenames[as.numeric(this$current.filenum)])
    tkbind(this$current.filePrep, "<<ComboboxSelected>>", function() {
        this$refreshComboboxVars(tclvalue(tkget(this$current.filePrep)))
    })
    tkgrid(tklabel(ttfchoosefile, text="File:  "), this$current.filePrep, sticky="w")
    tkgrid(ttfchoosefile, pady=10)
    
    ### add checkbuttons for data preparation
    ttfcheckboxes <- ttklabelframe(this$ttlfautogate, text="Calculation options")
    this$cbtremovedub <- tclVar("0")
    cbtremovedub <- tkcheckbutton(ttfcheckboxes, variable=this$cbtremovedub, text="Remove doublets")
    tk2tip(cbtremovedub, "Check for removing doublets.")
    this$cbttrimming <- tclVar("0")
    cbttrimming <- tkcheckbutton(ttfcheckboxes, variable=this$cbttrimming, text="Trimming")
    tk2tip(cbttrimming, "Check for trimming 0.02% of each marker.")
    tkgrid(cbttrimming, cbtremovedub, sticky="w", pady=1, padx=10)
    tkgrid(ttfcheckboxes, pady=10)
    
    ### add plot/start buttons
    ttfbuttons <- tkframe(this$ttlfautogate)
      bthist <- tkbutton(ttfbuttons, width = 25, height = 3, text = "Plot histograms", command = function() {
          this$plotHistograms(plotter = "preproc", pdf = FALSE)
      })
      btautogate <- tkbutton(ttfbuttons, width = 25, height = 3, text = "Calculate cutoffs", command = function() {
          this$autoSetCutoff()
      })
      btinteractgate <- tkbutton(ttfbuttons, width = 25, height = 3, text = "Set cutoffs interactively", command = function() {
          this$interactGate()
      })
    tkgrid(bthist, btautogate, btinteractgate, padx=20, pady=10)
    ### add buttons to select all markers or none
    len <- length(this$selected.vars)
    btselAll <- tkbutton(ttfbuttons, text="Select all", command=function() {
      len <- length(this$selected.vars)
      for (i in 1:len) {
        tclvalue(this$cbVal[[i]]) <- "1"
      }
    })
    btdeselAll <- tkbutton(ttfbuttons, text="Deselect all", command=function() {
      len <- length(this$selected.vars)
      for (i in 1:len) {
        tclvalue(this$cbVal[[i]]) <- "0"
      }
    })
    tkgrid(btselAll, btdeselAll)
    tkgrid(ttfbuttons)
    
    ### add markers
    this$ttlfselMarker <- ttklabelframe(this$ttlfautogate, text="Select marker:")
    ttfcol1 <- tkframe(this$ttlfselMarker)
    ttfcol2 <- tkframe(this$ttlfselMarker)
    ttfcol3 <- tkframe(this$ttlfselMarker)
    ttfcol4 <- tkframe(this$ttlfselMarker)
    residue <- 0
    if (len %% 4 > 0) residue <- 1
    quarter <- floor(1 * len / 4) +  residue
    for (i in 1:quarter) {
      cb <- tkcheckbutton(ttfcol1, variable=this$cbVal[[i]], text=this$selected.vars[i])
      cutoffentry <- tkentry(ttfcol1, width=6, textvariable=this$vcutoffs[[i]])
      tkgrid(cb, tklabel(ttfcol1, text="cutoff: "), cutoffentry, padx=2, pady=1, sticky="w")
    }
    for (j in (i + 1):(i + quarter)) {
      cb <- tkcheckbutton(ttfcol2, variable=this$cbVal[[j]], text=this$selected.vars[j])
      cutoffentry <- tkentry(ttfcol2, width=6, textvariable=this$vcutoffs[[j]])
      tkgrid(cb, tklabel(ttfcol2, text="cutoff: "), cutoffentry, padx=2, pady=1, sticky="w")
    }
    for (k in (j + 1):(j + quarter)) {
      cb <- tkcheckbutton(ttfcol3, variable=this$cbVal[[k]], text=this$selected.vars[k])
      cutoffentry <- tkentry(ttfcol3, width=6, textvariable=this$vcutoffs[[k]])
      tkgrid(cb, tklabel(ttfcol3, text="cutoff: "), cutoffentry, padx=2, pady=1, sticky="w")
    }
    
    for (l in (k + 1):len) {
      cb <- tkcheckbutton(ttfcol4, variable=this$cbVal[[l]], text=this$selected.vars[l])
      cutoffentry <- tkentry(ttfcol4, width=6, textvariable=this$vcutoffs[[l]])
      tkgrid(cb, tklabel(ttfcol4, text="cutoff: "), cutoffentry, padx=2, pady=1, sticky="w")
    }
    tkgrid(ttfcol1, ttfcol2, ttfcol3, ttfcol4, padx=20, pady=1, sticky="we")
    tkgrid(this$ttlfselMarker, padx=10)
    
    tkgrid(this$ttlfautogate, pady=1)
  } else if (mode == 2) {
    # create info panel
    this$ttlfnormalize <- tkframe(this$ttpreproc)
    
    ### add buttons select/deselect all
    ttfbuttons <- tkframe(this$ttlfnormalize)
    # add start normalization button
    btnormalize <- tkbutton(ttfbuttons, height = 3, text = "Start normalization", command = function() {
        this$normalizeFiles()
    })
    tkgrid(btnormalize, pady=10, sticky="snew")
    
    len <- length(this$current.filenames)
    for (i in 1:len) {
      this$cbValFile[[i]] <- tclVar("0")
    }
    btselAll <- tkbutton(ttfbuttons, text="Select all", command=function() {
      for (i in 1:len) {
        tclvalue(this$cbValprepFile[[i]]) <- "1"
      }
    })
    btdeselAll <- tkbutton(ttfbuttons, text="Deselect all", command=function() {
      for (i in 1:len) {
        tclvalue(this$cbValprepFile[[i]]) <- "0"
      }
    })
    tkgrid(btselAll, btdeselAll, padx=20, pady=1)
    tkgrid(ttfbuttons)
    
    ### add files to select
    ttfselFile <- ttklabelframe(this$ttlfnormalize, text="Select files:")
    ttfcol1 <- tkframe(ttfselFile)
    ttfcol2 <- tkframe(ttfselFile)
    ttfcol3 <- tkframe(ttfselFile)
    for (i in 1:(floor(len / 3) + (len %% 3))) {
      cb <- tkcheckbutton(ttfcol1, variable=this$cbValprepFile[[i]], text=this$current.filenames[i])
      tkgrid(cb, padx=2, pady=1, sticky="w")
    }
    for (j in (i + 1):(i + floor(len / 3))) {
      cb <- tkcheckbutton(ttfcol2, variable=this$cbValprepFile[[j]], text=this$current.filenames[j])
      tkgrid(cb, padx=2, pady=1, sticky="w")
    }
    for (k in (j + 1):len) {
      cb <- tkcheckbutton(ttfcol3, variable=this$cbValprepFile[[k]], text=this$current.filenames[k])
      tkgrid(cb, padx=2, pady=1, sticky="w")
    }
    tkgrid(ttfcol1, ttfcol2, ttfcol3, padx=20, pady=1, sticky="wen")
    tkgrid(ttfselFile, pady=10, padx=5)
    
    tkgrid(this$ttlfnormalize)
  }   
}

