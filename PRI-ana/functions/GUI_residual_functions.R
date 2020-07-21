#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020


### GUI refreshing functions ---------------------------

### Refreshes the GUI from the marker frame to the left
# It is connected to button "GUI" on top
fcs$refreshPanel <- function() {
  this <- fcs
  printf("w: do refreshPanel")
  
  len <- length(this$selected.cutoffs)
  
  #### refresh checkbox markers 
  scrollFrame <- tkwidget(this$sw, "ScrollableFrame", height=630, width=this$width.panel)
  tcl(this$sw, "setwidget", scrollFrame)
  subfID <- tclvalue(tcl(scrollFrame, "getframe"))
  # label project name
  lab <- tcl("label", paste(subfID, ".lab", sep=""), text=sprintf("%s", this$shortenFilename(this$current.project)))
  tkgrid(lab, sticky="w", padx=5)
  
  i <- j <- 1
  while (i < (len * 4)) {
    cutoffcb <- tcl("checkbutton", paste(subfID, ".", i, sep=""), variable=this$cbVal[[j]], text=this$selected.vars[j])
    cutofflabel <- tcl("label", paste(subfID, ".", i + 1, sep=""), text="cutoff: ")
    cutoffentry <- tcl("entry", paste(subfID, ".", i + 2, sep=""), width=6, textvariable <- this$vcutoffs[[j]])
    cutoffperccb <- tcl("checkbutton", paste(subfID, ".", i + 3, sep=""), variable=this$cbcutoffperc[[j]], text="in %")
    tkgrid(cutoffcb, cutofflabel, cutoffentry, cutoffperccb, sticky="w", pady=1)
    i <- i + 4
    j <- j + 1
  }
}

### GUI pop-up window to select a project
fcs$openProject <- function() {
  this <- fcs
  
  printf("w: do openProject")
  
  this$ttprojects <- tktoplevel()
  this$ttprojectsopen <- TRUE
  
  # set window title
  tkwm.title(this$ttprojects, "Project List")
  #set window size
  tkwm.geometry(this$ttprojects, "400x600")
  
  ##### add table widget
  listProjects <- ttktreeview(this$ttprojects, columns=1, show="headings")
  
  # create scroll bar
  yscr <- tkscrollbar(listProjects, command=function(...) tkyview(listProjects, ...))
  tkconfigure(listProjects, yscrollcommand=function(...) tkset(yscr, ...))
  tkpack(yscr, side="right", fill="both")
  xscr <- tkscrollbar(listProjects, command=function(...) tkxview(listProjects, ...), orient = "horizontal")
  tkconfigure(listProjects, xscrollcommand=function(...) tkset(xscr, ...))
  tkpack(xscr, side="bottom", fill="both")
  
  shade <- c("none", "gray")
  idx.file <- grep("fileIdentity|fileIndex", this$dataframes.name)
  tcl(listProjects, "column", 1, width=this$max.nchar)
  for (i in 1:length(this$total.projects)){
    size <- nrow(this$getDFtable(this$dataframes.name[idx.file[i]]))
    tkinsert(listProjects, "", "end", values=as.tclObj(paste(this$total.projects[i], "(", size, ")", sep="")), tag=shade[i %% 2 + 1])
  }
  #tcl(this$treetable, "configure", background="gray95")
  tktag.configure(this$treetable, "gray", background="gray95")
  tkpack(listProjects, fill="both", expand=TRUE)
  tk2tip(listProjects, "Double Click to Open")
  
  # get row index
  #this$projectChildren=tcl(listProjects, "children", "")
  #print(sapply(as.character(this$projectChildren), function(i) tclvalue(tkindex(listProjects, i))))
  
  # double click on project    
  tkbind(listProjects, "<Double-Button-1>", function(W, x, y) {
    sel <-  as.character(tcl(W, "identify", "row", x, y))
    this$selected.project <- strsplit(tclvalue(tcl(W, "item", sel, "-values")), "\\(")[[1]][1]
    project.num <- which(this$total.projects == this$selected.project)
    #if (project.num != this$current.projectnum) {
    #print(paste("Project selected:", this$selected.project))
    this$refreshComboboxStart(project.num)
    #}
    tkwm.withdraw(tkwinfo("toplevel", W))
  })
}

### GUI pop-up window to select a database
fcs$openDB <- function() {
  this <- fcs
  
  printf("w: do openDB")
  file <- tclvalue(tkgetOpenFile(initialdir=fcs$db.path, defaultextension="sqlite3"))
  this$exit()
  this$connectDb(file)
  this$db.name <- file
  this$GUImain()
}

### Refreshes the marker frame and combobox frame and the markers, files and saved cutoffs
# num     project ID
fcs$refreshComboboxStart <- function(num) {
  # refresh File Combobox
  this <- fcs
  
  printf("w: do refreshComboboxStart with project #%s", num)
  
  this$selected.projectnum <- num
  this$selected.project <- this$total.projects[num]
  
  ### look if current cutoffs and checks are also the saved ones
  # cutoffs
  new <- unlist(this$new.cutoffs)
  new <- new[!is.na(new)]
  saved <- unlist(this$saved.cutoffs)
  saved <- saved[!is.na(saved)]
  both <- new == saved
  # checks
  new2 <- unlist(this$new.checks)
  new2 <- new2[!is.na(new2)]
  saved2 <- unlist(this$saved.checks)
  saved2 <- saved2[!is.na(saved2)]
  both2 <- new2 == saved2
  
  ### ask to save cutoffs and checks
  if (!all(both) & !all(both2)) {
    answer <- tkmessageBox(title = "Cutoffs..", message = "Save cutoffs first?", icon = "info", type = "yesno")
    if (tclvalue(answer) == "yes") this$saveCutoffsToDB()
  }
  
  # get filenames for table
  this$current.filetable <- this$getDFtable(paste0(this$selected.project, this$fileid_name))
  this$current.filenames <- this$current.filetable[, 2]
  
  # select first file
  this$selected.filenum <- this$current.filetable[which(this$current.filetable[, 2] == this$current.filenames[1]), 1]
  # and set in the GUI
  tkconfigure(this$tkchoosefile, values=this$current.filenames)
  tkset(this$tkchoosefile, this$current.filenames[1])
  ### refresh filenames in title
  displayfile <- this$shortenFilename(this$current.filenames[1], title=TRUE)
  tkdelete(this$title, "1.0", "end")
  tkinsert(this$title, "1.0", displayfile)
  
  # get stain table with marker columns and checks
  this$current.staintable <- this$getDFtable(paste0(this$selected.project, this$markerid_name))
  
  ### load cutoffs
  if (ncol(this$current.staintable) == 6) {
    ### if cutoffs were already saved
    # get all cutoffs
    for (file.id in unique(this$current.filetable[, 1])) {
      this$new.cutoffs[[file.id]] <- this$current.staintable[which(this$current.staintable[, 1] == file.id), 5]
      this$new.checks[[file.id]] <- this$current.staintable[which(this$current.staintable[, 1] == file.id), 6]
    }
    this$saved.cutoffs <- this$new.cutoffs
    this$saved.checks <- this$new.checks
  } else if (ncol(this$current.staintable) == 5) {
    ### if there is no check column
    # get all cutoffs
    for (file.id in unique(this$current.filetable[, 1])) {
      this$new.cutoffs[[file.id]] <- this$current.staintable[which(this$current.staintable[, 1] == file.id), 5]
      this$new.checks[[file.id]] <- rep("0", nrow(this$current.staintable[which(this$current.staintable[, 1] == file.id), ]))
    }
    this$saved.cutoffs <- this$new.cutoffs
    this$saved.checks <- list()
  } else {
    print("w: Initiate new cutoff list")
    ### if no cutoffs listed in this project
    # initiate new cutoff list
    for (file.id in unique(this$current.filetable[, 1])) {
      this$new.cutoffs[[file.id]] <- rep("0", nrow(this$current.staintable[which(this$current.staintable[, 1] == file.id), ]))
      this$new.checks[[file.id]] <- rep("0", nrow(this$current.staintable[which(this$current.staintable[, 1] == file.id), ]))
    }
    this$saved.cutoffs <- list()
    this$saved.checks <- list()
  }
  
  ### set variables from project in tab di tri and tetraplot
  this$selected.vars <- this$getVariables(table=this$selected.projectnum, index=this$selected.filenum)
  
  ### if FSC/SSC are listed
  # set vars after the FSC/SSCs (because less used)
  tmp <- max(grep("SC", this$selected.vars)) 
  tmp2 <- min(grep(c("Time"), this$selected.vars))
  if (tmp2 > tmp & !is.infinite(tmp2) & tmp2 < (length(this$selected.vars) - 3)) tmp <- tmp2
  if (((tmp + 3) > length(this$selected.vars)) | is.infinite(tmp)) tmp <- 0
  
  tkconfigure(this$cbvar1, values=this$selected.vars)
  if (length(param$currVarTri1) == 0) param$currVarTri1 <- 1 + tmp
  else if (param$currVarTri1 < tmp) param$currVarTri1 <- 1 + tmp
  tkset(this$cbvar1, this$selected.vars[param$currVarTri1])
  
  tkconfigure(this$cbvar2, values=this$selected.vars)
  if (length(param$currVarTri2) == 0) param$currVarTri2 <- 2 + tmp
  else if (param$currVarTri2 < tmp) param$currVarTri2 <- 2 + tmp
  tkset(this$cbvar2, this$selected.vars[param$currVarTri2])
  
  tkconfigure(this$cbvar3, values=this$selected.vars)
  if (length(param$currVarTri3) == 0) param$currVarTri3 <- 3 + tmp
  else if (param$currVarTri3 < tmp) param$currVarTri3 <- 3 + tmp
  tkset(this$cbvar3, this$selected.vars[param$currVarTri3])
  
  tkconfigure(this$cbvar1di, values=this$selected.vars)
  if (length(param$currVarDi1) == 0) param$currVarDi1 <- 1 + tmp
  else if (param$currVarDi1 < tmp) param$currVarDi1 <- 1 + tmp
  tkset(this$cbvar1di, this$selected.vars[param$currVarDi1])
  
  tkconfigure(this$cbvar2di, values=this$selected.vars)
  if (length(param$currVarDi2) == 0) param$currVarDi2 <- 2 + tmp
  else if (param$currVarDi2 < tmp) param$currVarDi2 <- 2 + tmp
  tkset(this$cbvar2di, this$selected.vars[param$currVarDi2])
  
  # renew markers and marker position
  this$refreshMarker(openProject=TRUE)
  
  ### project is chosen, now no function if you click on main window
  this$openProjectYet <- TRUE
  tkbind(this$tt, "<Button-1>", function(...) {})
  
  print("w: Done refreshComboboxStart.")
}

### Refreshes the markers in the tabs
# tab     
fcs$refreshMarker <- function(tab=FALSE, openProject=FALSE) {
  # renew checkboxes "markers" in tab "Overview"
  this <- fcs
  
  len <- length(this$selected.vars)
  this$max.nchar.vars <- max(nchar(this$selected.vars))
  
  if (tab) {
    ### go here if you swith between the tabs
    ### tab 0 = diploT
    ### tab 1 = triploT
    ### tab 2 = quadruploT
    
    ### tab numbering starts with "0"
    tab.old <- this$current.tab
    tab.new <- tclvalue(tkindex(this$panelTabs, "current"))
    
    printf("w: do refreshMarker: tab=%s tab.new=%s tab.old=%s", tab, tab.new, tab.old)
    
    ### set x-/y-/z-features the same within the other tabs
    if (tab.old == "0" & exists("cbvar1", env=fcs)) {
      ### tab 0 = diploT
      
      ### set combobox
      var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1di)))
      tkset(this$cbvar1, var1)
      tkset(this$cbvar1quad, var1)
      
      var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2di)))
      tkset(this$cbvar2, var2)
      tkset(this$cbvar2quad, var2)

      ### if z variable is the same as in x and y, then go one var up
      # in other words:
      # if cbvar3 == cbvar1/2, then set cbvar3 randomly but different
      ### triplot ------------------------------------------------------
      idx <- which(tclvalue(tkget(this$cbvar1)) == this$selected.vars)
      idy <- which(tclvalue(tkget(this$cbvar2)) == this$selected.vars)
      idz <- which(tclvalue(tkget(this$cbvar3)) == this$selected.vars)
      
      if (length(which(idz == c(idx, idy))) > 0 | length(idz) == 0) {
        ### if var index z is identical to var index x or y
        # take another random var index for z
        t <- 1:len
        t <- t[-c(idx, idy)]
        random <- sample(t, 1)
        tkset(this$cbvar3, this$selected.vars[random])
        printf("w: changed z-feature from %s to %s", this$selected.vars[idz], this$selected.vars[random])
      }
      ### quadruplot ----------------------------------------------------
      idx <- which(tclvalue(tkget(this$cbvar1quad)) == this$selected.vars)
      idy <- which(tclvalue(tkget(this$cbvar2quad)) == this$selected.vars)
      idz1 <- which(tclvalue(tkget(this$cbvar3quad)) == this$selected.vars)
      idz2 <- which(tclvalue(tkget(this$cbvar4quad)) == this$selected.vars)
      
      if (any(c(idz1, idz2) %in% c(idx, idy))) {
        ### if var index z is identical to var index x or y
        # take another random var index for z
        t <- 1:len
        t <- t[-c(idx, idy)]
        rand <- sample(t, 1)
        tkset(this$cbvar3quad, this$selected.vars[rand])
        printf("w: changed C1-feature from %s to %s", this$selected.vars[idz1], this$selected.vars[rand])
        t <- t[-rand]
        rand2 <- sample(t, 1)
        tkset(this$cbvar4quad, this$selected.vars[rand2])
        printf("w: changed C2-feature from %s to %s", this$selected.vars[idz2], this$selected.vars[rand2])
      }
      
      ### initiate
      this$coords.info <- vector()
    } else if (tab.old == "1") {
      ### tab 1 = triploT
      
      ### set combobox
      var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
      tkset(this$cbvar1di, var1)
      tkset(this$cbvar1quad, var1)
      
      var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
      tkset(this$cbvar2di, var2)
      tkset(this$cbvar2quad, var2)

      var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))
      tkset(this$cbvar3quad, var3)

      ### quadruplot ----------------------------------------------------
      # if cbvar4 == cbvar1/2/3, then set cbvar4 randomly but different
      idx <- which(tclvalue(tkget(this$cbvar1quad)) == this$selected.vars)
      idy <- which(tclvalue(tkget(this$cbvar2quad)) == this$selected.vars)
      idz1 <- which(tclvalue(tkget(this$cbvar3quad)) == this$selected.vars)
      idz2 <- which(tclvalue(tkget(this$cbvar4quad)) == this$selected.vars)
      
      if (idz2 %in% c(idx, idy, idz1)) {
        ### if var index z is identical to var index x or y
        # take another random var index for z
        t <- 1:len
        t <- t[-c(idx, idy, idz1)]
        rand <- sample(t, 1)
        tkset(this$cbvar4quad, this$selected.vars[rand])
        printf("w: changed C2-feature from %s to %s", this$selected.vars[idz2], this$selected.vars[rand])
      }
    } else if (tab.old == "2") {
      ### tab 2 = quadruploT

      ### set combobox
      var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1quad)))
      tkset(this$cbvar1di, var1)
      tkset(this$cbvar1, var1)

      var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2quad)))
      tkset(this$cbvar2di, var2)
      tkset(this$cbvar2, var2)

      var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3quad)))
      tkset(this$cbvar3, var3)
    }
  } else {
    ### initiate
    this$idx <- list()
    
    printf("w: do refreshMarker: len(vars)=%s fileidx=%s tab=%s openProject=%s", len, this$selected.filenum, tab, openProject)
    
    ### get var names before the "." and make them capitals
    vars.old <- this$shortenMarkername(this$current.vars)
    vars.new <- this$shortenMarkername(this$selected.vars)
    
    ### go through all new selected.vars
    # if any of selected.vars is in current.vars
    # save check and cutoff
    # i : new position of var
    # idx[[i]][1] : new position of var
    # idx[[i]][2] : var checked or not ("0"/"1")
    # idx[[i]][3] : cutoff value of var
    for (i in 1:len) {
      this$idx[[i]] <- tmp <- which(vars.new[i] == vars.old)
      if (length(tmp) > 0) {
        this$idx[[i]][2] <- tclvalue(this$cbVal[[tmp[1]]])
        this$idx[[i]][3] <- tclvalue(this$vcutoffs[[tmp[1]]])
      }
    }
    
    # save cutoffs from old file
    if (!openProject) this$refreshCutoffs()
    this$refreshCutoffs(saved=TRUE)
    len <- length(this$selected.vars)
    
    #### refresh checkbox markers version 3
    scrollFrame <- tkwidget(this$sw, "ScrollableFrame", height=630, width=this$width.panel)
    tcl(this$sw, "setwidget", scrollFrame)
    subfID <- tclvalue(tcl(scrollFrame, "getframe"))
    # label project name
    lab <- tcl("label", paste(subfID, ".lab", sep=""), text=sprintf("%s", this$shortenFilename(this$selected.project)))
    tkgrid(lab, sticky="w", padx=5)
    
    i <- j <- 1
    while (i < (len * 4)) {
      #printf("j=%s vars=%s cutoffs=%s subfID=%s", j, this$selected.vars[j], this$selected.cutoffs[j], paste(subfID, ".", i + 3, sep=""))
      #tclvalue(this$cbVal[[j]]) <- "0"
      #tclvalue(this$vcutoffs[[j]]) <- this$selected.cutoffs[j]
      #tclvalue(this$cbcutoffperc[[j]]) <- "0"
      cutoffcb <- tcl("checkbutton", paste(subfID, ".", i, sep=""), variable=this$cbVal[[j]], text=this$selected.vars[j])
      cutofflabel <- tcl("label", paste(subfID, ".", i + 1, sep=""), text="cutoff: ")
      cutoffentry <- tcl("entry", paste(subfID, ".", i + 2, sep=""), width=6, textvariable=this$vcutoffs[[j]])
      cutoffperccb <- tcl("checkbutton", paste(subfID, ".", i + 3, sep=""), variable=this$cbcutoffperc[[j]], text="in %")
      tkgrid(cutoffcb, cutofflabel, cutoffentry, cutoffperccb, sticky="w", pady=1)
      i <- i + 4
      j <- j + 1
    }
    ###
    
    ### then go through idx list and set check/cutoff 
    for (i in 1:len) {
      ### if one of the var where in the old file
      # and cutoff was not zero
      if (length(this$idx[[i]]) > 0 & as.numeric(this$idx[[i]][3]) != 0) {
        position <- as.numeric(this$idx[[i]][1])
        tclvalue(this$cbVal[[i]]) <- this$idx[[i]][2]
        tclvalue(this$vcutoffs[[i]]) <- this$idx[[i]][3]
        if (this$working & tclvalue(this$vcutoffs[[i]]) != "0") {
          printf("w: changed var=%s from position %s to %s with value=%s, checked=%s", 
                 this$selected.vars[i], position, i, tclvalue(this$vcutoffs[[position]]), tclvalue(this$cbVal[[position]])
         )
        }
      }
    }
  }
  printf("w: Done refreshMarker")
}

fcs$refreshCutoffs <- function (fileindex=NA, current=FALSE, reset=FALSE, saved=FALSE) {
  this <- fcs
  
  if (is.na(fileindex)) {
    fileindex <- this$selected.filenum
  }
  len <- length(this$selected.cutoffs)
  len <- length(this$selected.vars)
  
  printf("w: do refreshCutoffs: table=%s fileidx=%s current=%s reset=%s load_saved=%s len=%s", this$selected.project, fileindex, current, reset, saved, len)
  
  if (reset) {
    ## rest cutoffs in GUI and in array new.cutoffs
    this$selected.cutoffs <- rep(0, len)
    this$new.cutoffs[[this$selected.filenum]] <- this$selected.cutoffs
    this$selected.checks <- rep("0", len)
    this$new.checks[[this$selected.filenum]] <- this$selected.checks
  } else if (current) {
    ### save current cutoffs in array new.cutoffs
    for (i in 1:len) {
      this$selected.cutoffs[i] <- this$checkDigits(cutoff_id=i)
      this$selected.checks[i] <- tclvalue(this$cbVal[[i]])
    }
    
    
    this$new.cutoffs[[this$current.filenum]] <- this$selected.cutoffs
    this$new.checks[[this$current.filenum]] <- this$selected.checks
  } else if (saved) {
    if (length(this$saved.cutoffs) > 0) {
      ### load saved cutoffs from database if available
      if (any(this$saved.cutoffs[[fileindex]] != "0")) {
        ### AND
        ### if saved cutoffs arent only zeros "0"
        this$selected.cutoffs <- this$saved.cutoffs[[fileindex]]
        this$selected.cutoffs[is.na(this$selected.cutoffs)] <- 0
        this$selected.checks <- this$saved.checks[[fileindex]]
        this$selected.checks[is.na(this$selected.checks)] <- "0"
      }
    } else {
      printf("No cutoffs in database saved.")
      if (any(this$new.cutoffs[[fileindex]] != "0")) {
        printf("Loading cutoffs from current session.")
        this$selected.cutoffs <- this$new.cutoffs[[fileindex]]
        this$current.checks <- this$new.checks[[fileindex]]
      }
    }
  } else {
    len <- length(this$selected.vars)
    ### first save cutoffs in recent file 
    for (i in 1:len) {
      this$current.cutoffs[i] <- this$checkDigits(cutoff_id=i)
      this$current.checks[i] <- tclvalue(this$cbVal[[i]])
    }
    this$new.cutoffs[[this$current.filenum]] <- this$current.cutoffs
    this$new.checks[[this$current.filenum]] <- this$current.checks
    
    ### then load cutoffs from current chosen file
    if (any(this$new.cutoffs[[fileindex]] != "0")) {
      this$selected.cutoffs <- this$new.cutoffs[[fileindex]]
      this$selected.checks <- this$new.checks[[fileindex]]
    }
  }
  
  ### if present cutoff length is longer than recent 
  # initiate additional positions 
  len <- length(this$selected.vars)
  if (len > length(this$vcutoffs)) {
    for (i in len:length(this$vcutoffs)) {
      this$cbVal[[i]] <- tclVar("0")
      this$vcutoffs[[i]] <- tclVar(this$selected.cutoffs[i])
      this$cbcutoffperc[[i]] <- tclVar("0")
    }
  }
  
  ### display cutoffs
  for (i in 1:len) {
    tclvalue(this$vcutoffs[[i]]) <- as.character(this$selected.cutoffs[i])
    tclvalue(this$cbVal[[i]]) <- this$selected.checks[i]
  }
  
  printf("w: Done refreshCutoffs: %s cutoffs:%s", len, paste(this$selected.cutoffs, collapse=" "))
  this$current.cutoffs <- this$selected.cutoffs
  this$current.checks <- this$selected.checks
}

fcs$refreshComboboxVars <- function(file) {
  # refresh var1 var2 var3 combobox in tabs
  this <- fcs
  
  displayfile <- this$shortenFilename(file, title=TRUE)
  
  ### refresh filenames in title
  tkdelete(this$title, "1.0", "end")
  tkinsert(this$title, "1.0", displayfile)
  
  if (grepl("^temp\\d\\d", file)) {
    fileindex <- 1
  } else {
    fileindex <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  }
  
  
  
  # if stain index changed set stain variables and refresh
  if (fileindex != this$current.filenum) {
    
    printf("w: do refreshComboboxVars: fileidx=%s file=%s", fileindex, file)
    
    this$selected.filenum <- fileindex
    this$selected.vars <- this$getVariables(index=fileindex)
    
    # if FSC/SSC are listed
    tmp <- max(grep("SC", this$selected.vars)) 
    tmp2 <- min(grep(c("Time"), this$selected.vars))
    if (tmp2 > tmp & !is.infinite(tmp2) & tmp2 < (length(this$selected.vars) - 3)) tmp <- tmp2
    if (((tmp + 3) > length(this$selected.vars)) | is.infinite(tmp)) tmp <- 0
    
    
    ######## triploT
    #### compare with old vars: if match then let it put
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1)))
    ## if var1 is not available in new file
    if (length(var1) == 0) {
      var1 <- this$selected.vars[tmp + 1]
      printf("w: var1-tri changed to %s", var1)
    }
    tkconfigure(this$cbvar1, values=this$selected.vars)
    tkset(this$cbvar1, var1)
    var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2)))
    ## if var2 is not available in new file
    if (length(var2) == 0) {
      var2 <- this$selected.vars[tmp + 2]
      printf("w: var2-tri changed to %s", var2)
    }
    tkconfigure(this$cbvar2, values=this$selected.vars)
    tkset(this$cbvar2, var2)
    var3 <- this$checkMarker(tclvalue(tkget(this$cbvar3)))
    ## if var3 is not available in new file
    if (length(var3) == 0) {
      var3 <- this$selected.vars[tmp + 3]
      printf("w: var3-tri changed to %s", var3)
    }
    tkconfigure(this$cbvar3, values=this$selected.vars)
    tkset(this$cbvar3, var3)
    
    
    ######## diploT
    #### compare with old vars: if match then let it put
    var1 <- this$checkMarker(tclvalue(tkget(this$cbvar1di)))
    ## if var1 is not available in new file
    if (length(var1) == 0) {
      var1 <- this$selected.vars[tmp + 1]
      printf("w: var1-di changed to %s", var1)
    }
    tkconfigure(this$cbvar1di, values=this$selected.vars)
    tkset(this$cbvar1di, var1)
    ## if var2 is not available in new file
    if (length(var2) == 0) {
      var2 <- this$selected.vars[tmp + 2]
      printf("w: var2-di changed to %s", var2)
    }
    var2 <- this$checkMarker(tclvalue(tkget(this$cbvar2di)))
    tkconfigure(this$cbvar2di, values=this$selected.vars)
    tkset(this$cbvar2di, var2)
    
    
    this$refreshMarker()
    
    # if change file, undo "gate data"
    # so that temporary data is not set
    tclvalue(this$cbtgateData) <- "0"
    
    printf("w: Done refreshComboboxVars")
  }
}

fcs$closePreproc <- function(mode) {
  this <- fcs
  
  ### ask for apply procession
  # refresh files and marker according to procession window
  if (mode == 1) this$refreshComboboxVars(tclvalue(tkget(this$current.filePrep)))
  # close window
  tkdestroy(this$ttpreproc)
}

fcs$changeFI.range <- function (mode) {
  this <- fcs
  
  if (mode == 1) {
    tclvalue(this$rbtrans) <- "asinh"
    tclvalue(this$vminvalX) <- this$asinh$range[1]
    tclvalue(this$vmaxvalX) <- this$asinh$range[2]
    tclvalue(this$vminvalY) <- this$asinh$range[3]
    tclvalue(this$vmaxvalY) <- this$asinh$range[4]
    
    ## change in triploT
    tkset(this$binSize, this$asinh$binsize)
    tkset(this$minCountTri, this$asinh$mincount)
    
    ## change in diploT
    tkset(this$binSizedi, this$asinh$binsize)
    tkset(this$minCountDi, this$asinh$mincount)
  } else if (mode == 2) {
    tclvalue(this$rbtrans) <- "biex"
    tclvalue(this$vminvalX) <- this$asinh$range[1]
    tclvalue(this$vmaxvalX) <- this$asinh$range[2]
    tclvalue(this$vminvalY) <- this$asinh$range[3]
    tclvalue(this$vmaxvalY) <- this$asinh$range[4]
    
    ## change in triploT
    tkset(this$binSize, this$asinh$binsize)
    tkset(this$minCountTri, this$asinh$mincount)
    tkconfigure(this$binSize, values=this$binSizes.biex)
    tkset(this$binSize, this$current.biex$binSize)
    tkset(this$minCountTri, this$current.biex$mincount)
    
    ## change in diploT
    tkconfigure(this$binSizedi, values=this$binSizes.biex)
    tkset(this$binSizedi, this$current.biex$binSize)
    tkset(this$minCountDi, this$current.biex$mincount)
  }
}


### residual GUI functions -----------------------------------------------------
fcs$addRectInfo <- function (setcex=1.0) {
  this <- fcs
  xmin.FI <- as.double(tclvalue(this$vminvalX))
  ymax.FI <- as.double(tclvalue(this$vmaxvalY))
  
  rect(this$coords$x[1], this$coords$y[1], this$coords$x[2], this$coords$y[2])
  text(xmin.FI, (ymax.FI + 1.0), sprintf("x=[%.1f, %.1f]; y=[%.1f, %.1f]", this$coords$x[1], this$coords$x[2], this$coords$y[1], this$coords$y[2]), cex=0.8 * setcex, adj=0)
  this$plot.attr[[1]]$RectInfo <- TRUE
}

fcs$addCellInfo <- function (setcex=1.0) {
  this <- fcs
  xmin.FI <- as.double(tclvalue(this$vminvalX))
  ymin.FI <- as.double(tclvalue(this$vminvalY))
  
  this$ncell.perc <- round(this$ncell.sel / this$origin.ncells * 100, 2)
  text(xmin.FI, (ymin.FI - 0.5), paste("#all", this$origin.ncells, "#sel", this$ncell.sel, "%", this$ncell.perc), cex=0.8 * setcex, adj=0)
  this$plot.attr[[1]]$CellInfo <- TRUE
}

fcs$doRect <- function () {
  this <- fcs
  
  this$rect.lwd <- this$rect.lwd + 0.5
  
  # if manual rect was not selected
  if (tclvalue(this$cbtmanRect) == "0") {
    
    dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
    devSet(devList()[tail(which(dev.label == names(devList())), n=1)])
    
    this$coords <- lapply(locator(2), round, 1)
    
    # start from left bottom corner to top upper
    if (this$coords$x[2] < this$coords$x[1]) {
      tmp <- this$coords$x[2]
      this$coords$x[2] <- this$coords$x[1]
      this$coords$x[1] <- tmp
    }
    if (this$coords$y[2] < this$coords$y[1]) {
      tmp <- this$coords$y[2]
      this$coords$y[2] <- this$coords$y[1]
      this$coords$y[1] <- tmp
    }
    
    # set coords in manual rect entries
    tclvalue(this$vx1) <- as.character(this$coords$x[1])
    tclvalue(this$vx2) <- as.character(this$coords$x[2])
    tclvalue(this$vy1) <- as.character(this$coords$y[1])
    tclvalue(this$vy2) <- as.character(this$coords$y[2])
    
  }
  
  # plot rectangle
  rect(this$coords$x[1], this$coords$y[1], this$coords$x[2], this$coords$y[2], lwd=this$rect.lwd)
  
  #file <- tclvalue(tkget(this$tkchoosefile))
  #this$getInfo(file)
  
  tkraise(this$tt)
}

fcs$getInfo <- function (file, vars) {
  # get information: vars, rectangle, num cells selected (%)
  this <- fcs
  checkGATED <- tclvalue(this$cbtgateData)
  
  printf("w: do getInfo file=%s GATED=%s temp.num=%s", file, checkGATED, this$temp.num)
  
  # if gate, stay at temporary data
  if ((checkGATED == "1") & this$temp.num > 0 & grepl("^temp\\d+", file)) {
    table <- file
    file.idx <- 0
  } else {
    table <- this$selected.project
    file.idx <- this$current.filetable[which(this$current.filetable[, 2] == file), 1]
  }
  
  if (this$current.project != table | this$current.filenum != file.idx) this$getRectData(table, file.idx, vars)
  
  # refresh info about selected cells and percentage
  this$ncell.sel <- nrow(this$data)
  this$ncell.perc <- round(this$ncell.sel / this$origin.ncells * 100, 2)
  tkconfigure(this$ncell.sel.gui, text=as.character(this$ncell.sel))
  tkconfigure(this$ncell.perc.gui, text=as.character(this$ncell.perc))
  tkconfigure(this$ncell.sel.gui.di, text=as.character(this$ncell.sel))
  tkconfigure(this$ncell.perc.gui.di, text=as.character(this$ncell.perc))
  
  file <- tclvalue(tkget(this$tkchoosefile))
  f <- unlist(strsplit(file, "\\, |\\, |\\; |\\;|\\. | "))
  f <- f[length(f)]
  
  # also refresh rect attr from "dataframes1/2"
  this$rect.attr <- list(
    table = this$current.project, 
    file.name = f, 
    origin.ncells = this$origin.ncells, 
    sel.ncells = this$ncell.sel, 
    binSize = as.numeric(tclvalue(tkget(this$binSize))), 
    mincount = as.numeric(tclvalue(tkget(this$minCountTri))), 
    vars = vars, 
    coords=c(this$coords$x[1], this$coords$x[2], this$coords$y[1], this$coords$y[2])
 )
}

fcs$addProdline <- function (cutoffs=c(0, 0)) {
  this <- fcs
  # add production cutoff line if provided
  if (cutoffs[1] > 0) {
    abline(v=cutoffs[1], col="darkgrey")
  }
  if (cutoffs[2] > 0) {
    abline(h=cutoffs[2], col="darkgrey")
  }
}

fcs$addTitle <- function(mode="") {
  this <- fcs
  
  if (length(dev.cur() == devList()[grep("di", names(devList()))]) > 0)  set.cex <- 0.9
  else set.cex <- 1.3
  
  if (mode == "date") {
    date <- gsub("-", "", Sys.Date())
    title(main=date, outer=TRUE, line=1, cex.main=set.cex, adj=1)
  } else {
    text <- as.character(tclvalue(tkget(this$title, "1.0", "end-1c")))
    
    if (!is.null(dev.list())) {
      # get title withouth new line "\n" with "end-1c"
      title(main=text, outer=TRUE, line=1, cex.main=set.cex)
    } else {
      tkmessageBox(title = "An error has occured!", 
                   message = "There is no plot window to print your title.", icon = "error", type = "ok")
      stop("There is no plot window to print your title.")
    }
  }
}

fcs$newPlotWindow <- function () {
  this <- fcs
  
  this$plotter.tri.num <- this$plotter.tri.num + 1
  nrow <- as.numeric(tclvalue(this$vnrow))
  ncol <- as.numeric(tclvalue(this$vncol))
  dev.label <- paste("plotter", "tri", this$plotter.tri.num, sep=".")
  
  #x11(width=ncol * 4, height=nrow * 4)
  devNew(type="x11", title="n-triploTs", width=ncol * 3.4, height=nrow * 3.7, label=dev.label)
  # mar in points, mai in inches
  # oma adds title lines
  # order: bottom, left, top, and right
  par(mfrow=c(nrow, ncol), oma=c(0.5, 1, 2, 1), mar=c(3, 3, 4, 2))
  #this$plotWindowExists <- TRUE
  #this$dev.plot <- dev.cur()
  #this$dev.list <- dev.list()
  this$plot.windows <- c(this$plot.windows, dev.label)
}

fcs$clickTab <- function(...) {
  this <- fcs
  
  tab.old <- this$current.tab
  tab.new <- tclvalue(tkindex(this$panelTabs, "current"))
  
  if (this$working) printf("w: do clickTab tab.old=%s tab.new=%s", tab.old, tab.new)
  
  if (!tab.new %in% c("3", "4")) this$refreshMarker(tab=TRUE)
  
  this$current.tab <- tab.new
}

fcs$clickinsertDF <- function(widget, ...) {
  # mouse click prints dataframe to treetable
  this <- fcs
  
  shade <- c("none", "gray")
  tkdelete(this$treetable, tkchildren(this$treetable, ""))
  
  # get data frame
  env <- ...[1]
  df <- eval(parse(text=paste("this$", env, sep="")))
  
  # insert df header
  tkconfigure(this$treetable, columns=colnames(df))
  for (i in 0:(ncol(df) - 1)){
    tcl(this$treetable, "heading", i, text=colnames(df)[i + 1])
  }
  
  # insert df content
  for (i in 1:nrow(df)) {
    tkinsert(this$treetable, "", "end", values=as.character(df[i, ]), tag=shade[i %% 2 + 1])
  }
  
  tktag.configure(this$treetable, "gray", background="gray95")
}

fcs$debug <- function() {
  this <- fcs
  this$ttdebug <- tktoplevel()
  
  # set window title
  tkwm.title(this$ttdebug, "Debug Functions")
  #set window size
  tkwm.geometry(this$ttdebug, "900x600")
  
  this$GUIdebug(this$ttdebug)
}

fcs$clickinsert <- function(widget, ...) {
  # mouse click prints function to text field
  this <- fcs
  
  child <- ...[1]
  parent <- tkparent(this$treewidget, child)
  
  # first delete all text in text field
  tkdelete(this$txt, "1.0", "end")
  
  # get function text
  if (as.character(parent) != "root") {
    
    # then print [child]$[function]=function
    tkinsert(this$txt, "end", paste(parent, "$", child, "=", sep=""))
    
    # get function
    geter <- get(child, eval(parse(text=as.character(parent))))
    
    # now print function
    tkinsert(this$txt, "end", paste(deparse(geter), collapse="\n"), collapse="\n\n")
  } else {
    tkinsert(this$txt, "end", "This is our parent environment.")
  }
}

fcs$saveWindow <- function (type="diploT") {
  this <- fcs
  
  displayfile <- tclvalue(tkget(this$tkchoosefile))
  ncol <- as.numeric(tclvalue(this$vncol))
  nrow <- as.numeric(tclvalue(this$vnrow))
  
  displayfile <- this$shortenFilename(displayfile, title=TRUE)
  
  # if path didnt save yet
  if (!exists("lastpath", where=eval(parse(text="fcs")))){
    this$lastpath <- getwd()
  }
  
  active.window <- names(which(devList() == dev.cur()))
  var1.di <- this$checkMarker(tclvalue(tkget(this$cbvar1di)))
  if (grepl("histogram", active.window)) {
    plot.type <- "histograms"
    plot.title <- sprintf("histograms of %s", displayfile)
  } else if (grepl("digraph", active.window)) {
    plot.type <- sprintf("diGraphs_%s", var1.di)
    plot.title <- sprintf("diGraphs of %s (%s)", var1.di, displayfile)
  } else if (grepl("di.", active.window)) {
    plot.type <- sprintf("diploTs_%s", var1.di)
    plot.title <- sprintf("diploTs of %s (%s)", var1.di, displayfile)
  } else {
    plot.type <- "triploTs" 
    plot.title <- sprintf("triploTs of %s", displayfile)
  }
  file <- tkgetSaveFile(defaultextension=".pdf", initialdir= this$lastpath, 
                       initialfile=sprintf("%s_%s_%s.pdf", displayfile, plot.type, this$version), 
                       title = plot.title)
  printf("save file: %s", tclvalue(file))
  ### remove file name to get file path
  filepath <- unlist(strsplit(as.character(file), "/"))
  filepath <- filepath[-length(filepath)]
  filepath <- paste(filepath, collapse="/")
  this$lastpath <- filepath
  
  ### save pdf in original size
  if (grepl("histogram", active.window)) {
    dev.copy2pdf(file=tclvalue(file), 
                 pointsize=5, 
                 title = plot.title, 
   )
  } else if (type == "diploT") {
    dev.label <- paste("plotter", "di", this$plotter.di.num, sep=".")
    if (length(which(dev.label == names(devList()))) != 0) devSet(devList()[which(dev.label == names(devList()))])
    dev.copy2pdf(file=tclvalue(file), 
                 title = plot.title)
    #devOff()
  } else {
    ### triploT windows
    # does not matter how many rows/columns displayed
    dev.copy2pdf(file=tclvalue(file), 
                 pointsize=12 * (ncol / 4), width=3.66 * ncol, height=4 * nrow, 
                 title = plot.title)
  }
  
  print(paste("PDF saved:", file))
  
  this$lastpath <- filepath
}

fcs$savePNG <- function (tdata=NULL, single=FALSE) {
  this <- fcs
  
  if (this$working) printf("w do savePNG: single=%s", single)
  
  xminval <- as.numeric(tkget(this$minvalX))
  xmaxval <- as.numeric(tkget(this$maxvalX))
  yminval <- as.numeric(tkget(this$minvalY))
  ymaxval <- as.numeric(tkget(this$maxvalY))
  
  binSize <- as.numeric(tkget(this$binSize))
  mincount <- as.numeric(tkget(this$minCountTri))
  
  checkCALC <- tclvalue(this$rbcalc)
  checkTRANS <- tclvalue(this$rbtrans)
  checkGATED <- tclvalue(this$cbtgateData)
  checkGRID <- tclvalue(this$cbtshowGrid)
  
  prodcells.col <- "black"
  quadrants.col <- "black"
  
  # if path didnt save yet
  if (!exists("lastpath", where=eval(parse(text="fcs")))){
    this$lastpath <- getwd()
  }
  
  if (single  == TRUE) {
    
    displayfile <- tclvalue(tkget(this$tkchoosefile))
    displayfile <- this$shortenFilename(displayfile)
    
    v1 <- tclvalue(tkget(this$cbvar1))
    v2 <- tclvalue(tkget(this$cbvar2))
    v3 <- tclvalue(tkget(this$cbvar3))
    vars <- c(v1, v2, v3)
    
    png.name <- file.path(sprintf("%s_%s_%s_%s_%s.png", displayfile, paste(vars, collapse="_"), checkTRANS, checkCALC, this$version))
    file <- tkgetSaveFile(initialdir= this$lastpath, defaultextension=".png", initialfile=png.name)
    
    cutoff_idx <- which(this$selected.vars == v1)
    cutoff_idy <- which(this$selected.vars == v2)
    cutoff_idz <- which(this$selected.vars == v3)
    cutoffs <- c(cutoff_idx, cutoff_idy, cutoff_idz)
    
    tdata <- as.matrix(this$data[vars])
    
    ### if percentage is checked
    # calculate cutoffs and set check button to zero
    for (i in 1:length(cutoffs)) {
      if (tclvalue(this$cbcutoffperc[[cutoffs[i]]]) == "1") {
        cutoffs[i] <- this$calcCUTOFF(tdata[vars[i]], this$checkDigits(cutoff_id=cutoffs[i]), vars[i], cutoffs[i])
      } else {
        cutoffs[i] <- this$checkDigits(cutoff_id=cutoffs[i])
      }
    }
  } else {
    vars <- colnames(tdata)
    
    png.folder <- file.path(this$lastpath, "png")
    
    if (grepl("linux", sessionInfo()$R.version$os)) {
      system(paste("mkdir -p -v ", png.folder, sep=""))
    } else {
      system(paste("mkdir ", png.folder, sep=""))
    }
    
    if (this$OverviewGate) {
      displayfile <- this$plot.attr[[1]]$file.name
    } else {
      displayfile <- tclvalue(tkget(this$tkchoosefile))
    }
    displayfile <- this$shortenFilename(displayfile)
    
    file <- file.path(png.folder, sprintf("%s_%s_%s_%s_%s.png", displayfile, paste(vars, collapse="_"), checkTRANS, checkCALC, this$version))
  }
  
  ### remove file name to get file path
  filepath <- unlist(strsplit(as.character(file), "/"))
  filename <- filepath[length(filepath)]
  filename <- sub(".png$", "", filename)
  
  filepath <- filepath[-length(filepath)]
  filepath <- paste(filepath, collapse="/")
  this$lastpath <- filepath
  
  ### png plot
  toPNG(name=filename, path=filepath, width=800, height=850, bg="white", {
    par(oma=c(2, 2, 2, 1), mar=c(4.5, 4.5, 5, 1))
    #png.cur <- dev.cur()
    
    #### plot start
    set.cex <- 1.4
    set.cex.axes <- 1.4
    set.mgp <- c(1.9, 0.6, 0)
    if (cutoffs[1] > 0) title.axis <- sprintf("%s (%s)", vars[1], cutoffs[1])
    else title.axis <- vars[1]
    if (cutoffs[2] > 0) title.axis <- c(title.axis, sprintf("%s (%s)", vars[2], cutoffs[2]))
    else title.axis <- c(title.axis, vars[2])
    
    plot(1, type="n", frame.plot=FALSE, xlim=c(xminval, xmaxval + 10 * binSize), axes=FALSE, 
         ylim=c(yminval - 2.5 * binSize, ymaxval + 5 * binSize), xlab=title.axis[1], ylab=title.axis[2], cex.lab=set.cex, cex.axis=0.5 * set.cex.axes, mgp=set.mgp)
    box(lwd=0.8, col="darkgrey")
    
    if (checkTRANS == "asinh") {
      scale <- this$asinh$scale
      label <- this$asinh$label
      grid.step <- this$asinh$step
    } else {
      scale <- this$biex$scale
      label <- this$biex$label
      grid.step <- this$biex$step
    } 
    
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
    if (cutoffs[1] > 0 & cutoffs[2] > 0) {
      
      ### count cells in quadrant
      tdata.q1 <- tdata[which(tdata[, 1] < cutoffs[1] &  tdata[, 2] < cutoffs[2]), 3]
      tdata.q2 <- tdata[which(tdata[, 1] >= cutoffs[1] &  tdata[, 2] < cutoffs[2]), 3]
      tdata.q3 <- tdata[which(tdata[, 1] >= cutoffs[1] &  tdata[, 2] >= cutoffs[2]), 3]
      tdata.q4 <- tdata[which(tdata[, 1] < cutoffs[1] &  tdata[, 2] >= cutoffs[2]), 3]
      
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
          tdata.plus <- tdata[which(tdata[, 3] > cutoffs[3]), ]
          ncells <- nrow(tdata.plus)
          
          tdata.q1 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] &  tdata.plus[, 2] < cutoffs[2]), 3]
          tdata.q2 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] &  tdata.plus[, 2] < cutoffs[2]), 3]
          tdata.q3 <- tdata.plus[which(tdata.plus[, 1] >= cutoffs[1] &  tdata.plus[, 2] >= cutoffs[2]), 3]
          tdata.q4 <- tdata.plus[which(tdata.plus[, 1] < cutoffs[1] &  tdata.plus[, 2] >= cutoffs[2]), 3]
          
          ### q[x].total [ink=blue]
          ### in MSI(+): percentage of cells in quadrant to total positive cells
          this$q1.total <- abs(100 * length(which(tdata.plus[, 1] < cutoffs[1] &  tdata.plus[, 2] < cutoffs[2])) / ncells)
          this$q2.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] &  tdata.plus[, 2] < cutoffs[2])) / ncells)
          this$q3.total <- abs(100 * length(which(tdata.plus[, 1] >= cutoffs[1] &  tdata.plus[, 2] >= cutoffs[2])) / ncells)
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
      }
    }
    
    tdata.zero <- tdata[apply(tdata[, -1], MARGIN = 1, function(row) {
      all(row > 0)
      }), ]
    ncells.zero <- nrow(tdata.zero)
    
    ### calc quadrants with only positive values
    # q1 Quadrant unten links
    # q2 Quadrant unten rechts
    # q3 Quadrant oben rechts
    # q4 Quadrant oben links
    if (cutoffs[1] > 0 & cutoffs[2] > 0) {
      q1.zero <- abs(100 * length(which(tdata.zero[, 1] < cutoffs[1] &  tdata.zero[, 2] < cutoffs[2])) / ncells.zero)      
      q2.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[1] &  tdata.zero[, 2] < cutoffs[2])) / ncells.zero)                    
      q3.zero <- abs(100 * length(which(tdata.zero[, 1] >= cutoffs[1] &  tdata.zero[, 2] >= cutoffs[2])) / ncells.zero)                    
      q4.zero <- abs(100 - q1.zero - q2.zero - q3.zero)
    }
    
    if (checkCALC == "density") {
      this$bintriplot(data=tdata, cutoffs=cutoffs, density=TRUE, binSize=binSize, mincells=mincount, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
    } else if (checkCALC == "MSI(+)") {
      this$bintriplot(data=tdata.plus, cutoffs=cutoffs, binSize=binSize, mincells=mincount, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp, quadrants.color = "blue", data.origin=tdata)
    } else {
      this$bintriplot(data=tdata, cutoffs=cutoffs, binSize=binSize, mincells=mincount, set.cex=label.cex, set.cex.axes=set.cex.axes, set.mgp=set.mgp)
    }
    
    if (checkGATED == "1") firstLine <- sprintf("%s(%0.1f %% ): %s/cof=%s", displayfile, this$ncell.perc, checkCALC, this$current.cofactor)
    else firstLine <- sprintf("%s: %s/cof=%s", displayfile, checkCALC, this$current.cofactor)
    title(main=firstLine, line=3.2, cex.main=1.4, adj=0)
    
    #if (checkCALC == "freq" | checkCALC == "MSI(+)") secondLine=sprintf("cells(min/max)=%s/%s; cutoff=%s", mincount, this$maxcells, cutoffs[3])
    if (checkCALC == "freq" | grepl("MSI", checkCALC)) secondLine <- sprintf("cells(min/max)=%s/%s;%s", mincount, this$maxcells, v3)
    else secondLine <- sprintf("cells(min/max)=%s/%s", mincount, this$maxcells)
    title(main=secondLine, line=2.3, cex.main=1, adj=0)
    
    thirdLine <- sprintf("%s-%s(%0.1f %% ); binSize=%s, #bins=%s", ncells, ncells.zero, (ncells.zero / ncells * 100), binSize, this$bincount)
    title(main=thirdLine, line=1.5, cex.main=1, adj=0)
    
    
    
    if (checkGATED == "1" | grepl("temp", displayfile)) {
      if (length(this$coords.info) > 2) {
        fourthLine <- sprintf("%s; %s", this$coords.info[1], this$coords.info[2])
        fifthLine <- sprintf("%s; %s", this$coords.info[3], this$coords.info[4])
      } else {
        fourthLine <- sprintf("%s", paste(this$coords.info, collapse="; "))
        fifthLine <- ""
      }
      title(main=fourthLine, line=0.8, cex.main=0.9, adj=0)
      title(main=fifthLine, line=0.1, cex.main=0.9, adj=0)
    }
    
  }
 )
  #dev.off(png.cur)
  
  if (single) {
    print(paste("File saved:", file))
    this$lastpng <- file
  } 
}

fcs$saveDF <- function(n){
  this <- fcs
  
  attr <- this$rect.attr
  
  # if path didnt save yet
  if (!exists("lastpath", where=eval(parse(text="fcs")))){
    this$lastpath <- getwd()
  }
  
  file <- tkgetSaveFile(defaultextension=".csv", initialdir= this$lastpath, initialfile=attr$file.name)
  
  ### remove file name to get file path
  filepath <- unlist(strsplit(as.character(file), "/"))
  filepath <- filepath[-length(filepath)]
  filepath <- paste(filepath, collapse="/")
  this$lastpath <- filepath
  
  
  comments <- vector()
  comments[1] <- paste("# project:", attr$table)
  comments[2] <- paste("# file name:", attr$file.name)
  comments[3] <- paste("# selected columns:", attr$vars)
  comments[4] <- sprintf("# rectangle at: x=[%.1f, %.1f]; y=[%.1f, %.1f]", attr$coords[1], attr$coords[2], attr$coords[3], attr$coords[4])
  comments[5] <- paste("# bin size:", attr$binSize)
  comments[6] <- paste("# min(cells) per bin:", attr$mincout)
  comments[7] <- paste("# original cell count:", attr$origin.ncells)
  comments[8] <- paste("# selected cell count:", attr$sel.ncells)
  comments[9] <- paste("# selected cell count(%):", round((attr$sel.ncells / attr$origin.ncells * 100), 2))
  
  # insert df content
  if (!tclvalue(file) == "") {
    chn <- tcl("open", file, "w")
    
    # insert comments
    for (i in 1:length(comments)){
      tcl("puts", chn, comments[i])
      print(comments[i])
    }
    
    # insert dataframe
    df <- eval(parse(text=paste("this$dataframes", n, sep="")))
    tmp <- colnames(df)[1]
    
    for (i in 2:ncol(df)){
      tmp <- paste(tmp, colnames(df)[i], sep=", ")
    }
    print(tmp)
    tcl("puts", chn, tmp)
    
    for (j in 1:nrow(df)) {
      tmp  <- df[j, 1]
      for (k in 2:ncol(df)){
        tmp <-  paste(tmp, df[j, k], sep=", ")
      }
      tcl("puts", chn, tmp)
    }
    
    tcl("close", chn)
    print(paste("Table Saved: ", tclvalue(file)))
    
  }
}


### not in use --------------------------------------------
if (FALSE) {
  fcs$saveCode <- function(){
    this <- fcs
    
    # if didn't save yet
    #if (!exists("lastfile", where=eval(parse(text=tkparent(this$treewidget, "lastfile"))))){
    #if (!exists("lastfile", where=eval(parse(text="fcs")))){
    #this$lastfile <- paste(getwd(), "/3D_plotter_", this$version, ".r", sep="")
    #}
    
    # if path didnt save yet
    if (!exists("lastpath", where=eval(parse(text="fcs")))){
      this$lastpath <- getwd()
    }
    
    file <- tkgetSaveFile(defaultextension=".r", initialdir= this$lastpath, initialfile=paste("/3D_plotter_", this$version, ".r", sep=""))
    
    ### remove file name to get file path
    filepath <- unlist(strsplit(as.character(file), "/"))
    filepath <- filepath[-length(filepath)]
    filepath <- paste(filepath, collapse="/")
    this$lastpath <- filepath
    
    
    if (!tclvalue(file) == "") {
      chn <- tcl("open", file, "w")
      tcl("puts", chn, tkget(this$txt, "0.0", "end"))
      tcl("close", chn)
    }
  }
  
  fcs$printall <- function () {
    this <- fcs
    
    # read script, separate each line
    script.header <- scan("../YH_3D_plotter.r", character(0), sep ="\n", quiet=TRUE, nlines=53) 
    
    # get only header and tail
    script.header <- noquote(script.header)
    if (this$working) printf("w: %s", script.header)
    
    # first delete all text in text field
    tkdelete(this$txt, "1.0", "end")
    
    # insert header
    tkinsert(this$txt, "end", paste(script.header, collapse="\n"))
    tkinsert(this$txt, "end", "\n\n")
    
    for (i in 1:length(this$parent.env)) {
      children <- ls(eval(parse(text=paste(this$parent.env[i]))))
      
      for (j in 1:length(children)) {
        if (class(eval(parse(text=paste("this$", children[j], sep=""))))[1] == "function") {
          
          parent <- tkparent(this$treewidget, children[j])
          
          # then print [children]$[function]=function
          tkinsert(this$txt, "end", paste(parent, "$", children[j], "=", sep=""))
          
          # get function
          geter <- get(children[j], eval(parse(text=as.character(parent))))
          
          # now print function
          tkinsert(this$txt, "end", paste(deparse(geter), collapse="\n"), collapse="\n\n")
        }
      }
    }
  }
  
  fcs$refreshCode <- function(){
    this <- fcs
    printf("w: do refreshCode :: destroy GUI and redo")
    # 1.0 means first row first character
    code <- tkget(this$txt, "1.0", "end")
    eval(parse(text=paste(as.character(tclvalue(code)), collapse="")))
    tkdestroy(this$tt)
    this$GUImain()
  }
}

