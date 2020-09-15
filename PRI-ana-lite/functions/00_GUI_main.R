#!/usr/bin/R
# Author: Yen Hoang and Felix Lohrke
# Date: 2020

# ---------- # GUI base function # ---------- #
Main$GUImain <- function() {

    Current = Main

    # databse selection via pop up at start of PRI-ana-lite
    ## there should be a default option/use db from last time
    #tk_messageBox(caption = "PRI-ANA-LITE V0.1", message = "Welcome to PRI-ANA-Lite, please select the database you want to work with.")
    #Current$database = tkgetOpenFile()

    # creating base and adding main title
    Current$mainframe = tktoplevel()
    tkwm.title(Current$mainframe, "PRI-ANA-LITE")

    # set window size
    tkwm.geometry(Current$mainframe, "1200x800")

    # create topMenu with close command
    topMenu <- tkmenu(Current$mainframe)
    tkconfigure(Current$mainframe, menu=topMenu)
    fileMenu <- tkmenu(topMenu, tearoff=FALSE)
    tkadd(topMenu, "command", label = "Close", command = function() {
      Current$GUIquit()
    })

    ### Preselection Status of project (preselect = TRUE)
    if (Current$preselection == TRUE) {
       
      # create listbox that contains projects
      listbox = tklistbox(Current$mainframe)
      tkpack(listbox, side = "top")
      tk2tip(listbox, "Double Click to Open")

      # retrieve tablenames from database 
      all_tables = Current$returnTableNames(file.path(Current$db.path, Current$db.name))

      # display tablenames in listbox
      for (table in 1:length(all_tables)) {
        tkinsert(listbox, "end", all_tables[table])
      }

      # bind double click command to select project
      tkbind(listbox, "<Double-Button-1>", function(){
        selection = as.integer(tclvalue(tkcurselection(listbox)))

        # assigning selected project
        Current$project = Current$returnTableNames(file.path(Current$db.path, Current$db.name))[selection+1]
        print(Current$project)

        # extracting marker data
        Current$data = Current$getMarkerData(file.path(Current$db.path, Current$db.name), Current$project, 1)

        # extracting sample names
        Current$samples = Current$getSampleNames(file.path(Current$db.path, Current$db.name), paste0(Current$project, "_fileIdentity"))

        # extracting unique marker names
        Current$marker_names = Current$getMarkerNames(file.path(Current$db.path, Current$db.name), paste0(Current$project, "_markerIdentity"), 1)

        # reload of app without preselection window
        Current$preselection = FALSE
        tkdestroy(Current$mainframe)
        Current$GUImain()
      }
      )
    }

    ### Preselection Status of project (preselect = FALSE)

    # add back button if project has been selected
    if (Current$preselection == FALSE) {
      tkadd(topMenu, "command", label = "Back", command = function() {
      Current$GUIback()
    })
    
    # add dropdown menu with samples to select
    # currently selected sample name is returned as selection
    combobox = ttkcombobox(Current$mainframe, values=unlist(Current$samples))
    tkpack(combobox, side = "top", pady = 5)
    tkbind(combobox, "<<ComboboxSelected>>", function() {
        selection = tclvalue(tkget(combobox))
        print(selection)
    })

    middleframe = tkframe(Current$mainframe, relief = "raised", borderwidth = 1)
    button1 = tkbutton(middleframe)
    button2 = tkbutton(middleframe)
    tkpack(button1)
    tkpack(button2, side = "right")
    tkpack(middleframe, fill = "x", side = "top")

    # creating checkboxes with all markers that will be used for calculation/plotting
    # 4 frames with dimension calculation is needed
    marker_frames = ttklabelframe(Current$mainframe, text = "Select Markers: ")
    marker_frame1 = tkframe(marker_frames, relief = "raised", borderwidth = 1)
    marker_frame2 = tkframe(marker_frames, relief = "raised", borderwidth = 1)
    marker_frame3 = tkframe(marker_frames, relief = "raised", borderwidth = 1)
    marker_frame4 = tkframe(marker_frames, relief = "raised", borderwidth = 1)

    # check if markers can be split into 4 quadrants
    # quarter = amount of markers in one quadrant
    residue = 0
    marker_length = nrow(Current$marker_names)
    if (marker_length %% 4 != 0) {
      residue = 1
    }
    quarter = floor(marker_length/4) + residue

    for (i in 1:quarter) {
      checkbox = tkcheckbutton(marker_frame1, variable="unlist(Current$marker_names)[i]", text=unlist(Current$marker_names[i,]))
      cutoffentry = tkentry(marker_frame1, width=4, textvariable="this$vcutoffs[[i]]")
      tkpack(checkbox, tklabel(marker_frame1, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    for (j in (i+1):(i+quarter)) {
      checkbox = tkcheckbutton(marker_frame2, variable="unlist(Current$marker_names)[j]", text=unlist(Current$marker_names[j,]))
      cutoffentry = tkentry(marker_frame2, width=4, textvariable="this$vcutoffs[[i]]")
      tkpack(checkbox, tklabel(marker_frame2, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    for (k in (j+1):(j+quarter)) {
      checkbox = tkcheckbutton(marker_frame3, variable="unlist(Current$marker_names)[k]", text=unlist(Current$marker_names[k,]))
      cutoffentry = tkentry(marker_frame3, width=4, textvariable="this$vcutoffs[[i]]")
      tkpack(checkbox, tklabel(marker_frame3, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    for (l in (k+1):marker_length) {
      checkbox = tkcheckbutton(marker_frame4, variable="unlist(Current$marker_names)[l]", text=unlist(Current$marker_names[l,]))
      cutoffentry = tkentry(marker_frame4, width=4, textvariable="this$vcutoffs[[i]]")
      tkpack(checkbox, tklabel(marker_frame4, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    tkpack(marker_frame1, side = "left", padx = 5, fill = "both")
    tkpack(marker_frame2, side = "left", padx = 5, fill = "both")
    tkpack(marker_frame3, side = "left", padx = 5, fill = "both")
    tkpack(marker_frame4, side = "left", padx = 5, fill = "both")
    #tkpack(marker_frame1, marker_frame2, marker_frame3, marker_frame4, padx=20, pady=1)
    tkpack(marker_frames, fill = "both", expand = TRUE, side = "top")
    
    #tkgrid(this$ttlfautogate, pady=1)
    
    
    
    
    }

    #tkpack(Current$mainframe)

    # create subframe
    #Current$subframe = tkframe(Current$mainframe)

    ## combobox to choose files

    # get filenames for table and combobox
    #Current$current.fileid_name = "_fileIndex"
    #Current$total.projects <- dbListTables(Current$conn)
    #Current$current.project <- Current$total.projects[1]
    #Current$current.filetable <- Current$getDFtable(paste0(Current$current.project, Current$fileid_name))
    #Current$current.filenames <- Current$current.filetable[, 2]


    #listboxframe = tkframe(Current$subframe)
    #Current$listbox = tklistbox(listboxframe, values = "", width = 40)

    #listboxframe = tkframe(Current$subframe)
    #Current$comboboxfiles = tklistbox(listboxframe)
    ## add filenames from database
    ## add select filename functionality
    ## add refresh after select
    #tkpack(tklabel(comboboxframe, text = "File: "), Current$comboboxfiles)
    #tkpack(listboxframe, pady = 10)
    #tkgrid(listboxframe, pady = 10)
    #tkpack(Current$subframe)


    # create entry values
    #test_ = tclVar("")

    # create frame with entryfield and button
    #fentry = tkframe(subframe)
    #tkpack(tklabel(fentry, text = "test"), side = "left")
    #tkpack(tkentry(fentry, textvariable = test_), side = "left")
    #tkpack(tkbutton(fentry, text = "ok", command = Current$GUIquit), side = "bottom")

    #tkpack(fentry, side="top")

    # create frame with quit
    #fquit = tkframe(Current$mainframe)
    #tkpack(tkbutton(fentry, text = "Quit", command = Current$GUIquit), side = "bottom")
    
    # packing
    
    #tkpack(fquit, side="bottom")
    #tkpack(subframe)
}

Main$GUIquit <- function() {
    Current = Main
    tkdestroy(Current$mainframe)
}

Main$GUIback <- function() {
    Current = Main
    Current$preselection = TRUE
    tkdestroy(Current$mainframe)
    Current$GUImain()
}
