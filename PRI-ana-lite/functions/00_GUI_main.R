#!/usr/bin/R
# Author: Felix Lohrke and Yen Hoang
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
    tkwm.title(Current$mainframe, Current$version)

    # set window size
    tkwm.geometry(Current$mainframe, "600x500")

    ### Preselection Status of project (preselect = TRUE)
    if (Current$preselection == TRUE) {

      # create topMenu with close command and db selection
      topMenu <- tkmenu(Current$mainframe)
      tkconfigure(Current$mainframe, menu=topMenu)
      fileMenu <- tkmenu(topMenu, tearoff=FALSE)

      tkadd(topMenu, "command", label = "Close", command = function() {
        Current$GUIquit()
      })
      tkadd(topMenu, "command", label = "Choose DB", command = function() {
        Current$GUIselectdb()
      })
      
      # create padding frame 
      padframe = tkframe(Current$mainframe)
      tkpack(padframe, pady = 30)

      # create labelframe
      label = tklabel(Current$mainframe, text = "Please select the Project you are working on:")
      tkpack(label)

      # create listbox that contains projects
      listbox = tklistbox(Current$mainframe)
      tkpack(listbox, side = "top", pady = 60)
      tk2tip(listbox, "Double Click to Open")

      # retrieve tablenames from database 
      all_tables = Current$returnTableNames(Current$database)

      # display tablenames in listbox
      for (table in 1:length(all_tables)) {
        tkinsert(listbox, "end", all_tables[table])
      }

      # bind double click command to select project
      tkbind(listbox, "<Double-Button-1>", function(){
        selection = as.integer(tclvalue(tkcurselection(listbox)))

        # assigning selected project
        Current$project = Current$returnTableNames(Current$database)[selection+1]
        print(Current$project)

        # extracting marker data
        #Current$data = Current$getMarkerData(file.path(Current$db.path, Current$db.name), Current$project, 1)

        # extracting sample names
        Current$samples = Current$getSampleNames(Current$database, paste0(Current$project, "_fileIdentity"))

        # extracting unique marker names
        Current$marker_names = Current$getMarkerNames(Current$database, paste0(Current$project, "_markerIdentity"), 1)

        # reload of app without preselection window
        Current$preselection = FALSE
        tkdestroy(Current$mainframe)
        Current$GUImain()
      }
      )
    }

    ### Preselection Status of project (preselect = FALSE)

    # create new topMenu with only back button
    if (Current$preselection == FALSE) {
      
      # new window size and menu with close + back
      tkwm.geometry(Current$mainframe, "1200x1000")
      topMenu <- tkmenu(Current$mainframe)
      tkconfigure(Current$mainframe, menu=topMenu)

      tkadd(topMenu, "command", label = "Close", command = function() {
        Current$GUIquit()
      })
      tkadd(topMenu, "command", label = "Back", command = function() {
      Current$GUIback()
      })
    
    # add dropdown menu with samples to select
    # currently selected sample name is returned as selection
    combobox = ttkcombobox(Current$mainframe, values=unlist(Current$samples), width = 30)
    tkset(combobox, unlist(Current$samples)[1])
    tkpack(combobox, side = "top", pady = 5)
    tkbind(combobox, "<<ComboboxSelected>>", function() {
        selection = tclvalue(tkget(combobox))
        selection_idx = tcl(combobox, "current")

        # extracting marker data for selected sample
        #Current$data = Current$getMarkerData(file.path(Current$db.path, Current$db.name), Current$project, 1)
    })

    # area for buttons to plot data
    middleframe = tkframe(Current$mainframe, relief = "raised", borderwidth = 1)
    button_plot_data = tkbutton(middleframe, text = "Plot data with calculated cutoffs")
    button_pdf_data = tkbutton(middleframe, text = "Save as pdf")
    tkpack(button_plot_data, pady = 6)
    tkpack(button_pdf_data, pady = 6)
    tkpack(middleframe, fill = "x", side = "top")

    # area for select buttons
    lowframe = tkframe(Current$mainframe, relief = "raised", borderwidth = 1)
    button_select_all = tkbutton(lowframe, text = "select all")
    button_deselect_all = tkbutton(lowframe, text = "deselect all")
    tkpack(button_select_all, pady = 6)
    tkpack(button_deselect_all, pady = 6)
    tkpack(lowframe, fill = "x")

    # adding comands to plot data button
    tkbind(button_plot_data, "<Button-1>", function(...) {

      # get current selected sample from dropbox
      sample_idx = as.integer(tcl(combobox, "current"))+1

      # catch error if no sample selected
      if (sample_idx == 0) {
        tk_messageBox(caption = Current$version, message = "Please select a sample from the list.")
      }
      sample_name = tclvalue(tkget(combobox))

      # get current selected markers
      # output of states of checkboxes
      marker_cols = c()

      # transformation of tcl states into integer vector for columns
      for (i in 1:marker_length) {
        marker_cols = c(marker_cols, as.integer(tclvalue(cb_states[[i]])))
      }
      marker_cols = which(marker_cols == 1)
      
      # retrieving number of selected markers
      num_markers = length(marker_cols)
      selected_marker_names = Current$marker_names[marker_cols,]
      print(selected_marker_names)

      # check if any markers are selected
      if (num_markers > 0) {
        
        # translate selected marker cols into database cols
        markerindex = Current$translateToDatabase(marker_cols)

        # get marker data for current selected markers and sample
        Current$specified_marker_data  = Current$getMarkerData(Current$database, Current$project, sample_idx, markerindex)

        # automatically calculate cutoffs
        thresholds = rep(0, marker_length)
        for (marker in 1:ncol(Current$specified_marker_data)) {
          thresholds[marker] = Current$calculateCutoff(Current$specified_marker_data[,marker])
          print(thresholds[marker])
        }

        # plotting Histograms
        Current$plotHistograms(Current$specified_marker_data, num_markers, selected_marker_names, thresholds, sample_name)

        # display calculated thresholds in textboxes
        x = 1
        for (selected in marker_cols) {
          tclvalue(Current$displayed_thresholds[[selected]]) = thresholds[x]
          x = x + 1
        }
        print(thresholds)

      } else {
        # catch error if no marker selected
        tk_messageBox(caption = Current$version, message = "Please select at least one Marker.")
        print("No Markers selected.")
      }


    })

    # add command to pdf button
    tkbind(button_pdf_data, "<Button-1>", function(...) {

      # getting save location and filename
      filename = tclvalue(tkgetSaveFile())
      
      # get current selected sample from dropbox
      sample_idx = as.integer(tcl(combobox, "current"))+1

      # catch error if no sample selected
      if (sample_idx == 0) {
        tk_messageBox(caption = Current$version, message = "Please select a sample from the list.")
      }
      sample_name = tclvalue(tkget(combobox))

      # get current selected markers
      # output of states of checkboxes
      marker_cols = c()

      # transformation of tcl states into integer vector for columns
      for (i in 1:marker_length) {
        marker_cols = c(marker_cols, as.integer(tclvalue(cb_states[[i]])))
      }
      marker_cols = which(marker_cols == 1)
      
      # retrieving number of selected markers
      num_markers = length(marker_cols)
      selected_marker_names = Current$marker_names[marker_cols,]
      print(selected_marker_names)

      # check if any markers are selected
      if (num_markers > 0) {
        
        # translate selected marker cols into database cols
        markerindex = Current$translateToDatabase(marker_cols)

        # get marker data for current selected markers and sample
        Current$specified_marker_data  = Current$getMarkerData(Current$database, Current$project, sample_idx, markerindex)

        # automatically calculate cutoffs
        thresholds = rep(0, marker_length)
        for (marker in 1:ncol(Current$specified_marker_data)) {
          thresholds[marker] = Current$calculateCutoff(Current$specified_marker_data[,marker])
          print(thresholds[marker])
        }

        # saving Histograms as pdf 
        pdf(filename)
        Current$plotHistograms(Current$specified_marker_data, num_markers, selected_marker_names, thresholds, sample_name)
        dev.off()
        tk_messageBox(caption = "PRI-ANA-LITE V0.3", message = paste0("Plots have been saved at ", filename))

        # display calculated thresholds in textboxes
        x = 1
        for (selected in marker_cols) {
          tclvalue(Current$displayed_thresholds[[selected]]) = thresholds[x]
          x = x + 1
        }
        print(thresholds)

      } else {
        # catch error if no marker selected
        tk_messageBox(caption = Current$version, message = "Please select at least one Marker.")
        print("No Markers selected.")
      }

    })
    
    # bind command to select all button
    tkbind(button_select_all, "<Button-1>", function(...) {
      for (i in 1:marker_length) {
        # marks tclvalue state at initiation
        tclvalue(cb_states[[i]]) = 1
    }

    })

    # bind command to deselect all button
    tkbind(button_deselect_all, "<Button-1>", function(...) {
      for (i in 1:marker_length) {
        # marks tclvalue state at initiation
        tclvalue(cb_states[[i]]) = 0
    }

    })

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

    # creating checkbox list to keep track of checkbox states
    cb_states = list()
    for (i in 1:marker_length) {
      # identifies groupings (each is distinct)
      cb_states[[i]] = i
      # marks tclvalue state at initiation
      tclvalue(cb_states[[i]]) = 0
    }
    
    # checkbox selection list
    
    if (marker_length %% 4 != 0) {
      residue = 1
    }
    quarter = floor(marker_length/4) + residue

    # global variable for displayed thresholds in textfields
    Current$displayed_thresholds = list()
    for (i in 1:marker_length) {
      Current$displayed_thresholds[[i]] = tclVar(0)
    }

    for (i in 1:quarter) {
      checkbox = tkcheckbutton(marker_frame1, variable=cb_states[[i]], text=unlist(Current$marker_names[i,]))
      cutoffentry = tkentry(marker_frame1, width=4, textvariable=Current$displayed_thresholds[[i]])
      tkpack(checkbox, tklabel(marker_frame1, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    for (j in (i+1):(i+quarter)) {
      checkbox = tkcheckbutton(marker_frame2, variable=cb_states[[j]], text=unlist(Current$marker_names[j,]))
      cutoffentry = tkentry(marker_frame2, width=4, textvariable=Current$displayed_thresholds[[j]])
      tkpack(checkbox, tklabel(marker_frame2, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    for (k in (j+1):(j+quarter)) {
      checkbox = tkcheckbutton(marker_frame3, variable=cb_states[[k]], text=unlist(Current$marker_names[k,]))
      cutoffentry = tkentry(marker_frame3, width=4, textvariable=Current$displayed_thresholds[[k]])
      tkpack(checkbox, tklabel(marker_frame3, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    for (l in (k+1):marker_length) {
      checkbox = tkcheckbutton(marker_frame4, variable=cb_states[[l]], text=unlist(Current$marker_names[l,]))
      cutoffentry = tkentry(marker_frame4, width=4, textvariable=Current$displayed_thresholds[[l]])
      tkpack(checkbox, tklabel(marker_frame4, text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
    tkpack(marker_frame1, side = "left", padx = 5)
    tkpack(marker_frame2,  side = "left", padx = 5)
    tkpack(marker_frame3, side = "left",  padx = 5)
    tkpack(marker_frame4,  side = "left", padx = 5)
    tkpack(marker_frames, expand = TRUE, side = "top")
    
    }
}

# secondary GUI functions
Main$GUIquit <- function() {
  print("do: GUIquit")
  Current = Main

  tkdestroy(Current$mainframe)
}

Main$GUIback <- function() {
  print("do: GUIback")
  Current = Main

  Current$preselection = TRUE
  tkdestroy(Current$mainframe)
  Current$GUImain()
}

Main$GUIselectdb <- function() {
  print("do: GUIselectdb")
  Current = Main

  last_database = Current$database
  Current$database = tclvalue(tkgetOpenFile(initialdir = file.path("","data","databases")))
  
  # error catch if no db selected
  if (Current$database == "") {
    Current$database = last_database
  }
}
