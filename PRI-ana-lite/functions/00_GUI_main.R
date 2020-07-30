#!/usr/bin/R
# Author: Yen Hoang and Felix Lohrke
# Date: 2020

# ---------- # GUI base function # ---------- #
Main$GUImain <- function() {

    Current = Main

    # subfunction
    #GUIquit = function(...)tkdestroy(mainframe)

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

    #tkpack(Current$mainframe)

    # create subframe
    Current$subframe = tkframe(Current$mainframe)

    ## combobox to choose files

    # get filenames for table and combobox
    Current$current.fileid_name = "_fileIndex"
    Current$total.projects <- dbListTables(Current$conn)
    Current$current.project <- Current$total.projects[1]
    Current$current.filetable <- Current$getDFtable(paste0(Current$current.project, Current$fileid_name))
    Current$current.filenames <- Current$current.filetable[, 2]


    comboboxframe = tkframe(Current$subframe)
    Current$comboboxfiles = ttkcombobox(comboboxframe, values = "", width = 40)
    ## add filenames from database
    ## add select filename functionality
    ## add refresh after select
    tkgrid(tklabel(comboboxframe, text = "File: "), Current$comboboxfiles, sticky = "w")
    tkgrid(comboboxframe, pady = 10)
    tkgrid(Current$subframe, pady = 1)


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
