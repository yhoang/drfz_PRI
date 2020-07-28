#!/usr/bin/R
# Author: Felix Lohrke
# Date: 2020

# GUI function
pal$GUImain <- function() {

    palnow = pal

    # subfunction
    #GUIquit = function(...)tkdestroy(mainframe)

    # creating base and adding main title
    palnow$mainframe = tktoplevel()
    tkwm.title(palnow$mainframe, "PRI-ANA-LITE")

    # create subframe
    subframe = tkframe(palnow$mainframe)

    # create entry values
    test_ = tclVar("")

    # create frame with entryfield and button
    fentry = tkframe(subframe)
    tkpack(tklabel(fentry, text = "test"), side = "left")
    tkpack(tkentry(fentry, textvariable = test_), side = "left")
    tkpack(tkbutton(fentry, text = "ok", command = palnow$GUIquit), side = "bottom")

    tkpack(fentry, side="top")

    # create frame with quit
    fquit = tkframe(palnow$mainframe)
    tkpack(tkbutton(fentry, text = "Quit", command = palnow$GUIquit), side = "bottom")
    
    # packing
    
    tkpack(fquit, side="bottom")
    tkpack(subframe)
}

pal$GUIquit <- function() {
    palnow = pal
    tkdestroy(palnow$mainframe)
}
