#!/usr/bin/R
# Author: Felix Lohrke and Yen Hoang
# DRFZ 2020

# ---------- # Functions without common purpose # ---------- #

# function that translates input of selected marker columns into markerindex
# for database interaction
Main$translateToDatabase <- function(marker_cols) {
    
    print("do: translateToDatabase")
    # go over every marker column number of selected markers
    # zero padding for nums under 10
    
    for (i in 1:length(marker_cols)) {

        if (marker_cols[i] %in% c(1,2,3,4,5,6,7,8,9)) {
            marker_cols[i] = sprintf("%02d", as.integer(marker_cols[i]))
        } else {
            marker_cols[i] = toString(marker_cols[i])
          }
        }

        # translation of integer vector into column names for marker selection
        markerindex = list()
        for (i in 1:length(marker_cols)) {
          markerindex[[i]] = paste0("col", marker_cols[i])
        }
        markerindex = paste(unlist(markerindex), collapse=", ")
    
        return(markerindex)

}

# function for dynamic marker checkbox creation - NOT WORKING AS FUNCTION 
# has been implemented in GUI_main directly instead
Main$createMarkerBoxes <- function(marker_length) {

  print("do: createMarkerBoxes")

  # markers
  markers = c(1:marker_length)

  # calculate num of fields so that max 8 marker per field
  number_fields = ceiling(marker_length/8)

  # splitting the markers with leftover
  groups = split(markers, rep(1:number_fields, each=8, length.out = length(markers)))

  # global variable for displayed thresholds in textfields
  Current$displayed_thresholds = list()

  # creating 4 fields adjusted to number of markers
  for (i in 1:marker_length) {
    Current$displayed_thresholds[[i]] = tclVar(0)
  }

  # main labeframe
  marker_frames = ttklabelframe(Current$mainframe, text = "Select Markers: ")
  all_frames = list()

  # dynamic marker field creation
  for (group in 1:length(groups)) {
    all_frames[[group]] = tkframe(marker_frames, relief = "raised", borderwidth = 1)
    #assign(paste0("marker_frame", group), temp)

    for (marker in 1:length(groups[[group]])) {
      checkbox = tkcheckbutton(all_frames[[group]], variable=cb_states[[marker]], text=unlist(Current$marker_names[marker,]))
      cutoffentry = tkentry(all_frames[[group]], width=4, textvariable=Current$displayed_thresholds[[marker]])
      tkpack(checkbox, tklabel(all_frames[[group]], text="cutoff: "), cutoffentry, padx=2, pady=1, expand = TRUE, fill = "both")
    }
  }

  for (frames in 1:length(all_frames)) {
    tkpack(all_frames[[frames]], side = "left", padx = 5)
  }
  tkpack(marker_frames, expand = TRUE, side = "top")


}