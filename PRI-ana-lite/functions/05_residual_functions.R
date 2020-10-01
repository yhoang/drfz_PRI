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

        # translation of iteger vector into column names for marker selection
        markerindex = list()
        for (i in 1:length(marker_cols)) {
          markerindex[[i]] = paste0("col", marker_cols[i])
        }
        markerindex = paste(unlist(markerindex), collapse=", ")
    
        return(markerindex)

}