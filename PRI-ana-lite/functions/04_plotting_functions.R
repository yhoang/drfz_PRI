#!/usr/bin/R
# Author: Yen Hoang and Felix Lohrke
# DRFZ 2020

# ---------- # Functions for plotting # ---------- #

# plotting a Histogram from marker data with threshold

Main$plotHistograms <- function(marker_data, num_markers, marker_names, threshold = 0) {
    Current = Main
    par(mfrow=c(1,num_markers))
    for (marker in 1:num_markers) {
        hist(marker_data[, marker], freq=F, 
        breaks=50, col = "palegreen2", main = marker_names[marker],
        xlab = "", lwd = 0.5)
        lines(density(marker_data[, marker]), lwd = 2, col = "brown")
        if (threshold != 0) {
            abline(v = threshold, lty = 2, lwd = 2)
        }
    }

   
}