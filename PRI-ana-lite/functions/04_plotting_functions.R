#!/usr/bin/R
# Author: Felix Lohrke and Yen Hoang
# DRFZ 2020

# ---------- # Functions for plotting # ---------- #

# plotting Histograms from marker data with thresholds

Main$plotHistograms <- function(marker_data, num_markers, marker_names, thresholds) {
    
    Current = Main
    
    ### handling of variable number of plotting histograms
	dev.label = "histogram.autogate"
	if (length(which(dev.label==names(devList()))) != 0 ) {
		devSet(devList()[which(dev.label==names(devList()))])
	} else {
		if ( num_markers > 36 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=7*2.8,height=ceiling(num_markers/7)*2.0,label=dev.label)
			par(mfrow=c(ceiling(num_markers/7),7),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else if ( num_markers>25 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=6*2.5,height=ceiling(num_markers/6)*2.0,label=dev.label)
			par(mfrow=c(ceiling(num_markers/6),6),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else if ( num_markers>16 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=5*3,height=ceiling(num_markers/5)*2.3,label=dev.label)
			par(mfrow=c(ceiling(num_markers/5),5),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else if ( num_markers>9 ) {
			devNew(type="x11",title="Histograms for cutoffs",width=4*4,height=ceiling(num_markers/4)*2.5,label=dev.label)
			par(mfrow=c(ceiling(num_markers/4),4),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		} else {
			devNew(type="x11",title="Histograms for cutoffs",width=3*4,height=ceiling(num_markers/3)*3,label=dev.label)
			par(mfrow=c(ceiling(num_markers/3),3),oma=c(1.5,1,1.5,1),mar=c(2,2.4,1.5,1),mgp=c(1.3,0.4,0))
		}
	
	}

    # actual plotting
    for (marker in 1:num_markers) {
        density = density(marker_data[, marker])
        x = density$x
        y = density$y
        start_y = (1/10)*max(y)
        start_x = (1/10)*max(x)
        id_y = which(y > start_y)
        id_x = which(x > start_x)
        xmin = x[intersect(id_y, id_x)[1]-5]
        xmax = round(max(marker_data[,marker]))

        # check for infity or NA
        if (is.infinite(xmin) | is.na(xmin)) {
            xmin = 0
        }
        if (is.infinite(xmax) | is.na(xmax)) {
            xmax = 8
        }

        hist(marker_data[, marker], freq=F, 
        breaks=50, col = "palegreen2", main = marker_names[marker],
        xlab = "", lwd = 0.5)
        lines(density(marker_data[, marker]), lwd = 2, col = "brown")

        # vertical lines
        #for (i in ceiling(xmin):floor(xmax)) {
        #    abline(v = i, col="darkgrey")
        #}
        if (thresholds[marker] != 0) {
            abline(v = thresholds[marker], lty = 2, lwd = 2)
        }
    }

   
}