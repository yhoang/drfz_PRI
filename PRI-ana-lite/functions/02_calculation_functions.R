#!/usr/bin/R
# Author: Felix Lohrke and Yen Hoang
# DRFZ 2020

# ---------- # Calculation functions # ---------- #
# 3 scenarios
# i) 

# automatically calculate cutoff for a marker depending on marker value behavior over time
# (trimming and doublets?)
Main$calculateCutoff <- function(marker_values) {
    #Current = Main
    print("do: calculateCutoff")
    # calulate density values of marker values
    # retrtieve points at which density is calculated (x) + denisty values (y)
    density = density(marker_values)
    x = density$x
    y = density$y
    cutoff_found = ""

    # only higher level of denisty values is observed
    # shouldnt x be selected via valid y values???
    start_y = (1/10)*max(y)
    id_y = which(y > start_y)
    start_x = (1/10)*max(x)
    id_x = which(x > start_x)

    # move 5 positions earlier than start
    x_min = x[intersect(id_y, id_x)[1]-5]
    x_max = round(max(marker_values))

    # create list for multiple found cutoffs per marker
    cutoff_list = list()

    # start of steps through density curve
    stepsize = 6:3
    for (step in 1:length(stepsize)) {

        # incline list
        # [1] step id
        # [2] x-value from step
        # [3] incline difference in range
        # x_incline = x-value from step (unrounded)
        incline_list = list()
        incline_x = c()
        cutoff = 0
        flag = FALSE
        idx = 1
        
        # moving through y and making sure steps dont go out of bounds
        for (range in 1:(length(y) - stepsize[step])) {
            y_min_range = min(y[range:(range + stepsize[step])])

            # difference between current position and end of step
            diff = diff(c(y[range], y[range + stepsize[step]]))

            # if min in range y is more than 1/10th of peak and x > 2.5
            # or if min in range y is more than 1/10th of peak and x > start_x
            # if condition found true at least once then flag = TRUE which
            # skips other steps
            if ((y_min_range > start_y) & (x[range] > 2.5)) {
                incline_list[[idx]] = range
                incline_list[[idx]][2] = round(x[range], 3)
                incline_list[[idx]][3] = diff
                incline_x[idx] = x[range]
                idx = idx + 1

            }
        }

        if (idx == 1) {
            for (range in 1:(length(y) - stepsize[step])) {
                y_min_range = min(y[range:(range + stepsize[step])])

                # difference between current position and end of step
                diff = diff(c(y[range], y[range + stepsize[step]]))

                # if min in range y is more than 1/10th of peak and x > 2.5
                # or if min in range y is more than 1/10th of peak and x > start_x
                # if condition found true at least once then flag = TRUE which
                # skips other steps
                if ((y_min_range > start_y) & (x[range] > start_x)) {
                    incline_list[[idx]] = round(range, 2)
                    incline_list[[idx]][2] = round(x[range], 3)
                    incline_list[[idx]][3] = diff
                    incline_x[idx] = x[range]
                    idx = idx + 1

                }
            }
        }

        # dunno about this one
        if (idx == 1) {
            flag = TRUE
        }

        # if none of the two conditions to create incline list have fired
        # look for minima/maxima and shoulder in steps
        if (flag != TRUE) {

            idx_minima = idx_maxima = idx_shoulder = c()

            for (inc in 1:(length(incline_x)-1)) {
               
               # check for local minima
               if (incline_list[[inc]][3] < 0 & incline_list[[inc+1]][3] > 0) {
                   idx_minima = c(idx_minima, inc)
               }

               # check for local maxima
               if (incline_list[[inc]][3] > 0 & incline_list[[inc+1]][3] < 0) {
                   idx_maxima = c(idx_maxima, inc)
               }

               # check for local shoulder
               if (abs(incline_list[[inc]][3]) < 0.01) {
                   idx_shoulder = c(idx_shoulder, inc)
               }
            }
            
            # remove all maxima indices from minima indices or near
            remove = vector()
            double_minmax = intersect(idx_minima, idx_maxima)
            if (length(double_minmax) > 0) {
                for (i in (double_minmax-3):(double_minmax+3)) {
                    remove = c(remove, which(idx_minima == i))
                }   
                idx_minima = idx_minima[-remove]
            }

            # remove shoulder idices near maxima +- 0.3
            #remove = vector() # intentional that she doesnt re-defines it?
            minus_shoulder = intersect(idx_maxima, idx_shoulder)
            if (length(minus_shoulder) > 0) {
                x_shoulder_max = incline_x[minus_shoulder]

                for (i in idx_shoulder) {

                    if (abs(x_shoulder_max - incline_x[i]) <= 0.3) {
                        remove = c(remove, which(idx_shoulder == i))

                    }
                }
                idx_shoulder = idx_shoulder[-remove]

            }

            # cutoff calculation

            # if there is a d to modularize since main script is in command def for buttonminima
            if (length(idx_minima) > 0) {
                tmp = round(length(idx_minima)/2)
                if (tmp == 0) {
                    tmp = idx_minima[1]    
                } else {
                   tmp = idx_minima[round(length(idx_minima)/2)]
                }
                cutoff_found = "minima"
                cutoff = as.numeric(incline_list[[tmp]][2])

            # if there is a shoulder
            # cutoff is either first shoulder or if divisible by 2
            # median of all shoulders
            } else if (length(idx_shoulder) > 0) {
                tmp = round(length(idx_shoulder)/2)
                if (tmp == 0) {
                    tmp = idx_shoulder[1]    
                } else {
                   tmp = idx_shoulder[round(length(idx_shoulder)/2)]
                }
                cutoff_found = "shoulder"
                cutoff = as.numeric(incline_list[[tmp]][2])

            # if neither minima or shoulder we use 20% quantile
            } else {
                cutoff = Main$calcCutoffQuantile(marker_values, 20)
            }

        }
        if (step == 1) {
           cutoff_list[[1]] = cutoff 
        } else {
           cutoff_list[[1]][step] = cutoff
        }

    }

    # get cutoff from median of found cutoffs
    print(cutoff_list)
    cutoff = round(median(cutoff_list[[1]]), 1)
    
    # display curve characteristic of cutoff
    print(cutoff_found)
    return(cutoff)

}
# function that takes marker values and returns cutoff based on % quantile
Main$calcCutoffQuantile <- function(marker_values, quantile = 20) {

    print("do: calcCutoffQuantile")

    marker_values = as.matrix(marker_values)

    # calculation of % quantile as cutoff (generally 20 %)
    marker_values = sort(marker_values, decreasing = TRUE)
    cutoff = round(marker_values[length(marker_values)*(quantile/100)], 3)

    return(cutoff)

}