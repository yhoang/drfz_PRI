#!/usr/bin/R
# Author: Yen Hoang
# DRFZ 2015-2020



### check and manipulate user inputs -------------------------------------------

### Check if the ranges of X and Y axes are the same
fcs$checkAxesRange  <-  function(){
  this <- fcs
  
  xmin.val <- as.numeric(tkget(this$minvalX))
  xmax.val <- as.numeric(tkget(this$maxvalX))
  ymin.val <- as.numeric(tkget(this$minvalY))
  ymax.val <- as.numeric(tkget(this$maxvalY))
  
  if (round(diff(c(xmin.val, xmax.val)), 1) == round(diff(c(ymin.val, ymax.val)), 1)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

### Check entered numbers; convert comma and rounding
## e.g. 3, 535 to 3.5
# num         number,  default= NA
# cutoff_id   if this is a cutoff,  round to -1,  default=NA
fcs$checkDigits  <-  function(num=NA, cutoff_id=NA){
  this <- fcs 
  
  if (!is.na(cutoff_id)){
    num <- tclvalue(this$vcutoffs[[cutoff_id[1]]])
  }
  
  ### only accepts numbers with points
  # deletes characters
  num <- gsub("[a-z]", "", num)
  # converts comma to point
  num <- gsub(", ", ".", num)
  # deletes starting zeros
  num <- gsub("^0+", "", num)
  
  ### if num is empty at the end,  return 0
  if (num == "" | is.na(num))num <- 0
  
  ### changes value in cutoff array
  if (!is.na(cutoff_id))tclvalue(this$vcutoffs[[cutoff_id[1]]]) <- num
  
  num <- round(as.numeric(num), 1)
  
  ### changes number in GUI
  if (!is.na(cutoff_id)){
    this$selected.cutoffs[cutoff_id[1]] <- num
    tclvalue(this$vcutoffs[[cutoff_id[1]]]) <- as.character(num)
  }
  ### returns number
  num
}

fcs$checkMarker  <-  function(var){
  this <- fcs
  
  printf("w: do checkMarker: var=%s", var)
  
  if (!any(var == this$selected.vars)){
    
    # removes all punctuations for comparison
    selected.vars <- gsub("[^[:alnum:]]", "", toupper(this$selected.vars))
    var.tmp <- gsub("[^[:alnum:]]", "", toupper(var))
    
    var <- this$selected.vars[which(selected.vars == var.tmp)]
    # get the var where name stops at the end
    if (length(var) == 0)var <- this$selected.vars[grep(sprintf("%s$", var.tmp), selected.vars)]
    # if there is no,  get first name which is similar
    if (length(var) == 0)var <- this$selected.vars[grep(var.tmp, selected.vars)][1]
  }
  var.idx <- which(this$selected.vars == var)
  
  if (length(var) == 0)print("w: Done checkMarker: NO feature found!")
  else printf("w: Done checkMarker: var #%s=%s", var.idx, var)
  
  var
}

### Check if there are dublicate markers
fcs$checkDoubleMarkers  <-  function(){
  this <- fcs
  
  len <- length(this$selected.vars)
  
  printf("w: do checkDoubleMarkers: vars(%s)=%s", len, paste(this$selected.vars, collapse=" "))
  
  vars.unique <- unique(this$selected.vars)
  len.unique <- length(vars.unique)
  
  if (len != len.unique){
    
    vars <- make.unique(this$selected.vars)
    
    printf("w: Done checkDoubleMarkers: double feature names removed!")
  }else {
    printf("w: Done checkDoubleMarkers: no double features.")
  }
  
  vars
}

### Shortening file name
# name    file name
# title   TRUE/FALSE,  different shortening for title and file name
fcs$shortenFilename  <-  function(name, title=FALSE){
  name <- sub(".csv$", "", name)
  name <- sub(".fcs$", "", name)
  name <- sub("^temp\\d\\d_", "", name)
  name <- sub("^export_", "", name)
  name <- sub("^(\\w\\w)_*", "\\1\\2", name)
  name <- sub("Stain_*|stain_|Stain*|stain*", "S\\1", name)
  name <- sub("Panel|panel|Panel*|panel*", "P\\1", name)
  name <- sub("Patient*|patient*|Patient_*|patient_*", "Pat\\1", name)
  name <- sub("Surface*|surface*|Surface_*|surface_*", "Surf\\1", name)
  name <- sub("Stimulated|stimulated", "Stim", name)
  name <- sub("Gruppe|gruppe|group", "Grp", name)
  name <- sub("Day|day", "d", name)
  name <- sub("Antibodies|Antibody|antibodies|antibody", "Ab", name)
  name <- sub("Untreated|untreated|untreat", "untr", name)
  name <- sub("PMA_Iono|PMA_iono|PMAIono|PMAiono|pmaiono|pmaIono|pma_iono", "P+I", name)
  name <- sub("(\\dh)_*", "\\1", name)
  
  name <- sub("(\\d{4})_(\\d{2})_(\\d{2})", "\\1\\2\\3", name, perl=TRUE)
  
  if (!title){
    ### if file name starts with 0-2 letters and follows with 6-8 digits
    # (short or long version of date)YY/MM/DD or YYYY/MM/DD)
    # then print only with last 3 digits
    name <- sub("^\\w\\w\\d{3, 5}(\\d)(\\d)(\\d)_", "\\1\\2\\3_", name, perl=TRUE)
  }
  
  name
}

### Shortens marker names
## take only first part of name if there is a "." in the name
## and capitalize it
## NEW VERSION allows dots in name
# markerNames     marker name
fcs$shortenMarkername  <-  function(markerNames){
  unlist(lapply(markerNames, function(x){
    toupper(strsplit(x, "[.]")[[1]][1])
  }))
  
  unlist(lapply(markerNames, function(x){
    len <- length(strsplit(x, "[.]")[[1]])
    y <- toupper(strsplit(x, "[.]")[[1]][1:(len - 1)])
    paste(y,  collapse=".")
  }))
}