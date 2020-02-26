# App packages
PRIlib = "/opt/R-3.5.1/library"

library(shiny, lib = PRIlib)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
library(V8, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
library(shinyBS, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
library(RSQLite, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
library(rChoiceDialogs, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
library(rJava,quietly=TRUE, lib = PRIlib)
library(flowCore, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
# For digest(). Calculates unique ID for files.
library(digest, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
# To bind dataframes with unequal columns.
library(gtools, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)
# To edit character vectors.
library(stringr, lib = PRIlib)
library(RISmed, quietly = TRUE, warn.conflicts = FALSE, lib = PRIlib)

# private packages
library(cytobank, lib = PRIlib)
# library(needfulFuns, lib = PRIlib)
