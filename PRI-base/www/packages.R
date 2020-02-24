# App packages

# Database
library(RSQLite, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
library(shiny, lib = Lib.path)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
library(V8, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
library(shinyBS, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
library(rChoiceDialogs, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
library(rJava,quietly=TRUE, lib = Lib.path)
library(flowCore, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
# For digest(). Calculates unique ID for files.
library(digest, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
# To bind dataframes with unequal columns.
library(gtools, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)
# To edit character vectors.
library(stringr, lib = Lib.path)
library(RISmed, quietly = TRUE, warn.conflicts = FALSE, lib = Lib.path)

# private packages
library(cytobank, lib = Lib.path)
# library(needfulFuns, lib = Lib.path)
