# App packages
PRIlib = "/opt/R-3.5.1/library"

library(shiny, quietly = TRUE, warn.conflicts = FALSE,lib.loc = PRIlib)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(V8, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(shinyBS, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(RSQLite, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(rChoiceDialogs, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(xlsx, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(flowCore, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
# For digest(). Calculates unique ID for files.
library(digest, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
# To bind dataframes with unequal columns.
library(gtools, quietly = TRUE, warn.conflicts = FALSE, lib.loc = PRIlib)
library(RISmed, lib.loc = PRIlib)
library(cytobank, lib.loc = PRIlib)
library(stringr, lib.loc = PRIlib)

# install.packages(c("flowCore"),dependencies = T,lib=PRIlib)
# install.packages(c("devtools","shiny"),dependencies = T, lib=PRIlib)
# install.packages(c("shinydashboard","shinyjs","V8","shinyBS","rChoiceDialogs","digest","gtools","stringr","RISMed"),dependencies = T, lib=PRIlib)
