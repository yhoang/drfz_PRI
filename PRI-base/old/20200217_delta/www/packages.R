# App packages
if (FALSE) {
library(shiny, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(V8, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(shinyBS, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(RSQLite, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(rChoiceDialogs, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(rJava,quietly=TRUE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(flowCore, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
# For digest(). Calculates unique ID for files.
library(digest, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
# To bind dataframes with unequal columns.
library(gtools, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
# To edit character vectors.
library(stringr,lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(cytobank, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
library(RISmed, quietly = TRUE, warn.conflicts = FALSE, lib.loc = file.path("Y:","AG_Baumgrass","Alexander.Rybak","win-library","3.4"))
}


library(shiny, quietly = TRUE, warn.conflicts = FALSE)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE)
library(V8, quietly = TRUE, warn.conflicts = FALSE)
library(shinyBS, quietly = TRUE, warn.conflicts = FALSE)
library(RSQLite, quietly = TRUE, warn.conflicts = FALSE)
library(rChoiceDialogs, quietly = TRUE, warn.conflicts = FALSE)
library(xlsx, quietly = TRUE, warn.conflicts = FALSE)
library(flowCore, quietly = TRUE, warn.conflicts = FALSE)
# For digest(). Calculates unique ID for files.
library(digest, quietly = TRUE, warn.conflicts = FALSE)
# To bind dataframes with unequal columns.
library(gtools, quietly = TRUE, warn.conflicts = FALSE)
library(RISmed)
library(cytobank)
library(stringr)

#source("http://bioconductor.org/biocLite.R")
#biocLite("shiny") 
#biocLite("shinydashboard") 
#biocLite("shinyjs") 
#biocLite("V8") 
#biocLite("shinyBS") 
#biocLite("rChoiceDialogs") 
#biocLite("RSQLite") 
#biocLite("flowCore") 
#biocLite("digest") 
#biocLite("gtools") 
#biocLite("stringr")
#biocLite("RISMed")
#install.packages("/data/databases/shiny/App20170929/R-packages/cytobank_1.3/cytobank/",repos = NULL, type = "source")
