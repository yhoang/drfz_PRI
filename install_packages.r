#usr/bin/R
### DRFZ
Lib.path = "Y:/AG_Baumgrass/AG-PRI/R/R-3.6.1/library"
### ASUS Zenbook
Lib.path = "/opt/R-3.5.1/library"
### DELTA
Lib.path = "/usr/lib/R/site-library"

###  PRI-ana
install.packages("RSQLite",lib = Lib.path)
install.packages("tcltk2",lib = Lib.path)
install.packages("R.devices",lib = Lib.path)

### PRI-base public packages -------------------------------------------------------------
# SHINY NEEDS TO BE VERSION 1.3.2 TO WORK!!!
packageurl="https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.3.2.tar.gz"
install.packages(packageurl, dependencies=T, repos=NULL, type="source",lib = Lib.path)

# these are not available on CRAN
BiocManager::install("flowCore",lib = Lib.path)
BiocManager::install("FlowRepositoryR",lib = Lib.path)
BiocManager::install(c("devtools","XML","rvest"),lib = Lib.path)

install.packages(c("shinydashboard","shinyjs","V8","shinyBS","rJava","rvest",
                   "XML","xml2","RISmed","rChoiceDialogs","digest","gtools","stringr",
                   "httr","devtools","roxygen2","DT","xlsx"),lib = Lib.path)

### PRI-base private packages -------------------------------------------------------------
install.packages("Y:/AG_Baumgrass/AG-PRI/R/private_packages/needfulFuns_1.3/needfulFuns/",repos=NULL,type="source",lib = Lib.path)
install.packages("Y:/AG_Baumgrass/AG-PRI/R/private_packages/cytobank_1.3/cytobank/",repos=NULL,type="source",lib = Lib.path)

