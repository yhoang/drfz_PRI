########################################################################################
####   global.R contains variables which ui.R and server.R can access               ####
########################################################################################

# check if environment exists-----------------------------------------------------------
# if not create it (useful when PRIbase is reloaded)
  if(!exists("account")){
    account=new.env()
  }
#
# check community account data----------------------------------------------------------
# flag that reports login status to cytobank. TRUE when logged in
  if(!exists("com.log",where=account)){
    account$com.log=F
  }
  account$com.url="https://community.cytobank.org/cytobank"
#
# check premium account data------------------------------------------------------------
#
  if(!exists("pre.log",where=account)){
    account$pre.log=F
  }
  account$pre.url="https://premium.cytobank.org/cytobank"
#
# load FlowRepository experiment table -------------------------------------------------
  
  account$flowRep_tables=needfulFuns::get.tables("www/FlowRep_Experiments.sqlite3")
#
#---------------------------------------------------------------------------------------