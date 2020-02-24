#!/usr/bin/r

# Tree Widget
tclRequire("BWidget")
# Image Package
tcl("lappend", "auto_path","../tcl/lib")
tcl("lappend", "auto_path","./tcl/lib")
tclRequire("tkpng")
#tcl("lappend", "auto_path","tcl/lib")
#tcl("package","require","tkpng")


#::Tree::bindArea
#::Tree::bindImage
#::Tree::bindText
#::Tree::cget
#::Tree::closetree
#::Tree::configure
#::Tree::create
#::Tree::delete
#::Tree::edit
#::Tree::exists
#::Tree::find
#::Tree::getcanvas
#::Tree::index
#::Tree::insert
#::Tree::itemcget
#::Tree::itemconfigure
#::Tree::line
#::Tree::move
#::Tree::nodes
#::Tree::opentree
#::Tree::parent
#::Tree::reorder
#::Tree::see
#::Tree::selection
#::Tree::toggle
#::Tree::use
#::Tree::visible
#::Tree::visiblenodes
#::Tree::xview
#::Tree::yview

bwidget.bitmap.getfile = function (image) { 
    return(paste(tcl("set", "BWIDGET::LIBRARY"),"/images/",image,".gif",sep="")) 
}

# images used for nodes
imageOpen=tclVar()
imageClose=tclVar()
imageFile=tclVar()
imageMatrix=tclVar()
imageBook=tclVar()
imageBookOpen=tclVar()
imageFunction=tclVar()

tkimage.create("photo", imageOpen, file = bwidget.bitmap.getfile("open")) 
tkimage.create("photo", imageClose, file = bwidget.bitmap.getfile("folder")) 
tkimage.create("photo", imageFile, file = bwidget.bitmap.getfile("file")) 
tkimage.create("photo",imageMatrix,data="
  R0lGODlhEAAQAIUAAPwCBHx+fHRydPT69DxqhBxihBxunBx2pBx6nByCrPTy
  7NTm7Mze5OTq7GxqbKSelLSyrKyqpLSupGRiZOzu3Ozq3FxaXOzezBxyjBxe
  jOzm1ExOTERGROzavOTStDQ2NOTOrDw+POTGnCQqLAAAAAAAAAAAAAAAAAAA
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaC
  QIBwSCwKA8ikMikUDJ7QKDQAcBIKhqzhgEgcEgpBdaBYMM5oxqIxEDsUD0hE
  HolI6hGFAzChPCp/gYCDFgAWFxgZiRgFi40ahYcZCZOVlJQXGwAcHYuKn4se
  mhweD6WnpqkeHAAfIA+wsbIPICGtIri5urkfQiO/wMHARsTFxkR+QQAh/mhD
  cmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENv
  ciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3
  LmRldmVsY29yLmNvbQA7")
tkimage.create("photo",imageBook,data="
  R0lGODlhEAAQAIQAAPwCBAQCBDyKhDSChGSinFSWlEySjCx+fHSqrGSipESO
  jCR6dKTGxISytIy6vFSalBxydAQeHHyurAxubARmZCR+fBx2dDyKjPz+/MzK
  zLTS1IyOjAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVkICCOZGmK
  QXCWqTCoa0oUxnDAZIrsSaEMCxwgwGggHI3E47eA4AKRogQxcy0mFFhgEW3M
  CoOKBZsdUrhFxSUMyT7P3bAlhcnk4BoHvb4RBuABGHwpJn+BGX1CLAGJKzmK
  jpF+IQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0K
  qSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpo
  dHRwOi8vd3d3LmRldmVsY29yLmNvbQA7")
tkimage.create("photo",imageBookOpen,data="
  R0lGODlhEAAQAIUAAPwCBAQCBExCNGSenHRmVCwqJPTq1GxeTHRqXPz+/Dwy
  JPTq3Ny+lOzexPzy5HRuVFSWlNzClPTexIR2ZOzevPz29AxqbPz6/IR+ZDyK
  jPTy5IyCZPz27ESOjJySfDSGhPTm1PTizJSKdDSChNzWxMS2nIR6ZKyijNzO
  rOzWtIx+bLSifNTGrMy6lIx+ZCRWRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAae
  QEAAQCwWBYJiYEAoGAFIw0E5QCScAIVikUgQqNargtFwdB9KSDhxiEjMiUlg
  HlB3E48IpdKdLCxzEAQJFxUTblwJGH9zGQgVGhUbbhxdG4wBHQQaCwaTb10e
  mB8EBiAhInp8CSKYIw8kDRSfDiUmJ4xCIxMoKSoRJRMrJyy5uhMtLisTLCQk
  C8bHGBMj1daARgEjLyN03kPZc09FfkEAIf5oQ3JlYXRlZCBieSBCTVBUb0dJ
  RiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwg
  cmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==")
tkimage.create("photo",imageFunction,data="
  R0lGODlhEAAQAIMAAPwCBAQCBMT+xAT+BASCBMT+/ATCBAT+/ARCRPz+xARC
  BPz+BATCxISCBDQyNMTCBCH5BAEAAAAALAAAAAAQABAAAARhEMgZwrwYBCFs
  vhs3eF8wDMJAVBVmnupazKRmGDFxzMVBBjcDQXfYHRA/QmKpKBYRSMoysVgw
  GEeoJ1ClLhpXhlbiqJobjcA1Sn48qug06+JwP+I1UMCNzmcqaR8lghN+EQAh
  /mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZl
  bENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8v
  d3d3LmRldmVsY29yLmNvbQA7")


tkbindarea = function (widget,...) { 
    tcl(widget,"bindArea", ...) 
}
tkbindimage = function (widget,...) { 
    tcl(widget,"bindImage", ...) 
}
tkbindtext = function (widget,...) { 
    tcl(widget,"bindText", ...) 
}
tkchildren = function(widget,...) {
	tcl(widget,"children",...)
}
tkclosetree = function (widget,...) { 
    tcl(widget,"closetree", ...) 
}
tkedit = function (widget,...) { 
    tcl(widget,"edit", ...) 
}
tkgetframe = function(widget,...) {
    # problem if called a second time for the same widget
    ID=tclvalue(tcl(widget,"getframe",...))
        if(exists("getframewin",envir=widget$env)) {
          print("second call")
          win=widget$env$getframewin
        } else {
          #print("first call")
          win=.Tk.newwin(ID)
          assign(ID,win,envir=widget$env)
          assign('parent',widget,envir=win$env)
           widget$env$getframewin=win
       }
    return(win)
}
tksetwidget = function(widget,...) {
    tcl(widget,"setwidget",...)
}
tkexists = function (widget,...) { 
    tcl(widget,"exists", ...) 
}
tkgetcanvas = function (widget,...) { 
    tcl(widget,"getcanvas", ...) 
}
tkline = function (widget,...) { 
    tcl(widget,"line", ...) 
}
tknodes = function (widget, ...) {
    return(tcl(widget,"nodes",...));
}
tkopentree = function (widget,...) { 
    tcl(widget,"opentree", ...) 
}
tkparent = function (widget,...) { 
    tcl(widget,"parent", ...) 
}
tkreorder = function (widget,...) { 
    tcl(widget,"reorder", ...) 
}
tkselection = function (widget,...) { 
    tcl(widget,"selection", ...) 
}
tkuse = function (widget,...) { 
    tcl(widget,"use", ...) 
}
tkvisible = function (widget,...) { 
    tcl(widget,"visible", ...) 
}
tkvisiblenodes = function (widget,...) { 
    tcl(widget,"visiblenodes", ...) 
}

### ttk treewidget
ttktag = function(widget,...) {
	tcl(widget,"tag",...)
}
