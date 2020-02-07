#' Plot Density Histograms and set cutoffs with mouse
#' @rdname setCutoff
#' @aliases setCutoff
#'
#' @details \code{Plots density histograms and saves cutoffs in a dataframe}.
#' @examples \donttest{# setCutof(expressionData)
#' }
#' @export

setCutoff=function(exprData,                                # data.frame containing expression data
                   dir=getwd(),                             # location to store PDF files
                   style="h",                               # specify type of plot (?plot.default)
                   color="steelblue3",                      # set color of plot
                   ...){                                    # set additional plot parameters
  location=getwd(); setwd(dir)                              # set working directory to store PDFs
  cutoffs=data.frame(markers=colnames(exprData),            # create data.frame (markers & cutoff)
                     cutoff=0)
  windows()                                                 # open plot device
  par(mfrow=c(3,3))                                         # 3 x 3 plots on one page
  for(i in 1:ncol(exprData)){
    d=density(exprData[,i])                                 # calculate density
    plot.default(d,                                         # plot histogram
                 main=cutoffs$markers[i],                   # set title
                 xlab="",ylab="density",                    # set axis labels
                 type=style,                                # set type
                 col=color,                                 # and colour
                 ...)                                       # placeholder for additional parameters
    grid(NULL,NA,lwd=0.5,col="gray40")                      # place grid over plot
    identified=identify(d,n=1,plot=F)                       # identify X-axis position when clicked in plot
    if(length(identified)==0) {next; print("Cutoff is 0")}  # skip identify (cutoff remains 0)
    else {cutoffs$cutoff[i]=round(d$x[identified],3)
    abline(v=d$x[identified],col="red",lty=2,lwd=1)     # show cutoff in plot
    print(paste("Cutoff of",cutoffs$markers[i],         # status message for console
                "is set to:",round(d$x[identified],3)))
    # Sys.sleep(0.5)
    }
  }
  graphics.off(); setwd(location)                           # close graphics device and restore orig. directory
  cutoffs
}