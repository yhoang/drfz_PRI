% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_FlowRep.R
\name{getFlowRep}
\alias{getFlowRep}
\alias{getFlo}
\title{Download one or more Datasets from FlowRepository}
\usage{
getFlowRep(expIDs, directory = getwd(), verbose = F)
}
\description{
Download one or more Datasets from FlowRepository
}
\details{
\code{Function needs one or more Experiment IDs as a vector or character and 
a directory path as character to save downloaded files}.
}
\examples{
\donttest{# getFlowRep("FR-FCM-ZY69","/Path/to/store/files")
}
}
