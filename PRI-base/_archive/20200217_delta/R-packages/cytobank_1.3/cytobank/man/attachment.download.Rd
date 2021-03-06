% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attachment_download.R
\name{attachment.download}
\alias{attachment.download}
\title{Download several or all attachments of an experiment}
\usage{
attachment.download(site, usr, pwd, link, filename, directory = getwd())
}
\description{
Download several or all attachments of an experiment
}
\details{
\code{Download attachment files of interest from Cytobank}.
}
\examples{
\donttest{# Download attachments from community server:
community="https://community.cytobank.org/cytobank"
attachments=attachment.list(community, 45682)
attachment.download(community, username, password,
attachment$Link, attachment$Filename, directory="path/to/store/files")
}
}
