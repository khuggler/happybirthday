% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeMarkdown.R
\name{makeMarkdown}
\alias{makeMarkdown}
\title{makeMarkdown}
\usage{
makeMarkdown(
  id_df,
  rollmean,
  subsetmonth,
  tempdir,
  envir = parent.frame(),
  spp
)
}
\arguments{
\item{id_df}{data.frame of lookup table}

\item{rollmean}{movement data product from the moveMetrics function}

\item{subsetmonth}{month to start plotting}

\item{tempdir}{temporary directory for storing products}

\item{spp}{character vector of species of interest. Choices include BHS, MD, ELK, MOOSE.}
}
\value{
returns plots for movement metrics in the temporary directory
}
\description{
create markdown plots for movement metrics
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
