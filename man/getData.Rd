% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{getData}
\alias{getData}
\title{getData}
\usage{
getData(
  id_df,
  tempdir = NA,
  veckeys = NA,
  telonic_usrs = NA,
  telonic_pass = NA,
  ATS_usrs = NA,
  ATS_pass = NA,
  lotek_usrs = NA,
  lotek_pass = NA,
  tzone = "America/Los_Angeles",
  subsetmonth = "02"
)
}
\arguments{
\item{id_df}{data.frame that has the animal ids and serial numbers}

\item{tempdir}{character. path to temporary directory for telonics files to be saved. If this file in the path does not exist it will be created for you. Default is NA.}

\item{veckeys}{if Vectronic data is downloaded, path to Vectronic keys is needed, Default: NA}

\item{telonic_usrs}{if telonic data is needed, vector of telonic usernames for download}

\item{telonic_pass}{if telonic data is needed, vector of telonic passwords for download}

\item{ATS_usrs}{if ATS data is needed, vector of ATS usernames}

\item{ATS_pass}{if ATS data is needed, vectors of ATS passwords}

\item{lotek_usrs}{if lotek data is needed, vector of telonic passwords for download}

\item{lotek_pass}{if lotek data is needed, vector of lotek passwords for download}

\item{tzone}{time zone for your study area. Options are "America/Los_Angeles" or "America/Denver". America/Los_Angeles is the default.}

\item{subsetmonth}{month to start downloading GPS data}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
This function downloads GPS data for a specified population and appends animal IDs
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
\seealso{
\code{\link[processx]{run}}
\code{\link[collar]{ats_login}}, \code{\link[collar]{fetch_ats_positions}}, \code{\link[collar]{ats_logout}}, \code{\link[collar]{get_paths}}, \code{\link[collar]{fetch_vectronics}}
\code{\link[dplyr]{bind_rows}}
}
