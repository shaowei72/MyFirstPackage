% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read yearly FARS data from list of files into a tibble list}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{Vector denoting the years for which the yearly FARS data is required}
}
\value{
This function returns a list of tibble, with each element containing the yearly FARS data
}
\description{
Function that reads in data from a csv file and returns a tibble
}
\details{
Function aborts and prints error message if there is no FARS data for a particular year
}
\note{
Makes use of make_filename() and fars_read() functions
}
\examples{
\dontrun{
fars_read_years(c("2020", "2019", "2018"))
}

}
