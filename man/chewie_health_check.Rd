% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chewie-health-check.r
\name{chewie_health_check}
\alias{chewie_health_check}
\title{Check the status of the {chewie}}
\usage{
chewie_health_check(.test = TRUE, .report_cache = TRUE)
}
\arguments{
\item{.test}{logical; whether to test the credentials (requires internet
connection)}

\item{.report_cache}{logical; whether to report the cache status (default:
TRUE)}
}
\description{
checks the NASA Earthdata Credentials and GEDI Cache
environment variables
}
