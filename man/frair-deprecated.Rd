\name{frair-deprecated}
\alias{frair-deprecated}
\alias{fr_bdII}
\alias{bdII}
\alias{bdII_fit}
\alias{bdII_nll}
\alias{bdII_diff}
\alias{bdII_nll_diff}

\title{Deprecated Functional Response Models}

\description{A list of deprecated (i.e. no longer in use) functions / models.}

\usage{
    bdII(...)
    bdII_fit(...)
    bdII_nll(...)
    bdII_diff(...)
    bdII_nll_diff(...)
}

\arguments{
  \item{... }{Items to pass through a deprecated function}
}

\details{

\code{bdII}, \code{bdII_fit} and \code{bdII_nll} were deprecated in verison 0.4.1. Although internally consistent, this code did not actually model the Beddington-DeAngelis Type-II curve, as advertised. This code used in these functions was renamed \link{emdII}. A correct implimentation of the Beddington-DeAngelis Type-II curve (assuming replacement) will be added at some point in the future.  

}

\author{
Daniel Pritchard
}
