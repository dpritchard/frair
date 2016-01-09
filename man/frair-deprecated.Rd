\name{frair-deprecated}
\alias{frair-deprecated}

\alias{fr_bdII}
\alias{bdII}
\alias{bdII_fit}
\alias{bdII_nll}
\alias{bdII_diff}
\alias{bdII_nll_diff}

\alias{fr_real77r}
\alias{real77r}
\alias{real77r_fit}
\alias{real77r_nll}
\alias{real77r_diff}
\alias{real77r_nll_diff}

\alias{fr_real77}
\alias{real77}
\alias{real77_fit}
\alias{real77_nll}
\alias{real77_diff}
\alias{real77_nll_diff}

\title{Deprecated Functional Response Models}

\description{A list of deprecated (i.e. no longer in use) functions / models.}

\usage{
    bdII(...)
    bdII_fit(...)
    bdII_nll(...)
    bdII_diff(...)
    bdII_nll_diff(...)
    real77r(...)
    real77r_fit(...)
    real77r_nll(...)
    real77r_diff(...)
    real77r_nll_diff(...)
    real77(...)
    real77_fit(...)
    real77_nll(...)
    real77_diff(...)
    real77_nll_diff(...)
}

\arguments{
  \item{... }{Items to pass through a deprecated function}
}

\details{

\code{bdII}, \code{bdII_fit} and \code{bdII_nll} were deprecated and removed from FRAIR in verison 0.4.1. Although internally consistent, this code did not actually model the Beddington-DeAngelis Type-II curve, as advertised. This code used in these functions was renamed \link{emdII}. A correct implimentation of the Beddington-DeAngelis Type-II curve (assuming replacement) will be added at some point in the future.  

The \code{real77*} family were deprecated and removed from FRAIR in verison 0.4.1. These functions were poorly specificed, with unhelpful (that is to say: incorrect) definitions of the exponent term (\code{q}). More sensible flexible exponent model specifications (\code{\link{flexp}} and \code{\link{flexpnr}}) have been added as a replacement.  
}

\author{
Daniel Pritchard
}
