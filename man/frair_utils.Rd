\name{frair_utils}
\alias{frair_utils}
\alias{fr_responses}
\alias{fr_setupout}

\title{frair utility functions}
\description{Utility functions for fitting predator-prey functional responses.}

\usage{
	fr_responses(show=FALSE)
    fr_setupout(start, fixed, samp)
	}

\arguments{
  \item{show }{A logical. If TRUE, information is printed to screen and nothing is returned.}
  \item{start }{A named numeric list}
  \item{fixed }{A named numeric list}
  \item{samp }{A numeric vector. The sample.}
}
\details{
\code{fr_responses} is both a vector of useful information (via \code{show=TRUE}) and a vechicle to keep track of implimented functional responses.  The later is enforced by matching responses supplied to \code{\link{frair_fit}} to the \code{names} returned by \code{fr_responses(show=FALSE)}.

\code{fr_setupout} is a utility function to help clean up the code in the various statitic functions. It returns a vector of (correctly) named NA's, based on the start and fixed input to the statistic function. 
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\section{todo }{
\itemize{
    \item No need to export fr_setupout. Unlikely to be useful to users?
    }
}

\examples{
resp_known <- names(fr_responses())
fr_responses(show=TRUE)
}
