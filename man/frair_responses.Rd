\name{frair_responses}
\alias{frair_responses}

\title{FRAIR responses}
\description{Available predator-prey functional responses.}

\usage{
	frair_responses(show=TRUE)
	}

\arguments{
  \item{show }{A logical. If TRUE, information is printed to screen and nothing is returned.}
}
\details{
\code{frair_responses} is both a vector of useful information (via \code{show=TRUE}) and a vechicle to keep track of implimented functional responses.  The later is enforced by matching responses supplied to \code{\link{frair_fit}} to the \code{names} returned by \code{frair_responses(show=FALSE)}.
}

\author{
Daniel Pritchard
}

\examples{
resp_known <- names(frair_responses())
frair_responses(show=TRUE)
}
