\name{fr_functions}
\alias{fr_responses}

\title{Functional responses}
\description{Support functions for fitting predator-prey functional responses.}

\usage{
	fr_responses(show=FALSE)
	}

\arguments{
  \item{show }{A logical. If TRUE, information is printed to screen and nothing is returned.}
}
\details{
\code{fr_responses} is both a vector of useful information (via \code{show=TRUE}) and a vechicle to keep track of implimented functional responses.  The later is enforced by matching responses supplied to \code{\link{fr_fit}} to the \code{names} returned by \code{fr_responses(show=FALSE)}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{fr_fit}}.
}

\examples{
resp_known <- names(fr_responses())
fr_responses(show=TRUE)
}
