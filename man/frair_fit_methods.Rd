\name{frair_fit_methods}
\alias{print.frfit}
\alias{plot.frfit}
\alias{lines.frfit}

\title{frair methods}
\description{Documentation for methods for class frfit}

\usage{
\method{print}{frfit}(x, \dots)
\method{plot}{frfit}(x, xlab=x$xvar, ylab=x$yvar, \ldots)
\method{lines}{frfit}(x, \dots)

}

\arguments{
  \item{x }{Output from a call to \code{frair_fit}.}
  \item{xlab }{Label for the x-axis.}
  \item{ylab }{Label for the y-axis.}
  \item{\ldots }{Other items passed to underlying functions.}
}


\details{
This documents modifications to standrd methods used in frair objects of classes \code{frfit}. However, becuase standard naming conventions have been used, utility funcitons might also work (e.g. \code{\link{coefficients}})

\code{lines} and \code{polygon} only adds lines or polygons to an existing plot, so an active graphics device needs to be present.
}

\seealso{
\code{\link{frair_fit}}, \code{\link{lines}}, \code{\link{polygon}}.
}

\author{
Daniel Pritchard
}

\examples{
# TODO
}



