\name{frair_fit_methods}
\alias{frair_fit_methods}
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
This documents modifications to standard methods used in frair objects of classes \code{frfit}. However, because standard naming conventions have been used, utility functions might also work (e.g. \code{\link{coefficients}})

\code{lines} only adds lines to an existing plot, so an active graphics device needs to be present.
}

\seealso{
\code{\link{frair_fit}}, \code{\link{lines}}.
}

\author{
Daniel Pritchard
}

\examples{
data(gammarus)
outII <- frair_fit(eaten~density, data=gammarus, response='rogersII', 
        start=list(a = 1.2, h = 0.015), fixed=list(T=1))

# Visualise fit
plot(outII, pch=20, col=rgb(0,0,0,0.2), xlim=c(0,30))
lines(outII)
}



