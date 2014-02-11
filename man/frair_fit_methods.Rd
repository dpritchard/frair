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
\method{lines}{frfit}(x, tozero=FALSE, \dots)
}

\arguments{
  \item{x }{Output from a call to \code{frair_fit}.}
  \item{xlab }{Label for the x-axis.}
  \item{ylab }{Label for the y-axis.}
  \item{tozero }{A logical. Should the line be drawn to the origin?. See Details.}
  \item{\ldots }{Other items passed to underlying functions.}
}


\details{
This documents standard methods for frair objects of class \code{frfit}. However, because standard naming conventions have been used, some undocumented utility functions might also work (e.g. \code{\link{coefficients}})

\code{lines} only adds lines to an existing plot, so an active graphics device needs to be present. \code{lines} draws the curve for the fitted response at values:

\code{seq(from=1, to=max(x$x), by=1)} or 

\code{seq(from=0, to=max(x$x), by=1)} if \code{tozero} is TRUE.  
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