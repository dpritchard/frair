\name{frair_methods}
\alias{print.frfit}
\alias{plot.frfit}
\alias{lines.frfit}
\alias{print.frboot}
\alias{plot.frboot}
\alias{lines.frboot}
\alias{drawpoly.frboot}

\title{frair methods}
\description{Documentation for methods for classes frfit and frboot}

\usage{
\method{print}{frfit}(x, \dots)
\method{plot}{frfit}(x, xlab=x$xvar, ylab=x$yvar, \ldots)
\method{lines}{frfit}(x, \dots)

\method{print}{frboot}(x, \dots)
\method{plot}{frboot}(x, xlab=x$xvar, ylab=x$yvar, \ldots)
\method{lines}{frboot}(x, all=FALSE, colour=1, alpha=1/sqrt(x$n_boot), ...)
\method{drawpoly}{frboot}(x, probs=c(0.025, 0.975), ...)
}

\arguments{
  \item{x }{Output from a call to \code{frair_fit}.}
  \item{all }{A logical. Should the bootstrapped results be plotted?.  See Details.}
  \item{xlab }{Label for the x-axis.}
  \item{ylab }{Label for the y-axis.}
  \item{colour }{A valid colour for the plotted lines.}
  \item{alpha }{A transparency for the (inevitably overlapping) lines.}
  \item{probs }{Lower and upper tails for confidence interval polygons. See \code{\link{quantile}}.}
  \item{\ldots }{Other items passed to underlying functions.}
}


\details{
This documents modifications to standrd methods used in frair objects of classes \code{frfit} and \code{frboot}. However, becuase standard naming conventions have been used, utility funcitons might also work (e.g. \code{\link{coefficients}})

\code{lines} and \code{polygon} only adds lines or polygons to an existing plot, so an active graphics device needs to be present.  

By default \code{all} is FALSE.  The simple reason for this is because the code is a little slow, so currently it is an 'opt-in' option.  
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



