\name{fr_plot}
\alias{fr_plot}

\title{Plot the output of \code{fr_fit}}
\description{A half-baked plotting code.  Only implemented for Rogers Type II fits.}
\usage{
fr_plot(fr_model, plotboot=FALSE, bootlines=TRUE, alpha=NaN, ...)
}

\arguments{
  \item{fr_model }{The output of \code{fr_fit} (i.e. a list of class \code{fr_fit} or \code{fr_boot}}
  \item{plotboot }{A logical. Should the bootstrapped results be plotted (USE WITH CAUTION).  See Details.}
  \item{bootlines }{A logical. Plots lines if \code{TRUE}.  Plots 95\%CI 'patches' if \code{FALSE}.}
  \item{alpha }{A numeric. Alpha transparancy for lines.}
  \item{... }{Other items passed to underlying functions.}
}
\details{
This function only adds lines or polygons to an existing plot, so an active graphics device needs to be present.  

By default \code{plotboot} is FALSE.  The very good reason for this is because two of the coefficients for the Rogers Type II curve are hardcoded as constants.  This is obviously not a tenable situation, but will require some modification of the \code{fr_boot} class to accomodate this.
}

\author{
Daniel Pritchard
}

\section{todo }{
\itemize{
    \item Remove hardcoded constants
    \item Allow for definition of confidence intervals (currently only allows 95\%CI)
    }
}

\seealso{
\code{\link{fr_fit}}, \code{\link{lines}}, \code{\link{polygon}}
}
\examples{
# TODO
}
