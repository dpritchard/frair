\name{frair_boot_methods}
\alias{print.frboot}
\alias{confint.frboot}
\alias{plot.frboot}
\alias{lines.frboot}
\alias{drawpoly.frboot}
\alias{print.frconf}

\title{frair methods}
\description{Documentation for methods for class frboot}

\usage{
\method{print}{frboot}(x, \dots)
\method{confint}{frboot}(object, parm='all', level=0.95, \ldots, citypes='all')
\method{plot}{frboot}(x, xlab=x$xvar, ylab=x$yvar, \ldots)
\method{lines}{frboot}(x, all_lines=FALSE, bootcol=1, bootalpha=1/sqrt(x$n_boot), \ldots)
\method{drawpoly}{frboot}(x, \ldots, probs=c(0.025, 0.975))

\method{print}{frconf}(x, \dots)
}

\arguments{
  \item{x, object }{Output from a call to \code{frair_boot} (or \code{confint.frboot}).}
  \item{parm }{A character vector. Which parameter to get CIs for? See Details.}
  \item{level }{A numeric. The confidence limit for CIs.}
  \item{citypes }{A character vector. What kind of CI? See Details.}
  \item{all_lines }{A logical. Should the bootstrapped results be plotted?. See Details.}
  \item{xlab }{Label for the x-axis.}
  \item{ylab }{Label for the y-axis.}
  \item{bootcol }{A valid colour for the bootstraped lines.}
  \item{bootalpha }{A transparency for the (inevitably overlapping) lines.}
  \item{probs }{Lower and upper tails for confidence interval polygons. See \code{\link{quantile}}.}
  \item{\ldots }{Other items passed to underlying functions.}
}


\details{
This documents modifications to standrd methods used in frair objects of class \code{frboot}. However, becuase standard naming conventions have been used, utility functions might also work (e.g. \code{\link{coefficients}})


The code underlying \code{confint.frboot} is quite complex and relys heavily on the excellent work done by Brian Ripley in \code{\link[boot]{boot.ci}}. Some of the complexity of \code{boot.ci} is hidden, but, like all FRIAR objects you can access the orginal method by passing the output directly (e.g. \code{boot.ci(object$fit)}). 

Like \code{\link[boot]{print.bootci}} the \code{print()} method for objects produced by \code{print.frboot} will report potentially unstable intervals. However these are calculated and returned by \code{confint.frboot}, not when \code{print()} is called (see Value, below). When calling \code{confint.frboot} you can request (a combination of) different intervals. The default \code{'all'} is equivilent to \code{c('norm', 'basic', 'stud', 'perc', 'bca')} which are the Normal approximation, Basic, Studenti[sz]ed, Percentile and BCa intervals, repectivly. Each has strengths and weaknesses which the user should be aware of.  

\code{lines} and \code{polygon} only add lines or polygons to an existing plot, so an active graphics device needs to be present. By default \code{all} is FALSE.  The simple reason for this is because the code is a little slow, so currently it is an 'opt-in' option.

}

\value{
\code{confint.frboot} returns a nested list with m items at the top level and n items at the second level, where m is the number of coefficients and n is the number of types of confidence intervals. Each named object at the second level is a list containing:
\item{lower }{The upper limit.}
\item{upper }{The lower limit.}
\item{bootciout }{The output from \code{boot.ci} (if successful; NA otherwise).}
and optionally:
\item{errors }{The error(s) encountered by \code{boot.ci}.}
\item{warnings }{The warning(s) encountered by \code{boot.ci}, plus a warning if extreme values were used.}
\item{notes }{A comment on potential instability of intervals, if justified.}

These last two items combine 'true' warnings and the tests for interval stability described in \code{\link[boot]{print.bootci}}

All confidence intervals are calculated on the original scale. If you want to calculate intervals on a transformed scale, call \code{\link[boot]{boot.ci}} directly using the \code{boot.ci(object$fit)} syntax.

}

\seealso{
\code{\link{frair_boot}}, \code{\link{lines}}, \code{\link{polygon}}.
}

\author{
Daniel Pritchard
}

\examples{
# TODO
}



