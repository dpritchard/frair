\name{frair_boot_methods}
\alias{frair_boot_methods}
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
\method{lines}{frboot}(x, all_lines=FALSE, tozero=FALSE, bootcol=1, bootalpha=1/sqrt(x$n_boot), \ldots)
\method{drawpoly}{frboot}(x, \ldots, probs=c(0.025, 0.975), tozero=FALSE)

\method{print}{frconf}(x, \dots)
}

\arguments{
  \item{x, object }{Output from a call to \code{frair_boot} (or \code{confint.frboot}).}
  \item{parm }{A character vector. Which parameter to get CIs for? See Details.}
  \item{level }{A numeric. The confidence limit for CIs.}
  \item{citypes }{A character vector. What kind of CI? See Details.}
  \item{all_lines }{A logical. Should the bootstrapped results be plotted? See Details.}
  \item{tozero }{A logical. Should the line be drawn to the origin? See Details.}
  \item{xlab }{Label for the x-axis.}
  \item{ylab }{Label for the y-axis.}
  \item{bootcol }{A valid colour for the bootstrapped lines.}
  \item{bootalpha }{A numeric (0-1). A transparency for the (inevitably overlapping) lines.}
  \item{probs }{Lower and upper tails for confidence interval polygons. See \code{\link[stats]{quantile}}.}
  \item{\ldots }{Other items passed to underlying functions.}
}


\details{
This documents standard methods for FRAIR objects of class \code{frboot}. However, because standard naming conventions have been used, some undocumented utility functions might also work (e.g. \code{\link[stats]{coefficients}})

The code underlying \code{confint.frboot} is quite complex and relies heavily on the excellent work done by Brian Ripley in \code{\link[boot]{boot.ci}}. Some of the complexity of \code{boot.ci} is hidden, but, like all FRAIR objects you can access the original method by passing the output directly (e.g. \code{boot.ci(object$fit)}). 

Like \code{\link[boot]{print.bootci}} the \code{print()} method for objects produced by \code{print.frboot} will report potentially unstable intervals. However, these are calculated and returned by \code{confint.frboot}, not when \code{print()} is called (see Value, below). When calling \code{confint.frboot} you can request (a combination of) different intervals. The default \code{'all'} is equivalent to \code{c('norm', 'basic', 'stud', 'perc', 'bca')} which are the Normal approximation, Basic, Studentised, Percentile and BCa intervals, respectively. Each has strengths and weaknesses which the user should be aware of.  

\code{lines} and \code{drawpoly} only add lines or polygons to an existing plot, so an active graphics device needs to be present. By default \code{all} is FALSE. The simple reason for this is because the code is a little slow (on some devices), so currently it is an 'opt-in' option.

\code{drawpoly} draws empirical confidence intervals. The intervals are constructed by evaluating every set of bootstrapped coefficients at:

\code{seq(from=min(x$x), to=max(x$x), length.out = 50)}.  

and then calculating the empirical confidence limits at each value of \emph{x} by:

\code{apply(val, 2, quantile, na.rm=T, probs=probs)}

Note that this is a rough approximation of a bootstrapped confidence interval and does not account for some of the intricacies (e.g. bootstrap bias) described in \link[boot]{boot.ci}.

Note also, that if \code{tozero} is TRUE, then both \code{lines} and \code{drawpoly} attempt to draw to zero by evaluating every set of bootstrapped coefficients at:

\code{seq(from=0, to=max(x$x), length.out = 50)} 

If the coefficients provided by a fit to the orginal data produce a value that is undefined at zero, then these functions will plot lines to a small, non-zero number (1e-04) instead (with a warning). However, this does not guarantee that all of the values produced by the bootstrapped coefficients will produce sensible values. Clearly the intention here is to provide a nice-looking representation of the fitted curve and it is up to the user to determine \emph{why} their desired curve is undefined at zero.
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

These last two items combine 'true' warnings and the tests for interval stability described in \code{\link[boot]{print.bootci}}.

All confidence intervals are calculated on the original scale. If you want to calculate intervals on a transformed scale, call \code{\link[boot]{boot.ci}} directly using the \code{boot.ci(object$fit)} syntax.

}

\seealso{
\code{\link{frair_boot}}, \code{\link{lines}}, \code{\link{polygon}}.
}

\author{
Daniel Pritchard
}

\examples{
# This example is not run to save CRAN build server time...
\dontrun{
data(gammarus)

# Holling's is the wrong fit for these data based on the experimental design
# But it runs more quickly, so is a useful demonstration
outhol <- frair_fit(eaten~density, data=gammarus, response='hollingsII', 
        start=list(a = 1, h = 0.08), fixed=list(T=40/24))
outholb <- frair_boot(outhol)

confint(outholb)

# Illustrate bootlines
plot(outholb, xlim=c(0,30), type='n', main='All bootstrapped lines')
lines(outholb, all_lines=TRUE)
points(outholb, pch=20, col=rgb(0,0,0,0.2))

# Illustrate bootpolys
plot(outholb, xlim=c(0,30), type='n', main='Empirical 95 percent CI')
drawpoly(outholb, col=rgb(0,0.5,0))
points(outholb, pch=20, col=rgb(0,0,0,0.2))
}
}
