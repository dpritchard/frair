\name{fr_fit}
\alias{fr_fit}
\alias{print.fr_fit}

\title{Fit predator-prey functional responses.}
\description{Fits predator-prey function responses and returns data in a consistent, predictable way, exposing some useful methods.}

\usage{
fr_fit(formula, data, response, start=list(), fixed=list(), 
        boot=FALSE, nboot=999, para=TRUE, WARN.ONLY=FALSE)

\method{print}{fr_fit}(x, \dots)
}

\arguments{
  \item{formula }{A simple formula of the form \code{y ~ x}.}
  \item{data }{The dataframe containing \code{x} and \code{y}.}
  \item{response }{A string denoting the response to fit.  See Details.}
  \item{start }{A named list. Starting values for optimised parameters.}
  \item{fixed }{A named list. Values that are not optimised.}
  \item{boot }{A logical. Should bootstrapped estimates be generated?}
  \item{nboot }{An integer. How many bootstraps to perform?}
  \item{para }{A logical. Should the bootstrapping be performed in parallel?}
  \item{WARN.ONLY}{A logical. If true some errors are suppressed. See Details.}
  \item{x }{Output from a non-bootstrap call to \code{fr_fit}.}
  \item{\ldots }{Other items passed to generic functions.}
}

\details{
The \code{response} requested must be known to dgmisc.  To establish what is supported inspect the output from \code{fr_responses(show=TRUE)}. All parameters listed by \code{fr_responses(show=TRUE)} (except \code{X}) must be provided in either \code{start} or \code{fixed}.  

The underlying fitting is response-specific and users should consult the help page for each function for more information about what is being done in the background.  

Non-parametric bootstrapping and paralellisation is handled by \code{\link{boot}} from the boot library.  Currently, if you request bootstrapped fits and \code{para=TRUE} (the default) then the function with attempt to use all available cores (which might affect performance while the bootstrap is underway!)

If more than 10\% of the bootstrapped fits fail then a warning is generated and if more than 50\% of the fits fail then an error is thrown and nothing is returned.  These are sensible defaults, but if you are very sure that you know what you are doing you can suppress this with \code{WARN.ONLY=TRUE} (a warning is thrown instead).  
}

\value{
This function returns a named list of class \code{fr_fit} or \code{fr_boot}, depending on the way in which the function is called.

Lists of class \code{fr_fit} have the following named items:
\item{call }{The original call to \code{fr_fit}.}
\item{data }{The original data supplied to \code{fr_fit}.}
\item{response }{A string. The fitted response.}
\item{xvar }{A string. The righthand side of \code{formula}.}
\item{yvar }{A string. The lefthand side of \code{formula}.}
\item{fit }{The raw object returned by the fitting procedure (response specific).}


In addition there is a named item for each fitted parameter (\emph{i.e.} those supplied in \code{start}), suffixed with a zero, containing the parameter estimate for the curve fitted to the original dataset.  

With the exception of the \code{fit} item, lists of class \code{fr_boot} contain everything listed above.  These objects also contain vector of \code{length(nboot)} containing each of the bootstrapped parameter estimates along with:
\item{sample }{An nboot-by-n matrix. The rows of the original data used in each bootstrap.}
\item{n_failed }{The number of failed fits.}
\item{n_duplicated }{The number of fits that were duplicates.}
\item{n_boot }{The number of bootstrapped fits.}
}

\section{Methods}{
Currently only a \code{print} method is defined for objects of class \code{fr_fit}.  This wraps the default \code{print} method for the fitted object (which is response specific).
}

\seealso{
\code{\link{fr_responses}}, \code{\link{fr_rogersII}}.
}

\author{
Daniel Pritchard
}

\examples{
den <- c(2,2,2,2,4,4,4,4,6,6,6,6,8,8,8,8,10,10,10,10,20,20,20,20,30,30,30,30,60,60,60,60)
eat <- c(0,2,2,2,4,4,4,4,5,6,6,3,3,8,6,5,5,3,0,7,5,14,9,7,18,4,17,8,4,16,46,13)
dat <- data.frame(density=den, eaten=eat)
out <- fr_fit(eaten~density, data=dat, response=rogersII, start=list(a = 1.2, h = 0.015), fixed=list(P=1, T=1), boot=FALSE)

}
