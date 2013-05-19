\name{friar_fit}
\alias{friar_fit}

\title{Fit predator-prey functional responses.}
\description{Fits predator-prey function responses and returns data in a consistent, predictable way, exposing some useful methods.}

\usage{
friar_fit(formula, data, response, start=list(), fixed=list(), 
        boot=FALSE, nboot=1500, para=TRUE, ncores=NaN, WARN.ONLY=FALSE)
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
  \item{ncores }{An integer. The number of cores to use for parallelisation. See Details}
  \item{WARN.ONLY}{A logical. If true some errors are suppressed. See Details.}
}

\details{
The \code{response} requested must be known to friar.  To establish what is supported inspect the output from \code{fr_responses(show=TRUE)}. All parameters listed by \code{fr_responses(show=TRUE)} (except \code{X}) must be provided in either \code{start} or \code{fixed}.  

The underlying fitting is response-specific and users should consult the help page for each function for more information about what is being done in the background.  

Non-parametric bootstrapping and paralellisation is handled by \code{\link{boot}} from the boot library.  Currently, if you request bootstrapped fits and \code{para=TRUE} (the default) then the function with attempt to use all available cores (which might affect performance while the bootstrap is underway!)

If more than 10\% of the bootstrapped fits fail then a warning is generated and if more than 50\% of the fits fail then an error is thrown and nothing is returned.  These are sensible defaults, but if you are very sure that you know what you are doing you can suppress this with \code{WARN.ONLY=TRUE} (a warning is thrown instead).  
}

\value{
This function returns a named list of class \code{frfit} or \code{frboot}, depending on the way in which the function is called.

Lists of class \code{frfit} have the following named items:
\item{call }{The original call to \code{friar_fit}.}
\item{x }{The original x data supplied to \code{friar_fit}.}
\item{y }{The original y data supplied to \code{friar_fit}.}
\item{response }{A string. The fitted response.}
\item{xvar }{A string. The righthand side of \code{formula}.}
\item{yvar }{A string. The lefthand side of \code{formula}.}
\item{optimvars }{A character vector. The optimised values (passed to \code{start}).}
\item{fixedvars }{A character vector. The fixed values (passed to \code{fixed}).}
\item{coefficients }{A named numeric. All coefficients needed to draw the optimised curve.}
\item{sample }{A numeric vector. Always \code{samp=c(1:nrow(data))} for objects of class \code{frfit} (c.f. class \code{frboot}, below).}
\item{fit }{The raw object returned by the fitting procedure (response specific).}

Objects of class \code{fr_boot} contain everything listed above, but \code{sample} is an nboot-by-n matrix denoting the bootstrap samples. In addition objects of \code{fr_boot} have the following:
\item{bootcoefs }{A named numeric matrix. The bootstrapped coefficients.}
\item{n_failed }{The number of failed fits.}
\item{n_duplicated }{The number of fits that were duplicates.}
\item{n_boot }{The number of (requested) bootstrapped fits.}
}

\seealso{
\code{\link{fr_responses}}, \code{\link{fr_rogersII}}.
}

\author{
Daniel Pritchard
}

\examples{
den <- c(2,2,2,2,4,4,4,4,6,6,6,6,8,8,8,8,10,10,10,
        10,20,20,20,20,30,30,30,30,60,60,60,60)
eat <- c(0,2,2,2,4,4,4,4,5,6,6,3,3,8,6,5,5,3,0,7,
        5,14,9,7,18,4,17,8,4,16,46,13)
dat <- data.frame(density=den, eaten=eat)
out <- friar_fit(eaten~density, data=dat, response=rogersII, 
        start=list(a = 1.2, h = 0.015), 
        fixed=list(P=1, T=1), boot=FALSE)
}
