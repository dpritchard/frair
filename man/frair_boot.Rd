\name{frair_boot}
\alias{frair_boot}

\title{Bootstrap a predator-prey functional response.}
\description{Bootstraps a previously fitted predator-prey functional response and returns data in a consistent, predictable way, exposing some useful methods.}

\usage{
frair_boot(frfit, start=NULL, strata=NULL, nboot=999, 
            para=TRUE, ncores=NaN, WARN.ONLY=FALSE)
}

\arguments{
  \item{frfit }{An object returned by \code{\link{frair_fit}}}
  \item{start }{An optional named list. See Details.}
  \item{strata }{A character string. Specifies a column in the data original data.}
  \item{nboot }{An integer. How many bootstraps to perform?}
  \item{para }{A logical. Should the bootstrapping be performed in parallel?}
  \item{ncores }{An integer. The number of cores to use for parallelisation. See Details}
  \item{WARN.ONLY}{A logical. If true some errors are suppressed. See Details.}
}

\details{
This function provides a simple, consistent way to generate bootstrapped estimates from a functional response fit. 

If \code{start} is not provided starting values for the bootstrapping are drawn from the original fit.  This interface is provided so that a single set of starting parameters (e.g. a 'global' estimate) can be used when bootstrapping different functional response fits (e.g. different treatments)

Non-parametric bootstrapping and parallelisation is handled by \code{\link[boot]{boot}} from the boot package.  Currently, if you request bootstrapped fits and \code{para=TRUE} (the default) then the function with attempt to use all except 1 available core. Note that this might affect performance of other tasks while the bootstrap is underway!

If more than 10\% of the bootstrapped fits fail then a warning is generated and if more than 50\% of the fits fail then an error is thrown and nothing is returned.  These are sensible defaults, but if you are very sure that you know what you are doing you can suppress this with \code{WARN.ONLY=TRUE} (a warning is thrown instead).  

}

\value{
This function returns a named list of class \code{frboot} with the following named items:
\item{call }{The original call to \code{frair_fit}.}
\item{x }{The original x data supplied to \code{frair_fit}.}
\item{y }{The original y data supplied to \code{frair_fit}.}
\item{response }{A string. The fitted response.}
\item{xvar }{A string. The righthand side of \code{formula}.}
\item{yvar }{A string. The lefthand side of \code{formula}.}
\item{optimvars }{A character vector. The optimised values (passed to \code{start}).}
\item{fixedvars }{A character vector. The fixed values (passed to \code{fixed}).}
\item{coefficients }{A named numeric. All coefficients needed to draw the optimised curve.}
\item{sample }{A nboot-by-n numeric matrix. Where each row represents one bootstrap sample.}
\item{fit }{The raw object returned by the fitting procedure (response specific).}
\item{bootcoefs }{A named numeric matrix. The bootstrapped coefficients.}
\item{n_failed }{The number of failed fits.}
\item{n_duplicated }{The number of fits that were duplicates.}
\item{n_boot }{The number of (requested) bootstrapped fits.}
\item{stratified }{Was a stratified bootstrap performed?}

Objects of class \code{frboot} have print, confint, plot, lines and drawpoly methods defined. See the help \link[=frair_boot_methods]{for those methods} for more information.  
}

\seealso{
\code{\link{frair_boot_methods}}, \code{\link{frair_fit}}, \code{\link{fr_rogersII}}.
}

\author{
Daniel Pritchard
}

\examples{
data(gammarus)
frair_responses() # See what is available
# A typeII fit
outII <- frair_fit(eaten~density, data=gammarus, response='rogersII', 
        start=list(a = 1.2, h = 0.015), fixed=list(T=1))
        
\dontrun{
outIIb <- frair_boot(outII) # Slow
confint(outIIb)

# Illustrate bootlines
plot(outIIb, xlim=c(0,30), type='n', main='All bootstrapped lines')
lines(outIIb, all_lines=TRUE)
points(outIIb, pch=20, col=rgb(0,0,0,0.2))

# Illustrate bootpolys
plot(outIIb, xlim=c(0,30), type='n', main='Empirical 95 percent CI')
drawpoly(outIIb, col=rgb(0,0.5,0))
points(outIIb, pch=20, col=rgb(0,0,0,0.2))
}
}
