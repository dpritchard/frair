\name{frair_fit}
\alias{frair_fit}

\title{Fit predator-prey functional responses.}
\description{Fits predator-prey functional responses and returns data in a consistent, predictable way, exposing some useful methods.}

\usage{
frair_fit(formula, data, response, start=list(), fixed=NULL)
}

\arguments{
  \item{formula }{A simple formula of the form \code{y ~ x}.}
  \item{data }{The dataframe containing \code{x} and \code{y}.}
  \item{response }{A string denoting the response to fit.  See Details.}
  \item{start }{A named list. Starting values for optimised parameters.}
  \item{fixed }{A named list. Values that are not optimised.}
}

\details{
\code{frair_fit} is a utility function which helps users fit common non-linear predator-prey curves to integer data. It uses maximum likelihood estimation, via \code{\link[bbmle]{mle2}} from the \code{bbmle} package.  

The response requested must be known to FRAIR. To establish what is supported, inspect the output from \code{frair_responses()}. All parameters listed by \code{frair_responses()} (except \code{X}) must be provided in either \code{start} or \code{fixed} and some guidance is given on the help pages for each function about what should (and should not) be optimised.

Generally speaking fitting non-linear curves to ecological datasets can be challenging. Approaches to fitting predator-prey functional response curves are described in further detail by Juliano (2001) and Bolker (2008). Many of the pitfalls (along with very sound advice) in non-linear curve fitting in general are described by Bolker \emph{et al.} 2013. Users are directed there for more information.  

Note that currently all fits encoded by FRAIR use the \code{\link[stats]{optim}} optimiser with a non-default number of iterations (5000 [frair] vs. 500 [default]) and that all fits except \code{typeI} use the 'Nelder-Mead' method (see Note). This is different from the mle2 default, which currently (bbmle v. 1.0.15) uses the 'BFGS' method. 

\code{mle2} is clever inasmuch as it will return fitted values even if inverting the Hessian matrix at the optimum fails. However, this will result in a warning along the lines of: 

\preformatted{Warning message:
In mle2(fit, start = start, fixed = fixed, data = list(X = dat$X, :
  couldn't invert Hessian
}

If this happens it could mean many things, but generally speaking it is indicative of a poor fit to the data. You might consider:

\itemize{
\item Checking the data for transcription errors or outliers
\item Trying different starting values
\item Trying a different (simpler) curve
\item Fitting the curve outside of FRAIR using another optimiser or another approach (see the Note, below)
\item Collecting more data
}

Note that the advice given in \link[bbmle]{mle2} to use the 'Nelder-Mead' method, is largely redundant because this is already the default in FRAIR (though you could try the 'BFGS' method quite safely...)

If convergence (i.e. fitting) fails for other reasons, see the manual page of \code{\link[stats]{optim}}. 
}

\note{
Future versions will allow the user more control over the underlying fitting algorithms. In the meantime FRAIR exports all of its (useful) functions so that users can fit the curves directly using their preferred method if the defaults are undesirable. See the Examples for an illustration of this approach.  
}

\value{
This function returns a named list of class \code{frfit} with the following named items:
\item{call }{The original call to \code{frair_fit}.}
\item{x }{The original x data supplied to \code{frair_fit}.}
\item{y }{The original y data supplied to \code{frair_fit}.}
\item{response }{A string. The fitted response.}
\item{xvar }{A string. The right hand side of \code{formula}.}
\item{yvar }{A string. The left hand side of \code{formula}.}
\item{optimvars }{A character vector. The optimised values (passed to \code{start}).}
\item{fixedvars }{A character vector. The fixed values (passed to \code{fixed}).}
\item{coefficients }{A named numeric. All coefficients needed to draw the optimised curve.}
\item{sample }{A numeric vector. Always \code{samp=c(1:nrow(data))} (\emph{c.f.} class \code{\link{frair_boot}}).}
\item{fit }{The raw object returned by \code{\link[bbmle]{mle2}}.}

Objects of class \code{frfit} have print, plot and lines methods defined. See the help \link[=frair_fit_methods]{for those methods} for more information.
}

\seealso{
\code{\link{frair_boot}}, \code{\link{frair_responses}}, \code{\link{fr_rogersII}}.
}

\references{
Juliano SA (2001) \emph{Nonlinear curve fitting: Predation and functional response curves}. In: Scheiner SM, Gurevitch J (eds). \emph{Design and analysis of ecological experiments}. Oxford University Press, Oxford, United Kingdom. pp 178--196.

Bolker BM (2008) \emph{Ecological Models and Data in R}. Princeton University Press, Princeton, NJ.

Bolker BM and others (2013) \emph{Strategies for fitting nonlinear ecological models in R, AD Model Builder, and BUGS}. Methods in Ecology and Evolution 4: 501--512. doi:10.1111/2041-210X.12044.
}

\author{
Daniel Pritchard
}

\examples{
data(gammarus)

frair_responses() # See what is available
# A typeII fit
outII <- frair_fit(eaten~density, data=gammarus, response='rogersII', 
        start=list(a = 1.2, h = 0.015), fixed=list(T=40/24))

# A linear fit
outI <- frair_fit(eaten~density, data=gammarus, response='typeI', 
        start=list(a=0.5), fixed=list(T=40/24))

# Visualise fits
plot(outII, pch=20, col=rgb(0,0,0,0.2), xlim=c(0,30))
lines(outII)
lines(outI, lty=3)

# Have a look at original fits returned by mle2 (*highly* recommended)
summary(outII$fit)
summary(outI$fit)
# Compare models using AIC
AIC(outI$fit,outII$fit)

\donttest{
# Extra examples (skipped for CRAN incoming tests). 

# Bythotrephes
data("bythotrephes")
# Fit several models and examine them using AIC. 
b_flex <- frair_fit(eaten~density, data=bythotrephes, 
                    response='flexpnr', 
                    start=list(b = 1.2, q = 0, h = 0.015), 
                    fixed=list(T=12/24))
b_II <- frair_fit(eaten~density, data=bythotrephes, 
                  response='flexpnr', 
                  start=list(b = 1.2, h = 0.015), 
                  fixed=list(T=12/24, q = 0))
b_rogersII <- frair_fit(eaten~density, data=bythotrephes, 
                  response='rogersII', 
                  start=list(a = 1.2, h = 0.015), 
                  fixed=list(T=12/24))
AIC(b_flex$fit, b_II$fit, b_rogersII$fit)
AICtab(b_flex$fit, b_II$fit, b_rogersII$fit)
# b_II and b_rogersII are identical, by definition when q = 0
# b_flex is strongly preferred (delta AIC = 16.9)

# The role of T
## Users need to be aware that changing T will change 
## the units of fitted coefficients.  
## For example, with the Gammarus dataset:
g_T1 <- frair_fit(formula = eaten~density, data = gammarus, 
                  response = "rogersII", 
                  start = list(a = 2, h = 0.1), fixed = list(T = 1))
g_Td <- frair_fit(formula = eaten~density, data = gammarus, 
                  response = "rogersII", 
                  start = list(a = 1, h = 0.1), fixed = list(T = 40/24))
g_Th <- frair_fit(formula = eaten~density, data = gammarus, 
                  response = "rogersII", 
                  start = list(a = 0.05, h = 4), fixed = list(T = 40))
diff_t <- round(rbind(coef(g_T1), coef(g_Td), coef(g_Th)), 2)
row.names(diff_t) <- c("g_T1 (Experimental Time)", "g_Td (Days)", "g_Th (Hours)")
print(diff_t)
}

\dontrun{
## Fitting curves outside of FRAIR
# Many advanced users will not be satisfied with FRAIR current limitations.
# To fit models outside FRAIR, you could proceed as follows:

# Using mle2 or mle manually:
strt <- list(a = 1.2, h = 0.015)
fxd <- list(T=40/24)
dat <- list('X'=gammarus$density, 'Y'=gammarus$eaten)
manual_fit <- mle2(rogersII_nll, start=strt, fixed=fxd, 
                   method='SANN', data=dat)
# Note that the SANN method is *not* a general-purpose algorithm, 
# but it will return *something*, so might be helpful for finding starting values.

# Controlling iterations, optimisers, etc... See ?mle2 and ?optim
cntrl <- list(trace = 3, maxit = 1000)
manual_fit_2 <- mle2(rogersII_nll, start=strt, fixed=fxd, 
                     method='BFGS', data=dat, control=cntrl)
}
}
