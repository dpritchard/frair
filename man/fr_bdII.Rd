\name{fr_bdII}
\alias{fr_bdII}
\alias{bdII}
\alias{bdII_fit}
\alias{bdII_nll}

\title{Beddington-DeAngelis Type II Response}
\description{Beddington-DeAngelis Type II decreasing prey function.}

\usage{
    bdII_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    bdII_nll(a, h, P, T, X, Y)
    bdII(X, a, h, P, T)
    }

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a' and 'h'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'P' and 'T'.}
  \item{boot }{A logical. Is the function being called for use by the boot function?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{a, h }{Capture rate and handling time. Usually items to be optimised.}
  \item{P, T }{P: Number of predators. T: Total time available}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This implements the Beddington-DeAngelis Type-II functional response which is an extension of the Roger's random predator equation to allow for multiple predators. With the exception of \emph{P} these functions are identical to those used in \code{\link{rogersII}}. 

The \code{bdII} function solves the random predator equation using the LambertW equation (using the \code{\link[emdbook]{lambertW}} function from the emdbook package), giving:

\code{X - lambertW(a * h * X * exp(-a * (P * T - h * X)))/(a * h)}

Note that generally speaking \emph{P} is determined by the experimental design and is therefore usually provided as a 'fixed' variable. When \eqn{P = 1} the results should be identical to those provided by \code{\link{rogersII}}.  

This is exactly the function in chapter 8 of Bolker (2008), which in turn presents examples from  Vonesh and Bolker (2005). Users are directed there for more information. 

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly if desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{bdII_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package).  The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{bdII_nll} function to optimise \code{bdII}.

Further references and recommended reading can be found on the the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\references{
Vonesh JR, Bolker BM (2005) Compensatory larval responses shift trade-offs associated with predator-induced hatching plasticity. Ecology 86: 1580--1591. doi:10.1890/04-0535.

Bolker, BM (2008) \emph{Ecological Models and Data in R}. Princeton University Press, Princeton, NJ.
}

\examples{
data(gammarus)
fitP1 <- frair_fit(eaten~density, data=gammarus, 
                response='bdII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1, P=1))
fitP2 <- frair_fit(eaten~density, data=gammarus, 
                response='bdII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1, P=2))
# Note that the coefficients are scaled to per per prey item
coef(fitP1)
coef(fitP2)

# Should give idential answers to rogersII when P=1
rogII <- frair_fit(eaten~density, data=gammarus, 
                response='rogersII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1))
coef(fitP1)
coef(rogII)

stopifnot(coef(fitP1)[1]==coef(rogII)[1])
stopifnot(coef(fitP1)[2]==coef(rogII)[2])

}