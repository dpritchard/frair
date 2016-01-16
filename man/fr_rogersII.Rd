\name{fr_rogersII}
\alias{fr_rogersII}
\alias{rogersII}
\alias{rogersII_fit}
\alias{rogersII_nll}

\title{Rogers' Type II Response}
\description{Rogers' Type II decreasing prey function.}

\usage{
    rogersII_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    rogersII_nll(a, h, T, X, Y)
    rogersII(X, a, h, T)
	}

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required.}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a' and 'h'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'T'.}
  \item{boot }{A logical. Is the function being called for use by \code{boot()}?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{a, h }{Capture rate and handling time. Usually items to be optimised.}
  \item{T }{\emph{T}, the total time available.}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This implements the Rogers' 'random predator' type-II functional response. This does not assume prey are replaced throughout the experiment (c.f. \code{\link{hollingsII}}). The number of prey eaten (\eqn{N_e}) follows the relationship:

\deqn{N_e=N_0(1-e^{(a(N_eh-T))})}{Ne=N0*(1-exp(a*(Ne*h-T)))}

Where \eqn{N_0} is the initial number of prey and \emph{a}, \emph{h} and \emph{T} are the capture rate, handling time and the total time available, respectively. The fact that \eqn{N_e} appears on both side of the equation, poses some problems, but can be efficiently dealt with using Lambert's transcendental equation (Bolker, 2008). FRAIR uses the \code{\link[lamW]{lambertW0}} function from the \emph{lamW} package and uses this function internally as:

\code{Ne <- X - lambertW0(a * h * X * exp(-a * (T - h * X)))/(a * h)}

where \eqn{X = N_0}. For further information users are directed to Chapter 8 (and preceding chapters, if needed) of Bolker (2008) where this approach is discussed in depth. Note that Bolker (2008) uses an implementation that 'partitions' the \emph{a} and \emph{h} coefficients between multiple prey items. This code is implemented in FRAIR as \code{\link{emdII}}.

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly if desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{hollingsII_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package). The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{rogersII_nll} function to optimise \code{rogersII}.

Further references and recommended reading can be found on the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\references{
Bolker BM (2008) \emph{Ecological Models and Data in R}. Princeton University Press, Princeton, NJ.
}

\examples{
data(gammarus)

pulex <- gammarus[gammarus$spp=='G.pulex',]
celt <- gammarus[gammarus$spp=='G.d.celticus',]

pulexfit <- frair_fit(eaten~density, data=pulex, 
                response='rogersII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1))
celtfit <- frair_fit(eaten~density, data=celt, 
                response='rogersII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1))

plot(c(0,30), c(0,30), type='n', xlab='Density', ylab='No. Eaten')
points(pulexfit)
points(celtfit, col=4)
lines(pulexfit)
lines(celtfit, col=4)

frair_compare(pulexfit, celtfit)

\dontrun{
pulexfit_b <- frair_boot(pulexfit)
celtfit_b <- frair_boot(celtfit)
confint(pulexfit_b)
confint(celtfit_b)
}
}
