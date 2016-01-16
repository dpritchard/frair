\name{fr_flexpnr}
\alias{fr_flexpnr}
\alias{flexpnr}
\alias{flexpnr_fit}
\alias{flexpnr_nll}

\title{Scaling Exponent Response, not assuming replacement}
\description{Scaling exponent response (not assuming replacement) based on ideas dating back to Real (1977, at least)}

\usage{
    flexpnr_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    flexpnr_nll(b, q, h, T, X, Y)
    flexpnr(X, b, q, h, T)
    }

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required.}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a' and 'h'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'T'.}
  \item{boot }{A logical. Is the function being called for use by \code{boot()}?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{b, q, h }{The search coefficient (\emph{b}), scaling exponent (\emph{q}) and the handling time (\emph{h}). Usually items to be optimised.}
  \item{T }{\emph{T}, the total time available.}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This combines a type-II non-replacement functional response (\emph{i.e.} a \link[=rogersII]{Roger's random predator equation}) with a scaling exponent on the capture rate (\emph{a}). This function is generalised from that described in \code{\link{flexp}} relaxing the assumption that prey are replaced throughout the experiment. 

The capture rate (\emph{a}) follows the following relationship:

\deqn{a = b X^q}{a = b*X^q}

and then (\emph{a}) is used to calculate the number of prey eaten (\emph{Ne}) following the same relationship as \code{\link{rogersII}}:

\deqn{N_e=N_0(1-e^{(a(N_eh-T))})}{Ne=N0*(1-exp(a*(Ne*h-T)))}

where \emph{b} is a search coefficient and other coefficients are as defined in \code{\link{rogersII}}. Because \emph{Ne} appears on both side of the equation, the solution is found using Lambert's transcendental equation. FRAIR uses the \code{\link[lamW]{lambertW0}} function from the \emph{lamW} package and the internal function is:

\code{Ne <- X - lambertW0(a * h * X * exp(-a * (T - h * X)))/(a * h)}

where \eqn{X = N0}. When \eqn{q = 0}, then \eqn{a = b} and the relationship collapses to traditional type-II Rogers' random predator equation. There is, therefore, a useful test on \eqn{q = 0} in the summary of the fit. 

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly if desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{flexpnr_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package). The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{flexpnr_nll} function to optimise \code{flexpnr}.

Further references and recommended reading can be found on the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}, \code{\link{flexp}}.
}

\references{
Real LA (1977) The Kinetics of Functional Response. \emph{The American Naturalist} 111: 289--300.
}

\examples{
data(gammarus)

pulex <- gammarus[gammarus$spp=='G.pulex',]

rogfit <- frair_fit(eaten~density, data=pulex, 
                response='rogersII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1))
expofit <- frair_fit(eaten~density, data=pulex, 
                response='flexpnr', start=list(b = 1.2, q = 0, h = 0.015), 
                fixed=list(T=1))
# Plot
plot(rogfit)
lines(rogfit)
lines(expofit, col=2)

summary(rogfit$fit)
summary(expofit$fit)
# No evidence that q is different from zero...
}
