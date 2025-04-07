\name{fr_flexp}
\alias{fr_flexp}
\alias{flexp}
\alias{flexp_fit}
\alias{flexp_nll}

\title{Scaling Exponent Response, assuming replacement}
\description{Scaling exponent response (assuming replacement) based on ideas dating back to Real (1977, at least)}

\usage{
    flexp_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    flexp_nll(b, q, h, T, X, Y)
    flexp(X, b, q, h, T)
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
This implements a type-II response with a scaling exponent on the capture rate (\emph{a}), based on the use of Hill's exponents described by Real (1977). When \eqn{q \ge 0} the response becomes progressively more 'type-III-ish'. Integer values of \emph{q} have useful interpretations based in enzymatic biochemistry but have been extended to many other fields (e.g. Flynn et al. 1997), including functional response analysis (Vucic-Pestic et al. 2010). Importantly, this function assumes that prey are replaced throughout the experiment (c.f. \code{\link{flexpnr}} which does not). 

The capture rate (\emph{a}) follows the following relationship:

\deqn{a = b X^q}{a = b*X^q}

and then (\emph{a}) is used to calculate the number of prey eaten (\emph{Ne}) following the same relationship as \code{\link{hollingsII}}:

\deqn{N_e=\frac{a N_0 T}{1+a N_0 h}}{Ne=(a*N0*T)/(1+a*N0*h)}

where \emph{b} is a search coefficient and other coefficients are as defined in \code{\link{hollingsII}}. Indeed when \eqn{q = 0}, then \eqn{a = b} and the relationship collapses to traditional type-II \link[=hollingsII]{Holling's Disc Equation}. There is, therefore, a useful test on \eqn{q = 0} in the summary of the fit.  

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly if desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{flexp_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package). The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link[bbmle]{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{flexp_nll} function to optimise \code{flexp}.

Further references and recommended reading can be found on the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}, \code{\link{flexpnr}}.
}

\references{
Real LA (1977) The Kinetics of Functional Response. \emph{The American Naturalist} 111: 289--300.

Vucic-Pestic O, Rall BC, Kalinkat G, Brose U (2010) Allometric functional response model: body masses constrain interaction strengths. \emph{Journal of Animal Ecology} 79: 249--256. doi:10.1111/j.1365-2656.2009.01622.x.

Flynn KJ, Fasham MJR, Hipkin CR (1997) Modelling the interactions between ammonium and nitrate uptake in marine phytoplankton. \emph{Philosophical Transactions of the Royal Society B: Biological Sciences} 352: 1625--1645.
}

\examples{
data(bythotrephes)
# NB: The flexpnr model is not correct for the experimental design of the bythotrephes data

expofit <- frair_fit(eaten~density, data=bythotrephes, 
                response='flexpnr', start=list(b = 0.5, q = 1, h = 0.15), 
                fixed=list(T=12/24))
## Plot
plot(expofit)
lines(expofit, col=2)

## Inspect
summary(expofit$fit) 
}
