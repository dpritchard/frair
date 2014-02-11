\name{fr_real77}
\alias{fr_real77}
\alias{real77}
\alias{real77_fit}
\alias{real77_nll}

\title{Scaling Exponent Response, assuming replacement}
\description{Scaling exponent response (assuming replacement) based on ideas dating back to Real (1977, at least)}

\usage{
    real77_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    real77_nll(b, q, h, T, X, Y)
    real77(X, b, q, h, T)
    }

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a' and 'h'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'T'.}
  \item{boot }{A logical. Is the function being called for use by the boot function?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{b, q, h }{The search coefficient (\emph{b}), scaling exponent (\emph{q}) and the handling time (\emph{h}). Usually items to be optimised.}
  \item{T }{\emph{T}, the total time available.}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This implements a type-II response with a scaling exponent on \emph{X}, based on the use of Hill's exponents described by Real (1977). When \eqn{q \ge 0} the response becomes progressively more 'type-III-ish'. Integer values of \emph{q} have useful interpretations based in enzymatic biochemistry but extending to may other fields (\emph{e.g.} Flynn \emph{et. al} 1997, Vucic-Pestic \emph{et. al} 2010). Importantly, this function assumes that prey are replaced throughout the experiment (\emph{c.f.} \code{\link{real77r}}). 

The attack rate (\emph{a}) and number of prey eaten (\emph{Ne}) follow the following relationships:

\deqn{a = b X^q}{a = b*X^q}

\deqn{N_e=\frac{a X^(q+1) T}{1+a X^(q+1) h}}{Ne=(a*X^(q+1)*T)/(1+a*X^(q+1)*h)}

Where \emph{b} is a search coefficient and other coefficients are as defined in \code{\link{hollingsII}}. Indeed when \eqn{q = 0} then \eqn{a = b} and the relationship collapses to traditional type-II \link[=hollingsII]{Holling's Disc Equation}. The use of \eqn{q + 1} in the equation for number of prey eaten (\emph{vs.} \eqn{q - 1} in the equation for \emph{a}) exposes a useful test on \eqn{q = 0} in the summary of the fit and follows the approach used by other authors (\emph{e.g.} Vucic-Pestic \emph{et. al} 2010)

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly is desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{real77_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package). The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{real77_nll} function to optimise \code{real77}.

Further references and recommended reading can be found on the the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\references{
Flynn KJ, Fasham MJR, Hipkin CR (1997) Modelling the interactions between ammonium and nitrate uptake in marine phytoplankton. Philosophical Transactions of the Royal Society B: Biological Sciences 352: 1625--1645.

Vucic-Pestic O, Rall BC, Kalinkat G, Brose U (2010) Allometric functional response model: body masses constrain interaction strengths. Journal of Animal Ecology 79: 249--256. doi:10.1111/j.1365-2656.2009.01622.x.
}

\examples{
# TODO
}
