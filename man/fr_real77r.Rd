\name{fr_real77r}
\alias{fr_real77r}
\alias{real77r}
\alias{real77r_fit}
\alias{real77r_nll}

\title{Scaling Exponent Response, not assuming replacement}
\description{Scaling exponent response (not assuming replacement) based on ideas dating back to Real (1977, at least)}

\usage{
    real77r_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    real77r_nll(b, q, h, T, X, Y)
    real77r(X, b, q, h, T)
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
This combines a type-II non-replacement functional response (\emph{i.e.} a \link[=rogersII]{Roger's random predator equation}) with a scaling exponent on \emph{X}. This function is generalised from that described in \code{\link{real77}} relaxing the assumption that prey are replaced throughout the experiment. 

The capture rate (\emph{a}) and number of prey eaten (\emph{Ne}) follow the following relationships:

\deqn{a = b X^q}{a = b*X^q}

\deqn{N_e=N_0^(q+1) (1-e^{(a(N_eh-T))})}{Ne=N0^(q+1)*(1-exp(a*(Ne*h-T)))}

Where \emph{b} is a search coefficient and other coefficients are as defined in \code{\link{rogersII}}. Because \emph{Ne} appears on both side of the equation, the solution is found using Lambert's transcendental equation. FRAIR uses the \code{\link[emdbook]{lambertW}} function from the \emph{emdbook} package and the internal function is:

\code{Ne <- X^(q+1)-lambertW(a*h*X^(q+1)*exp(-a*(T-h*X^(q+1))))/(a*h)}

where \eqn{X = N0}. When \eqn{q = 0} then \eqn{a = b} and the relationship collapses to traditional type-II Roger's random predator equation. The use of \eqn{q + 1} in the equation for number of prey eaten (\emph{vs.} \eqn{q - 1} in the equation for \emph{a}) exposes a useful test on \eqn{q = 0} in the summary of the fit. 

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly is desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{real77r_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package). The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{real77r_nll} function to optimise \code{real77r}.

Further references and recommended reading can be found on the the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\examples{
# TODO
}
