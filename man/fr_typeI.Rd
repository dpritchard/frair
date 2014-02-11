\name{fr_typeI}
\alias{fr_typeI}
\alias{typeI}
\alias{typeI_fit}
\alias{typeI_nll}

\title{Type I Response}
\description{A generic type I (linear) response.}

\usage{
    typeI_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    typeI_nll(a, T, X, Y)
    typeI(X, a, T)
    }

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'T'.}
  \item{boot }{A logical. Is the function being called for use by the boot function?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{a }{The capture rate}
  \item{T }{T: Total time available}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This implements a simple Type-I, or linear functional response. This is helpful when the response is known (or suspected) to be handling time independent. It is implemented as: 

\deqn{N_e = a N_0 T}{Ne = a*N0*T}

where \emph{a} is the capture rate, \emph{T} is the total time available and \emph{N0} (== X) is the initial prey density. 

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly is desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{typeI_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package).  The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{typeI_nll} function to optimise \code{typeI}.

Further references and recommended reading can be found on the the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\examples{
datx <- rep(1:60, times=5)
r1 <- rnorm(60*5, mean = 0.25, sd = 0.1)
r2 <- rnorm(60*5, mean = 0.75, sd = 0.1)
r1[r1>1] <- 1
r2[r2>1] <- 1
daty1 <- abs(round(r1*datx, 0))
daty2 <- abs(round(r2*datx, 0))
dat <- data.frame(datx,daty1,daty2)

TI1 <- frair_fit(daty1~datx, data=dat, response='typeI', 
        start=list(a=0.5), fixed=list(T=1))
TI2 <- frair_fit(daty2~datx, data=dat, response='typeI', 
        start=list(a=0.5), fixed=list(T=1))

plot(c(0,60), c(0,60), type='n', xlab='Density', ylab='No. Eaten')
points(TI1)
points(TI2, col=4)
lines(TI1)
lines(TI2, col=4)

# Test with frair_compare
frair_compare(TI1, TI2)

\dontrun{
# Test with a big stick
TI1b <- frair_boot(TI1)
TI2b <- frair_boot(TI2)
confint(TI1b)
confint(TI2b)

plot(c(0,60), c(0,60), type='n', xlab='Density', ylab='No. Eaten')
drawpoly(TI1b, col=1)
drawpoly(TI2b, col=4)
points(TI1b, pch=20)
points(TI2b, pch=20, col=4)
}

}
