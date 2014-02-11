\name{fr_hollingsII}
\alias{fr_hollingsII}
\alias{hollingsII}
\alias{hollingsII_fit}
\alias{hollingsII_nll}

\title{Holling's Original Type II Response}
\description{Holling's Type II predator-prey function.}

\usage{
    hollingsII_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    hollingsII_nll(a, h, T, X, Y)
    hollingsII(X, a, h, T)
    }

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a' and 'h'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'T'.}
  \item{boot }{A logical. Is the function being called for use by the boot function?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{a, h }{Capture rate and handling time. Usually items to be optimised.}
  \item{T }{T: Total time available}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This implements the Hollings original Type-II functional response, otherwise known as the 'disc equation'. An important assumption of this equation is that prey density remains constant (\emph{i.e.} a 'replacement' experimental design). In practice this is often not the case and often the Roger's 'random predator' equation might be more appropriate (see \code{\link{rogersII}}).

In Holling's original formulation the number of prey eaten (\emph{Ne}) follows the relationship:

\deqn{N_e=\frac{a N_0 T}{1+a N_0 h}}{Ne=(a*N0*T)/(1+a*N0*h)}

Where \eqn{N_0} is the initial number of prey and \emph{a}, \emph{h} and \emph{T} are the capture rate, handling time and the total time available, receptively. It is implemented internally in FRAIR as: 

\code{Ne <- (a*X*T)/(1+a*X*h)}

where \eqn{X = N_0}. 

None of these functions are designed to be called directly, though they are all exported so that the user can call them directly if desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{rogersII_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package).  The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{rogersII_nll} function to optimise \code{rogersII}.

Further references and recommended reading can be found on the the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\references{
Bolker, BM (2008) \emph{Ecological Models and Data in R}. Princeton University Press, Princeton, NJ.
}

\examples{
datx <- rep(c(1,2,3,4,6,12,24,50,100), times=10)
daty1 <- round(hollingsII(X=datx, 
            a=0.75*rnorm(length(datx), mean=1, sd=0.1), 
            h=0.1*rnorm(length(datx), mean=1, sd=0.1), 
            T=1),0)
daty2 <- round(hollingsII(X=datx, 
            a=0.75*rnorm(length(datx), mean=1, sd=0.1), 
            h=0.01*rnorm(length(datx), mean=1, sd=0.1), 
            T=1),0)
dat <- data.frame(datx,daty1,daty2)

hollII_1 <- frair_fit(daty1~datx, data=dat, response='hollingsII', 
        start=list(a=1, h=0.1), fixed=list(T=1))
hollII_2 <- frair_fit(daty2~datx, data=dat, response='hollingsII', 
        start=list(a=1, h=0.01), fixed=list(T=1))

plot(c(0,100), c(0,40), type='n', xlab='Density', ylab='No. Eaten')
points(hollII_1)
points(hollII_2, col=4)
lines(hollII_1)
lines(hollII_2, col=4)

frair_compare(hollII_1, hollII_2)
}
