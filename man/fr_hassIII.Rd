\name{fr_hassIII}
\alias{fr_hassIII}
\alias{hassIII}
\alias{hassIII_fit}
\alias{hassIII_nll}

\title{Hassell's Type III Response}
\description{Hassell's original type III response (assuming replacement)}

\usage{
    hassIII_fit(data, samp, start, fixed, boot=FALSE, windows=FALSE)
    hassIII_nll(b, c, h, T, X, Y)
    hassIII(X, b, c, h, T)
    }

\arguments{
  \item{data }{A data frame containing X and Y (at least).}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required.}
  \item{start }{A named list. Starting values for items to be optimised.  Usually \emph{b}, \emph{c} and \emph{h}.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually \emph{T}.}
  \item{boot }{A logical. Is the function being called for use by the boot function?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{b,c,h }{Hassel's \emph{b} and \emph{c}, plus \emph{h}, the handling time. Usually items to be optimised.}
  \item{T }{\emph{T}, the total time available.}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
This implements the original Hassel's Type-III functional response, assuming prey density is kept constant (\emph{i.e.} a 'replacement' experimental design). In practice, constant prey density might be an unrealistic assumption, in which case users should consider the \code{\link{hassIIIr}} function instead. 

In Hassel \emph{et al.}'s original formulation, the capture rate \emph{a} is assumed to vary with the prey density in the following hyperbolic relationship:

\code{a <- (b*X)/(1+c*X)}

where \code{b} and \code{c} are coefficients to be fitted and \emph{X} is the initial prey density. This is the initial formulation of Hassell \emph{et al.} (1977) and uses their naming conventions. The value for \emph{a} is then used within a traditional Holling's disc equation (see \code{\link{hollingsII}}). 

None of these functions are designed to be called directly, though they are all exported so that the user can do so if desired. The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{hassIII_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (\emph{via} \code{boot()} in the boot package). The \code{windows} argument if required to prevent needless calls to \code{require(frair)} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information. \code{mle2} uses the \code{hassIII_nll} function to optimise \code{hassIII}.

Further references and recommended reading can be found on the the help page for \link{frair_fit}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\references{
Hassell M, Lawton J, Beddington J (1977) Sigmoid functional responses by invertebrate predators and parasitoids. The Journal of Animal Ecology 46: 249--262.
}

\examples{
datx <- rep(c(1,2,3,4,6,12,24,50,100), times=10)
daty1 <- round(hassIII(X=datx, 
            b=0.08*rnorm(length(datx), mean=1, sd=0.1), 
            c=0.1*rnorm(length(datx), mean=1, sd=0.1), 
            h=0.1*rnorm(length(datx), mean=1, sd=0.1), 
            T=1),0)
daty2 <- round(hassIII(X=datx, 
            b=0.05*rnorm(length(datx), mean=1, sd=0.1), 
            c=0.1*rnorm(length(datx), mean=1, sd=0.1), 
            h=0.2*rnorm(length(datx), mean=1, sd=0.1), 
            T=1),0)
dat <- data.frame(datx,daty1,daty2)

hassIII_1 <- frair_fit(daty1~datx, data=dat, response='hassIII', 
        start=list(b=0.05, c=0.1, h=0.1), fixed=list(T=1))
hassIII_2 <- frair_fit(daty2~datx, data=dat, response='hassIII', 
        start=list(b=0.05, c=0.1, h=0.1), fixed=list(T=1))

plot(c(0,100), c(0,15), type='n', xlab='Density', ylab='No. Eaten')
points(hassIII_1)
points(hassIII_2, col=4)
lines(hassIII_1)
lines(hassIII_2, col=4)

frair_compare(hassIII_1, hassIII_2)

}
