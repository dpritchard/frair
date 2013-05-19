\name{fr_rogersII}
\alias{fr_rogersII}
\alias{rogersII}
\alias{rogersII_fit}
\alias{rogersII_nll}

\title{Roger's Type II Response}
\description{Roger's Type II decreasing prey function.}

\usage{
    rogersII_fit(data, samp, start, fixed, boot = FALSE, windows=FALSE)
    rogersII_nll(a, h, T, P, X, Y)
    rogersII(X, a, h, P, T)
	}

\arguments{
  \item{data }{A dataframe containing X and Y.}
  \item{samp }{A vector specifying the rows of data to use in the fit. Provided by \code{boot()} or manually, as required}
  \item{start }{A named list. Starting values for items to be optimised.  Usually 'a' and 'h'.}
  \item{fixed }{A names list. 'Fixed data' (not optimised). Usually 'T' and 'P'.}
  \item{boot }{A logical. Is the function being called for use by the boot function?}
  \item{windows }{A logical. Is the operating system Microsoft Windows?}
  \item{a, h }{Attack rate and handling time. Often items to be optimised.}
  \item{P, T }{P and T}
  \item{X }{The X variable. Usually prey density.}
  \item{Y }{The Y variable.  Usually the number of prey consumed.}
}
\details{
None of these functions are designed to be called directly, though they can be if desired.  The intention is that they are called via \code{\link{frair_fit}}, which calls them in the order they are specified above.

\code{rogersII_fit} does the heavy lifting and also pulls double duty as the \code{statistic} function for bootstrapping (via \code{boot()} in the boot package).  The \code{windows} argument if required to prevent needless calls to \code{require()} on platforms that can manage sane parallel processing.

The core fitting is done by \code{\link{mle2}} from the \code{bbmle} package and users are directed there for more information.  \code{mle2} uses the \code{rogersII_nll} function to optimise \code{rogersII}.

\code{rogersII} is fitted using the \code{\link{lambertW}} from the \code{\link{emdbook}} package (indeed the fuction is copied from the help file of \code{lambertW})

All of these functions and this approach in general are described in further detail by Bowker (2008) and users are directed there for more information.  Users within the QUB functional repsonse group are directed to the Functional Reposnse Manual for further information.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}.
}

\references{
Bolker, B. M., 2008. \emph{Ecological Models and Data in R}. Princeton University Press, Princeton, NJ.
}

\examples{
# TODO
}
