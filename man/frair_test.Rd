\name{frair_test}
\alias{frair_test}
\alias{print.frtest}

\title{Test for evidence of type-II or type-III functional responses}
\description{Implements the phenomenological test of type-II \emph{versus} type-III functional responses described by Juliano (2001)}

\usage{
frair_test(formula, data)
\method{print}{frtest}(x, \dots)
}

\arguments{
  \item{formula }{A simple formula of the form \code{y ~ x}.}
  \item{data }{The dataframe containing \code{x} and \code{y}.}
  \item{x }{Output from \code{frair_test}.}
  \item{\dots }{Other items passed to the print method.}
}

\details{
This function wraps up an otherwise trivial test for type-II \emph{versus} type-III functional responses in a format consistent with the FRAIR syntax. It can be considered 'phenomenological' inasmuch as it tells the user if a type-II or type-III response is preferred, but not what form that curve should take nor if it is sensible to fit such a curve via non-linear regression.  

The test relies on the established principle that a logistic regression on the \emph{proportion} of prey consumed is a more sensitive test of functional response shape, especially at low prey densities, when a non-linear curve may not be able to distinguish the subtle difference in curve shape. 

The logic follows that on the proportion scale, a type-II response will show an increasing (\emph{i.e.} positive and statistically different from zero) initial slope with respect to density whereas a type-III response will show a negative slope, followed by a positive higher order slope. 

The test proceeds by fitting two models:

\code{glm(cbind(eaten,noteaten)~density, family='binomial')}

\code{glm(cbind(eaten,noteaten)~density+density^2, family='binomial')}

where \code{eaten} is the lefthand side of the \code{formula} input, \code{density} is the righthand side and \code{noteaten} is the difference between the two. The output from these models to determine which functional response is preferred using the logic above.

Currently no consideration is given to a type-I (\emph{i.e.} linear) response or any other potentially sensible fit other than a type-II or type-III response. It is up to the user to decide if it is appropriate to continue with fitting a mechanistic model of the functional response (\emph{i.e.} \code{\link{frair_fit}}, \code{\link{frair_compare}} and/or \code{\link{frair_boot}}) on the back of the results of this test. 
}

\value{
\code{frair_test} returns a list of class \code{frtest} with the following items:
\item{call }{The original call to \code{frair_test}.}
\item{x }{The original x data supplied to \code{frair_test}.}
\item{y }{The proportion of prey eaten: \eqn{y/x}}
\item{xvar }{A string. The right hand side of \code{formula}.}
\item{yvar }{A string. Always \code{'Proportion'}.}
\item{modT2 }{The output from the type-II glm}
\item{modT3 }{The output form the type-III glm}

}
\references{
Juliano SA (2001) Nonlinear curve fitting: Predation and functional response curves. In: Scheiner SM, Gurevitch J (eds). Design and analysis of ecological experiments. Oxford University Press, Oxford, United Kingdom. pp 178--196.
}
\author{
Daniel Pritchard
}

\seealso{
\code{\link{frair_fit}}
}
\examples{
data(gammarus)
frair_test(eaten~density, data=gammarus)

dat <- data.frame(x=1:100, y=floor(hassIII(1:100,b=0.01,c=0.001,h=0.03,T=1)))
frair_test(y~x, data=dat)
}
