\name{frair_compare}
\alias{frair_compare}

\alias{typeI_diff}
\alias{typeI_nll_diff}

\alias{rogersII_diff}
\alias{rogersII_nll_diff}

\alias{hollingsII_diff}
\alias{hollingsII_nll_diff}

\alias{hassIII_diff}
\alias{hassIII_nll_diff}

\alias{hassIIInr_diff}
\alias{hassIIInr_nll_diff}

\alias{emdII_diff}
\alias{emdII_nll_diff}

\alias{flexp_diff}
\alias{flexp_nll_diff}

\alias{flexpnr_diff}
\alias{flexpnr_nll_diff}

\title{Test the difference between two functional response fits}
\description{Explicitly model, and then test, the difference between two functional response fits.}

\usage{frair_compare(frfit1, frfit2, start = NULL)}

\arguments{
  \item{frfit1 }{An object of class \code{frfit}}
  \item{frfit2 }{An object of class \code{frfit}}
  \item{start }{A named numeric list with starting values for the combined fit. See Details.  }
}
\details{
This function provides a sensible test of the optimised coefficients between two functional responses fitted by \code{\link{frair_fit}}. This is achieved by explicitly modelling a 'difference' (delta) parameter for each optimised variable following the advice outlined in Julliano (2001). Briefly, consider the following Hollings type-II model:

\code{a*X*T/(1+a*X*h)}

the model containing delta parameters becomes:

\code{(a-Da*grp)*X*T/(1+(a-Da*grp)*X*(h-Dh*grp))}

where \code{grp} is a dummy coding variable and \code{Da} and \code{Dh} are the delta parameters. Here, the first functional response fit (\code{frfit1}) is coded \code{grp=0} and the second fit (\code{frfit2}) is coded \code{grp=1}. Therefore Da and Dh represent the difference between the two modeled coefficients and the standard output from the maximum likelihood fitting explicitly tests the null hypothesis of no difference between the two groups. 

The difference model is re-fit to the combined data in \code{frfit1} and \code{frfit2} using the same maximum likelihood approaches outlined in \code{\link{frair_fit}}. 

This function could be seen as a less computationally intensive alternative to bootstrapping but relies on \code{\link[bbmle]{mle2}} successfully returning estimates of the standard error. \code{mle2} does this by inverting a Hessian matrix, a procedure which might not always be successful. 

Future versions of FRAIR will look to improve the integration between \code{mle2} and allow users access to the various hessian control parameters. In the meantime the following delta functions are exported so that users can 'roll their own' maximum likelihood implementation using this approach:

\tabular{lll}{
    Original Function        \tab Difference Function    \tab Difference NLL Function\cr
    \code{\link{typeI}}      \tab \code{typeI_diff}      \tab \code{typeI_nll_diff}\cr
    \code{\link{hollingsII}} \tab \code{hollingsII_diff} \tab \code{hollingsII_nll_diff}\cr
    \code{\link{rogersII}}   \tab \code{rogersII_diff}   \tab \code{rogersII_nll_diff}\cr
    \code{\link{hassIII}}    \tab \code{hassIII_diff}    \tab \code{hassIII_nll_diff}\cr
    \code{\link{hassIIInr}}  \tab \code{hassIIInr_diff}  \tab \code{hassIIInr_nll_diff}\cr
    
    \code{\link{emdII}}      \tab \code{emdII_diff}      \tab \code{emd_nll_diff}\cr
    \code{\link{flexp}}      \tab \code{flexp_diff}      \tab \code{flexp_nll_diff}\cr
    \code{\link{flexpnr}}    \tab \code{flexpnr_diff}    \tab \code{flexpnr_nll_diff}
  }

}
\value{
Prints the results of the test to screen and invisibly returns on object of class \code{frcompare} inheriting from \code{class(list)} containing:

\item{frfit1 }{The first FR fit, as supplied.}
\item{frfit2 }{The second FR fit, as supplied.}
\item{test_fit }{The output of the re-fitted model.}
\item{result }{Coefficients from the test that are otherwise printed to screen.}

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

pulex <- gammarus[gammarus$spp=='G.pulex',]
celt <- gammarus[gammarus$spp=='G.d.celticus',]

pulexfit <- frair_fit(eaten~density, data=pulex, 
                response='rogersII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1))
celtfit <- frair_fit(eaten~density, data=celt, 
                response='rogersII', start=list(a = 1.2, h = 0.015), 
                fixed=list(T=1))
# The following tests the null hypothesis that the 
# true difference between the coefficients is zero:
frair_compare(pulexfit, celtfit) # Reject null for 'h', do not reject for 'a'

\dontrun{
# Provides a similar conclusion to bootstrapping followed by 95\% CIs
pulexfit_b <- frair_boot(pulexfit)
celtfit_b <- frair_boot(celtfit)
confint(pulexfit_b)
confint(celtfit_b) # 'a' definitely overlaps
}
}