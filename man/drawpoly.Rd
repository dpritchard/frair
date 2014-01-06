\name{drawpoly}
\alias{drawpoly}
\alias{drawpoly.default}

\title{Draw polygons}
\description{Draws polygons around to denote}
\usage{
    drawpoly(x, upper, lower, ...)
}

\arguments{
  \item{x }{The x values of the polygon}
  \item{upper }{The upper 'edge' of the polygon}
  \item{lower }{The lower 'edge' of the polygon}
  \item{\dots }{Other arguments passed to \code{polygon}}
}

\details{
\code{drawpoly} is a generic method for drawing polygons where the polygon is drawn as:

\code{polygon(x=c(x, rev(x), x[1]), y=c(upper, rev(lower), upper[1])}

\emph{i.e.} a line following along the top edge (left-to-right along \code{x}) and back along the bottom edge (right-to-left along \code{x}). 

The specific method implemented here for FRAIR is \code{link{drawpoly.frboot}}.
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{drawpoly.frboot}}, ~~~
}

\examples{
datx <- 1:6
upper <- datx*1.2
lower <- datx*0.8
plot(datx, datx, type='n', ylim=c(0,10), xlab='X', y='Y')
drawpoly(datx, upper, lower, col=2)
points(datx, datx, pch=20)
}
