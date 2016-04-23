\name{bythotrephes}
\alias{bythotrephes}
\docType{data}
\title{Bythotrephes Functional Response Data}
\description{Functional response dataset for \emph{Bythotrephes} spp. (water fleas) preying on prey items of different sizes.}
\usage{data(bythotrephes)}
\format{A dataframe with the following structure:
	\tabular{rlll}{
		density: \tab An integer. The initial density of prey\cr
		eaten: \tab An integer. The number of prey eaten\cr
		alive:\tab An integer. The number of prey left alive\cr
		size: \tab A factor with levels 'small', 'medium' and 'large'. The size of prey items.
	}
}
\details{\emph{Bythotrephes} spp. (water fleas) preying on prey items of different sizes. Prey were not replaced during the experiment. Provides a  example dataset for type-III and flexible exponent models. }
\source{Daniel Barrios-O'Neill.}
\examples{
data(bythotrephes)
str(bythotrephes) 

with(bythotrephes, 
	plot(density, eaten, type='n', 
	xlab='Density', ylab='No. Prey Eaten'))
with(bythotrephes[bythotrephes$size=='large',], 
	points(density-0.2, eaten, pch=20, col=rgb(0.5,0,0,0.4)))
with(bythotrephes[bythotrephes$size=='medium',], 
	points(density, eaten, pch=20, col=rgb(0,0.5,0,0.4)))
with(bythotrephes[bythotrephes$size=='small',], 
	points(density+0.2, eaten, pch=20, col=rgb(0,0,0.5,0.4)))

legend(1,12, c('Large', 'Medium', 'Small'), pch=20, bty = 'n', 
    col=c(rgb(0.5,0,0), rgb(0,0.5,0), rgb(0,0,0.5)))
}
\keyword{datasets}
