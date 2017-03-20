\name{gammarus}
\alias{gammarus}
\docType{data}
\title{Gammarus Functional Response Data}
\description{Functional response dataset for two species of \emph{Gammarus} spp. (freshwater amphipods) eating \emph{Simulium} spp. (black fly) larvae.}
\usage{data(gammarus)}
\format{A dataframe with the following structure:
    \tabular{rlll}{
		density: \tab An integer. The initial density of prey\cr
		eaten: \tab An integer. The number of prey eaten\cr
		alive:\tab An integer. The number of prey left alive\cr
		spp: \tab A factor with levels \code{G.d.celticus} and \code{G.pulex}. The species of predator.
	}
}
\details{
This dataset is a stripped-down version of that presented in Paterson et al. 2014. It contains only \emph{Simulium} spp. data with all other treatments (other than predator identity) pooled. The predators are amphipods which are either native (\emph{Gammarus duebeni celticus}) or invasive (\emph{Gammarus pulex}) to waterways in Ireland.  Total experimental time was 40 hours.  
}
\source{Paterson RA, Dick JTA, Pritchard DW, Ennis M, Hatcher MJ & Dunn AM. 2014. Predicting invasive species impacts: community module functional response experiments reveal context dependencies. \emph{Journal of Animal Ecology} 84:453-463 doi:1111/1365-2656.12292}

\examples{
data(gammarus)
str(gammarus) 

with(gammarus, 
    plot(density, eaten, type='n', 
	xlab='Density', ylab='No. Prey Eaten'))
with(gammarus[gammarus$spp=='G.d.celticus',], 
	points(density-0.2, eaten, pch=20, col=rgb(0,0.5,0,0.2)))
with(gammarus[gammarus$spp=='G.pulex',], 
	points(density+0.2, eaten, pch=20, col=rgb(0.5,0,0,0.2)))

legend(2,20, c('Native', 'Invasive'), pch=20, 
	col=c(rgb(0,0.5,0), rgb(0.5,0,0)))
}
\keyword{datasets}
