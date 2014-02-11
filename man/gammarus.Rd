\name{gammarus}
\alias{gammarus}
\docType{data}
\title{Gammarus Functional Response Data}
\description{Functional response dataset for two species of Gammarus (freshwater amphipods) eating Simulium (black fly) larvae.}
\usage{data(gammarus)}
\format{A dataframe with the following structure:
    \tabular{rlll}{
		density: \tab An integer. The initial density of prey\cr
		eaten: \tab An integer. The number of prey eaten\cr
		alive:\tab An integer. The number of prey left alive\cr
		spp: \tab A factor with levels 'G.d.celticus' and 'G.pulex'. The species of predator.
	}
}
\details{
This dataset is a stripped-down version of that presented in Paterson \emph{et al.} 2014. It contains only Simulium data and all other treatments (other than predator identity) pooled. The predators are amphipods which are either native (\emph{Gammarus duebeni celticus}) or invasive (\emph{Gammarus pulex}) to waterways in Ireland.  
}
\source{Paterson R.A., Dick J.T.A., Pritchard D.W., Ennis M., Hatcher M.J., Dunn A.M. In Review. Predicting invasive species impacts: community module functional response experiments reveal context dependencies. Submitted to Journal of Animal Ecology, December 2013.}

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
