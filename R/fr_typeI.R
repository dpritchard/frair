## Type I functional response function.
# Basically a straight line with an intercept at zero

## Type I functional response ##
typeI <- function(X, c) {
	X*c
}

# typeI_fit: Does the heavy lifting
# data = The data from which to subsample. X and Y are drawn from here.
# samp = Provided by boot() or manually, as required
# start = List of starting values for items to be optimised.  Can only be 'c'.
typeI_fit <- function(data, samp, start, fixed=list(), boot=FALSE, windows=FALSE) {
	if(windows && boot){
		dgmisc_load <- require(dgmisc, warn.conflicts=FALSE, quietly=TRUE)
		emdbook_load <- require(emdbook, warn.conflicts=FALSE, quietly=TRUE)
		bbmle_load <- require(bbmle, warn.conflicts=FALSE, quietly=TRUE)
		if(any(c(dgmisc_load, emdbook_load, bbmle_load)==FALSE)){
			stop('Error establishing workspace for parallel computing in Windows.')
		}
	}
	samp <- sort(samp)
	data <- data[samp,]
	X <- data$X
	Y <- data$Y
	fixed[['X']] <- X
	fixed[['Y']] <- Y
	try_typeI <- try(mle2(typeI_nll, start=start, data=fixed), silent=T) 
	## Remove 'silent=T' for more verbose output
	if (inherits(try_typeI, "try-error")){
 		# The fit failed...
 		out = c(NA, NA, NA, NA, samp)
 		names(out) <- c('c', 'cvar', rep('', times=length(samp)))
 		if(boot){
 			return(out)
 		} else {
 			stop(try_typeI[1])
 		}
 	} else {
 		out = c(coef(try_typeI)['c'], vcov(try_typeI)['c','c'], samp)
         names(out) <- c('c', 'cvar', rep('', times=length(samp)))
 		if(boot){
 			return(out)
 		} else {
 			return(try_typeI)
 		}
 	}
}	
# typeI_nll
# Provides negative log-likelihood for estimations via mle2()
# See Bowkers book for more info
# Generalised from rogersII_nll, shoudl be OK (DP)
typeI_nll <- function(c, X, Y) {
	if (c < 0) {
		return(NA)
		}
		prop.exp = typeI(X, c)/X
		return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
	}