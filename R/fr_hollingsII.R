## Holling's Orginal Type II pre-prey function.
# hollingsII: The guiding function...
hollingsII <- function(X, a, h) {
    if(is.list(a)){
        coefs <- a
        a <- coefs[['a']]
        h <- coefs[['h']]
    }
	a*X/(1+a*h*X)
}
# hollingsII_fit: Does the heavy lifting
hollingsII_fit <- function(data, samp, start, fixed, boot=FALSE, windows=FALSE) {
	# Setup windows parallel processing
	fr_setpara(boot, windows)
	samp <- sort(samp)
	dat <- data[samp,]
	out <- fr_setupout(start, fixed, samp)

    try_hollingsII <- try(mle2(hollingsII_nll, start=start, fixed=fixed, data=list('X'=dat$X, 'Y'=dat$Y)), silent=T) # Remove 'silent=T' for more verbose output
	if (inherits(try_hollingsII, "try-error")) {
 		# The fit failed...
 		if(boot){
 			return(out)
        } else {
 			stop(try_hollingsII[1])
 		}
 	} else {
        # The fit 'worked'
 	    for (i in 1:length(names(start))){
            # Get coefs for fixed variables
 	        cname <- names(start)[i]
            vname <- paste(names(start)[i], 'var', sep='')
 	        out[cname] <- coef(try_hollingsII)[cname]
            out[vname] <- vcov(try_hollingsII)[cname, cname]
 	    }
 	    for (i in 1:length(names(fixed))){
            # Add fixed variables to the output
 	        cname <- names(fixed)[i]
 	        out[cname] <- as.numeric(fixed[cname])
 	    }
 		if(boot){
 			return(out)
 		} else {
 			return(list(out=out, fit=try_hollingsII))
 		}
 	}
}	
# hollingsII_nll: Provides negative log-likelihood for estimations via mle2()
hollingsII_nll <- function(a, h, X, Y) {
    if (a < 0 || h < 0) {
		return(NA)
		}
		prop.exp = a/(1+a*h*X)
		return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
	}