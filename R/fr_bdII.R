## Beddington–DeAngelis Type-II curve
# Reduces to rogers_II when P == 1
# N0 replaced with 'X' for simplicity and consistency.
# X = Number of 'prey' (prey density / concentration)
# Y = Number of prey eaten / consumed / killed / absorbed

## Beddington–DeAngelis Type-II function ##
# Same as ?lambertW, with the addition of 'P'
# Everything except 'X' should be provided.
bdII <- function(X, a, h, P, T) {
    if(is.list(a)){
        coefs <- a
        a <- coefs[['a']]
        h <- coefs[['h']]
        P <- coefs[['P']]
        T <- coefs[['T']]
    }
	X - lambertW(a * h * X * exp(-a * (P * T - h * X)))/(a * h)
}
# bdII_fit: Does the heavy lifting
bdII_fit <- function(data, samp, start, fixed, boot=FALSE, windows=FALSE) {
	# Setup windows parallel processing
	fr_setpara(boot, windows)
	samp <- sort(samp)
	dat <- data[samp,]
	out <- fr_setupout(start, fixed, samp)

    try_bdII <- try(mle2(bdII_nll, start=start, fixed=fixed, data=list('X'=dat$X, 'Y'=dat$Y)), silent=T) # Remove 'silent=T' for more verbose output
	if (inherits(try_bdII, "try-error")) {
 		# The fit failed...
 		if(boot){
 			return(out)
        } else {
 			stop(try_bdII[1])
 		}
 	} else {
        # The fit 'worked'
 	    for (i in 1:length(names(start))){
            # Get coefs for fixed variables
 	        cname <- names(start)[i]
            vname <- paste(names(start)[i], 'var', sep='')
 	        out[cname] <- coef(try_bdII)[cname]
            out[vname] <- vcov(try_bdII)[cname, cname]
 	    }
 	    for (i in 1:length(names(fixed))){
            # Add fixed variables to the output
 	        cname <- names(fixed)[i]
 	        out[cname] <- as.numeric(fixed[cname])
 	    }
 		if(boot){
 			return(out)
 		} else {
 			return(list(out=out, fit=try_bdII))
 		}
 	}
}	
# bdII_nll
# Provides negative log-likelihood for estimations via mle2()
# See Bowkers book for more info
bdII_nll <- function(a, h, T, P, X, Y) {
	if (a < 0 || h < 0 || T < 0 || P < 0) {
		return(NA)
		}
		prop.exp = bdII(X, a, h, P, T)/X
		return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
	}