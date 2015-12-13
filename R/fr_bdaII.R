## Beddington-DeAngelis Type-II curve
# X = Number of 'prey' (prey density / concentration)
# Y = Number of prey eaten / consumed / killed / absorbed

## Beddington-DeAngelis Type-II function ##
# Everything except 'X' should be provided.
bdaII <- function(X, a, h, P, T) {
    if(is.list(a)){
        coefs <- a
        a <- coefs[['a']]
        h <- coefs[['h']]
        P <- coefs[['P']]
        T <- coefs[['T']]
    }
	return(X - lamW::lambertW0(a * h * X * exp(-a * (P * T - h * X)))/(a * h))
}
# bdaII_fit: Does the heavy lifting
bdaII_fit <- function(data, samp, start, fixed, boot=FALSE, windows=FALSE) {
	# Setup windows parallel processing
	fr_setpara(boot, windows)
	samp <- sort(samp)
	dat <- data[samp,]
	out <- fr_setupout(start, fixed, samp)

    try_bdaII <- try(mle2(bdaII_nll, start=start, fixed=fixed, data=list('X'=dat$X, 'Y'=dat$Y), 
                         optimizer='optim', method='Nelder-Mead', control=list(maxit=5000)), 
                    silent=T)
	if (inherits(try_bdaII, "try-error")) {
 		# The fit failed...
 		if(boot){
 			return(out)
        } else {
 			stop(try_bdaII[1])
 		}
 	} else {
        # The fit 'worked'
 	    for (i in 1:length(names(start))){
            # Get coefs for fixed variables
 	        cname <- names(start)[i]
            vname <- paste(names(start)[i], 'var', sep='')
 	        out[cname] <- coef(try_bdaII)[cname]
            out[vname] <- vcov(try_bdaII)[cname, cname]
 	    }
 	    for (i in 1:length(names(fixed))){
            # Add fixed variables to the output
 	        cname <- names(fixed)[i]
 	        out[cname] <- as.numeric(fixed[cname])
 	    }
 		if(boot){
 			return(out)
 		} else {
 			return(list(out=out, fit=try_bdaII))
 		}
 	}
}	
# bdaII_nll
# Provides negative log-likelihood for estimations via mle2()
# See Bowkers book for more info
bdaII_nll <- function(a, h, P, T, X, Y) {
	if (a < 0 || h < 0){return(NA)}
	prop.exp = bdaII(X, a, h, P, T)/X
	if(any(is.complex(prop.exp))){return(NA)} # Complex numbers don't help!
	# The proportion consumed must be between 0 and 1 and not NaN
	# If not then it must be bad estimate of a and h and should return NA
	if(any(is.nan(prop.exp)) || any(is.na(prop.exp))){return(NA)} 
	if(any(prop.exp > 1) || any(prop.exp < 0)){return(NA)} 
	return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
}

# The difference function
bdaII_diff <- function(X, grp, a, h, P, T, Da, Dh) {
  # return(X - lamW::lambertW0(a * h * X * exp(-a * (P * T - h * X)))/(a * h))
    return(X - lamW::lambertW0((a-Da*grp) * (h-Dh*grp) * X * exp(-(a-Da*grp) * (P * T - (h-Dh*grp) * X)))/((a-Da*grp) * (h-Dh*grp)))
}

# The diff NLL fucntions
bdaII_nll_diff <- function(a, h, T, P, Da, Dh, X, Y, grp) {
    if (a < 0 || h < 0){return(NA)}
    prop.exp = bdaII_diff(X, grp, a, h, P, T, Da, Dh)/X
    if(any(is.complex(prop.exp))){return(NA)} # Complex numbers don't help!
    # The proportion consumed must be between 0 and 1 and not NaN
    # If not then it must be bad estimate of a and h and should return NA
    if(any(is.nan(prop.exp)) || any(is.na(prop.exp))){return(NA)} 
    if(any(prop.exp > 1) || any(prop.exp < 0)){return(NA)} 
    return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
}



