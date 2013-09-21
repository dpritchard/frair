## Type I functional response function.
# Basically a straight line with an intercept at zero

## Type I functional response ##
typeI <- function(X, c) {
    if(is.list(c)){
        coefs <- c
        c <- coefs[['c']]
    }
    X*c
}

# typeI_fit: Does the heavy lifting
# data = The data from which to subsample. X and Y are drawn from here.
# samp = Provided by boot() or manually, as required
# start = List of starting values for items to be optimised.  Can only be 'c'.

typeI_fit <- function(data, samp, start, fixed, boot=FALSE, windows=FALSE) {
    # Setup windows parallel processing
	fr_setpara()
    
    samp <- sort(samp)
    dat <- data[samp,]
    out <- fr_setupout(start, fixed, samp)
    
    try_typeI <- try(mle2(typeI_nll, start=start, data=c(fixed, list('X'=dat$X, 'Y'=dat$Y))), silent=T) 
    ## Remove 'silent=T' for more verbose output
    if (inherits(try_typeI, "try-error")){
        # The fit failed...
        if(boot){
            return(out)
        } else {
            stop(try_typeI[1])
        }
    } else {
        # The fit 'worked'
        for (i in 1:length(names(start))){
            # Get coefs for fixed variables
            cname <- names(start)[i]
            vname <- paste(names(start)[i], 'var', sep='')
            out[cname] <- coef(try_typeI)[cname]
            out[vname] <- vcov(try_typeI)[cname, cname]
        }
        for (i in 1:length(names(fixed))){
            # Add fixed variables to the output
            cname <- names(fixed)[i]
            out[cname] <- as.numeric(fixed[cname])
        }
        if(boot){
            return(out)
        } else {
            return(list(out=out, fit=try_typeI))
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