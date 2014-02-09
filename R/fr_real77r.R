## The scaling exponent formualtion originally(?) stemming from uses of Hall's exponents by Real (1977)
# real77r: A typeII + scaling exponent
real77r <- function(X, b, q, h, T) {
    if(is.list(b)){
        coefs <- b
        b <- coefs[['b']]
        q <- coefs[['q']]
        h <- coefs[['h']]
        T <- coefs[['T']]
    }
    a <- (b*X^q)
    return(X^(q+1)-lambertW(a*h*X^(q+1)*exp(-a*(T-h*X^(q+1))))/(a*h))
}

# real77r_fit: Does the heavy lifting
real77r_fit <- function(data, samp, start, fixed, boot=FALSE, windows=FALSE) {
    # Setup windows parallel processing
    fr_setpara(boot, windows)
    samp <- sort(samp)
    dat <- data[samp,]
    out <- fr_setupout(start, fixed, samp)
    
    try_real77r <- try(mle2(real77r_nll, start=start, fixed=fixed, data=list('X'=dat$X, 'Y'=dat$Y), 
                           optimizer='optim', method="Nelder-Mead", control=list(maxit=5000)), 
                      silent=T)
    if (inherits(try_real77r, "try-error")) {
        # The fit failed...
        if(boot){
            return(out)
        } else {
            stop(try_real77r[1])
        }
    } else {
        # The fit 'worked'
        for (i in 1:length(names(start))){
            # Get coefs for fixed variables
            cname <- names(start)[i]
            vname <- paste(names(start)[i], 'var', sep='')
            out[cname] <- coef(try_real77r)[cname]
            out[vname] <- vcov(try_real77r)[cname, cname]
        }
        for (i in 1:length(names(fixed))){
            # Add fixed variables to the output
            cname <- names(fixed)[i]
            out[cname] <- as.numeric(fixed[cname])
        }
        if(boot){
            return(out)
        } else {
            return(list(out=out, fit=try_real77r))
        }
    }
}	
# real77r_nll: Provides negative log-likelihood for estimations via mle2()
real77r_nll <- function(b, q, h, T, X, Y) {
    if (b <= 0 || h <= 0) {return(NA)} # Estimates must be > zero
    if (q < -1){return(NA)} # q+1 must be positive
    prop.exp = real77r(X, b, q, h, T)/X
    if(any(is.complex(prop.exp))){return(NA)} # Complex numbers don't help!
    # The proportion consumed must be between 0 and 1 and not NaN
    # If not then it must be bad estimate of a and h and should return NA
    if(any(is.nan(prop.exp)) || any(is.na(prop.exp))){return(NA)} 
    if(any(prop.exp > 1) || any(prop.exp < 0)){return(NA)} 
    return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
}

# The diff function
real77r_diff <- function(X, grp, b, q, h, T, Db, Dq, Dh) {
  # a <- ( b        *X^ q        )
    a <- ((b-Db*grp)*X^(q-Dq*grp))
  # return(X^( q        +1)-lambertW(a* h        *X^( q        +1)*exp(-a*(T- h        *X^( q        +1))))/(a* h        ))
    return(X^((q-Dq*grp)+1)-lambertW(a*(h-Dh*grp)*X^((q-Dq*grp)+1)*exp(-a*(T-(h-Dh*grp)*X^((q-Dq*grp)+1))))/(a*(h-Dh*grp)))
}

# The diff_nll function
real77r_nll_diff <- function(b, q, h, T, Db, Dq, Dh, X, Y, grp) {
    if (b <= 0 || h <= 0) {return(NA)} # Estimates must be > zero
    if (q < -1){return(NA)} # q+1 must be positive
    prop.exp = real77r_diff(X, grp, b, q, h, T, Db, Dq, Dh)/X
    if(any(is.complex(prop.exp))){return(NA)} # Complex numbers don't help!
    # The proportion consumed must be between 0 and 1 and not NaN
    # If not then it must be bad estimate of a and h and should return NA
    if(any(is.nan(prop.exp)) || any(is.na(prop.exp))){return(NA)} 
    if(any(prop.exp > 1) || any(prop.exp < 0)){return(NA)} 
    return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
}

