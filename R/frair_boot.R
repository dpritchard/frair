## frair_boot
# Wrapper function to bootstrap functional response curves
# One of the core functions of the frair package
frair_boot <- function(frfit, strata=NULL, nboot=999, para=TRUE, ncores=NaN, WARN.ONLY=FALSE){
    
    if(!inherits(frfit, 'frfit')){
        stop(paste0(deparse(substitute(frfit)), ' is not a friar object. Please fit a suitable response curve to your data with friar_fit() first.'))
    }
    
    # Check that can deal with the strata.
    stdo <- FALSE
    if(!is.null(strata)){
        if(length(strata)!=length(frfit$x)){
            stop('strata must be the same length as x')
        }
        stdat <- strata
        stdo <- TRUE
    }
    
    # Check that nboot is not less than 2...
    if(nboot<2){
        stop('nboot cannot be less than 2!')
    }
    
    # Attempt to hog all except one core if it isn't specified
    if(para && is.nan(ncores)){
        ncores <- parallel:::detectCores()
        if(ncores>1){ncores <- ncores-1}
    } else {
        if(ncores<1){stop('You cannot use less than 1 core for parallel processing!')}
        ncores <- floor(ncores)
    }
    
    # Figure out what to do about parallel processing
    iswindows <- FALSE
    if(para){
        os <- as.character(Sys.info()['sysname'])
        if(is.na(match(tolower(os), 'windows'))) {
            paramode <- 'multicore'
        } else {
            paramode <- 'snow'
            iswindows <- TRUE
        }
    } else {
        paramode = 'no'
    }
    
    # Get data out of frfit and setup output
    moddata <- data.frame('Y'=frfit$y, 'X'=frfit$x)
    response <- frfit['response']
    if(length(frfit$optimvars)==0){start <- NULL} else {start <- as.list(coef(frfit)[frfit$optimvars])}
    if(length(frfit$fixedvars)==0){fixed <- NULL} else {fixed <- as.list(coef(frfit)[frfit$fixedvars])}
    # Clear the old fit and sample object...
    frfit[c('sample', 'fit')] <- NULL
    out <- frfit
    class(out)[class(out)=='frfit'] <- 'frboot'
    
    # Print some output to calm people's nerves!
    cat('BOOTSTRAPPING.\n')
    if(frfit$response%in%c('rogersII')){
        cat(paste0('NB: This function calls the lambertW function, so this might take a while. Please be patient.'))
    }
    flush.console()
    
    ## Go! ##
    # Get the response...
    frfunc <- get(unlist(fr_responses()[[frfit$response]])[1])
    # Do it!
    if(stdo){
        frout <- boot(data=moddata, statistic=frfunc, R=nboot, start=start, fixed=fixed, strata=stdat, boot=TRUE, windows=iswindows, parallel=paramode, ncpus=ncores)
    } else {
        frout <- boot(data=moddata, statistic=frfunc, R=nboot, start=start, fixed=fixed, boot=TRUE, windows=iswindows, parallel=paramode, ncpus=ncores)
    }
    ## NB: Slightly different calls, depending on if stratified bootstrapping is requested. This prevents print.boot form thinking it is a stratified bootstrap, even if it isn't (evidently is uses the call to determine this, so gets confused, even if you pass a vector of 1's!)
    
    if(nrow(frout$t)!=nboot){
        stop("Bootstrap function didn't return nboot rows. This should be impossible!")
    }
    
    # Add the (bootstrapped) coefficients to the output
    out[['bootcoefs']] <- as.matrix(frout$t[,match(names(coef(frfit)), names(frout$t0))])
    dimnames(out[['bootcoefs']]) <- list(NULL, names(frout$t0[match(names(coef(frfit)), names(frout$t0))]))
    # Add sample
    out[['sample']] <- frout$t[,names(frout$t0)=='']
    # Add some bootstrap performance information
    out[['n_failed']] <- sum(is.na(frout$t[,1]))
    out[['n_duplicated']] <- sum(duplicated(out[['sample']]))
    out[['n_boot']] <- nboot
    out[['stratified']] <- stdo
    # Finally, add the fit to the output (in case people need it)
    out[['fit']] <- frout

    # For bootstrapped data, we need to check the number of failures
    prop_fail <- out[['n_failed']]/out[['n_boot']]
    if(prop_fail>0.5){
        if(WARN.ONLY){
            warning('More than 50% of the fits failed. This should be an error, but you have deliberately suppressed it with WARN.ONLY=TRUE.')
        } else {
            out <- NA
            stop('More than 50% of the fits failed. This is an error.  Nothing will be returned.')
        }
    } else if(prop_fail>0.1) {
        warning('More than 10% of the fits failed. Suggest careful consideration of the output.')
    }
# Finally, return the booted object
return(out)
}