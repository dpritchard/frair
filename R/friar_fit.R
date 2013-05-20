## frair_fit
# Wrapper function to fit functional response curves
# The workhorse of the frair package
frair_fit <- function(formula, data, response, start=list(), fixed=list(), strata=NULL, boot=FALSE, nboot=1500, para=TRUE, ncores=NaN, WARN.ONLY=FALSE){
	# Parse call, can check formula...
	call <- match.call()
	mf <- match.call(expand.dots = FALSE)
	mf_list <- as.list(mf)
	expandmod <- terms(formula(mf_list$formula), data=data)
	expandform <- formula(expandmod)
	leftside <- all.vars(expandform[[2]])
	rightside <- all.vars(expandform[[3]])
	if(length(leftside)!=1 || length(rightside)!=1) {
		stop('Currently only formulae with one dependent and one independent variable (e.g. y ~ x) are supported.')
	}
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    # moddata is the data we need for the fitting...
    moddata <- eval.parent(mf)
    names(moddata) <- c('Y', 'X')
    
    # Plausibly, 'response' might be the function itself, which isn't helpful at this point.
    if(is.function(response)){
    	response <- as.character(mf_list$response)
    }
    
    # Check that we can get strata from the data. It needs to be a factor
    stdat=rep(1,nrow(data))
    if(!is.null(strata)){
        if(!is.character(strata) | !match(strata, names(data), nomatch=FALSE)){
            stop('Strata must be a charater string and denote a column in the data')
        }
        stdat <- as.factor(data[,strata])
    }
    
    # Check we can deal with the requested response
    resp_known <- names(fr_responses())
    resp_check <- match(response, resp_known, 0L)
    if(resp_check==0){
    	stop("'response' not recognised. Use fr_responses(show=T) to see what has been implemented.")
    }
    
    # Check start
    if(length(start)==0){
    	stop("You didn't provide starting values. It's impossible to fit anything without knowing what to optimise!")
    }
    if(any(lapply(start, length)!=1)){
    	stop("The items in start must be named numeric value of length one.")
    }
    if(!(all(is.numeric(unlist(start))))){
    	stop("The items in start must be named numeric values.")
    }
    
    # Check fixed
    if(length(fixed)>0 && any(lapply(fixed, length)!=1)){
    	stop("The items in fixed must be named numeric value of length one.")
    }
    if(length(fixed)>0 && !(all(is.numeric(unlist(fixed))))){
    	stop("The items in fixed must be named numeric values.")
    }
   
    # Check we have everything we need
    req_input <- names(formals(response))
    req_input  <- req_input[req_input!='X']
    input_matches <- match(req_input, c(names(start), names(fixed)), NA)
    if(any(is.na(input_matches))){
    	missing_input <- req_input[is.na(input_matches)]
    	if(length(missing_input)>1){
    		stop(paste("Your requested response function requires input: ", paste(req_input, collapse=', '), ".\n  The following items are missing: ", paste(missing_input, collapse=', '), ".\n  Please provide them via 'start' or 'fixed', as appropriate.", sep=''))
    	} else {
    		stop(paste("Your requested response function requires input: ", paste(req_input, collapse=', '), ".\n  The following item is missing: ", paste(missing_input, collapse=', '), ".\n  Please provide it via 'start' or 'fixed', as appropriate.", sep=''))
    	}
    }
    
    ## Go time!
    # Common output
    out <- list('call' = call, 'x' = moddata$X, 'y'=moddata$Y, 'response'=response, 'xvar' = rightside, 'yvar' = leftside, optimvars=names(start), fixedvars=names(fixed))
    
    ## Bootstrapping ##
	if(boot){
    	# Setup output
    	class(out) <- c('frboot', class(out))
  		
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
		
		# Attempt to hog all cores if it isn't specified
        if(para && is.nan(ncores)){
            ncores <- parallel:::detectCores()
        } else {
            ncores <- floor(ncores)
        }
    	
    	# Print some output to calm people's nerves!
    	cat('\nNow bootstrapping.  Please be patient...\n\n')
    	flush.console()
    	
    	## Case specific fitting...
    	# rogersII
    	if(response=='rogersII'){
    		frout <- boot(data=moddata, statistic=rogersII_fit, R=nboot, start=start, fixed=fixed, strata=stdat, boot=TRUE, windows=iswindows, parallel=paramode, ncpus=ncores)
    		if(nrow(frout$t)!=nboot){
                stop("Bootstrap function didn't return nboot rows. This should be impossible!")
    		}
		# Generic Type I
		} else if(response=='typeI'){
    		frout <- boot(data=moddata, statistic=typeI_fit, R=nboot, start=start, fixed=fixed, strata=stdat, boot=TRUE, windows=iswindows, parallel=paramode, ncpus=ncores)
    		if(nrow(frout$t)!=nboot){
                stop("Bootstrap function didn't return nboot rows. This should be impossible!")
    		}
    	# No function
    	} else {
    		stop('Unknown function.  This should be impossible!')
    	}
    	## End case specific fitting
        
    	# Add the (original) coefficients to the output
    	out[['coefficients']] <- frout$t0[req_input]
        # Add the (bootstrapped) coefficients to the output
    	out[['bootcoefs']] <- as.matrix(frout$t[,match(req_input, names(frout$t0))])
        dimnames(out[['bootcoefs']]) <- list(NULL, names(frout$t0[match(req_input, names(frout$t0))]))
        # Add sample
    	out[['sample']] <- frout$t[,names(frout$t0)=='']
        # Add some bootstrap performance information
    	out[['n_failed']] <- sum(is.na(frout$t[,1]))
    	out[['n_duplicated']] <- sum(duplicated(out[['sample']]))
    	out[['n_boot']] <- nboot
    	# Finally, add the fit to the output (in case people need it)
    	out[['fit']] <- frout
	
    ## Not bootstrapping ##
    } else {
    	# Setup output
    	class(out) <- c('frfit', class(out))
    	
    	# In this instance, the sample is just the data itself...
    	samp=c(1:nrow(moddata))
    	
    	## Case specific fitting...
    	# rogersII
    	if(response=='rogersII'){
    		frout <- rogersII_fit(data=moddata, samp=c(1:nrow(moddata)), start=start, fixed=fixed)
    	# Generic Type I
    	} else if(response=='typeI'){
    		frout <- typeI_fit(data=moddata, samp=c(1:nrow(moddata)), start=start, fixed=fixed)
    	# No function
    	} else {
    		stop('Unknown function.  This should be impossible!')
    	}
    	## End case specific fitting...
        
        # Add the coefficients to the output
        out[['coefficients']] <- frout$out[req_input]
    	# Add the sample, in this case, just a simple vector
    	out[['sample']] <- samp
        # Finally, add the fit to the output (in case people need it)
    	out[['fit']] <- frout$fit
    }
    
    # For bootstrapped data, we need to check the number of failures
    if(inherits(out, 'frboot')){
    	prop_fail <- out[['n_failed']]/out[['n_boot']]
    	if(prop_fail>0.5){
    		out <- NA
            if(WARN.ONLY){
                warning('More than 50% of the fits failed. This is an error.  Nothing will be returned.')
            } else {
                stop('More than 50% of the fits failed. This is an error.  Nothing will be returned.')
            }
    	} else if(prop_fail>0.1) {
    		warning('More than 10% of the fits failed. Suggest careful consideration of the output.')
    	}
    }
    # Finally return our object!
    return(out)
}