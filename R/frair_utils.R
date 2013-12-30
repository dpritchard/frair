## Functional Response Functions 
# Each function specification needs to be listed in 'resp_known' with a description.
## resp_known is the master list of usable functions.
## each named entry here must have a corresponding function entry (e.g. fr_rogersII.R) and vice versa!
fr_responses <- function(show=FALSE){
    # resp_known: NAME = list(FIT_FUN, DESCRIPTION, USES_LAM_W)
    resp_known <- list(
        "typeI"=list("typeI_fit", "A generic linear (type I) response", FALSE),
        "hollingsII"=list("hollingsII_fit", "Holling's orginal type II function", FALSE),
        "rogersII"=list("rogersII_fit", "Roger's type II decreasing prey function", TRUE), 
        "bdII"=list("bdII_fit", "Beddington–DeAngelis type II function", TRUE))
    
    if(show){
        C1 <- c('Response',names(resp_known))
        C2 <- 'Fit Function'
        C3 <- 'Parameters'
        C4 <- 'Description'
        
        for (a in 1:length(resp_known)) { 
            C2 <- c(C2, unlist(resp_known[[a]][1]))
            C3 <- c(C3, paste(names(formals(fun=get(names(resp_known)[a]))), collapse=','))
            C4 <- c(C4, unlist(resp_known[[a]][2]))
        }
        C1n <- max(sapply(C1,nchar))+4
        C2n <- max(sapply(C2,nchar))+4
        C3n <- max(sapply(C2,nchar))+4
        C4n <- max(sapply(C4,nchar))+4
        # Print
        cat(format(C1[1], width=C1n), format(C2[1], width=C2n), format(C3[1], width=C3n), format(C4[1], width=C4n), '\n', sep='')
        cat(format(paste(rep('-',C1n-4), collapse=''), width=C1n),format(paste(rep('-',C2n-4), collapse=''), width=C2n), format(paste(rep('-',C3n-4), collapse=''), width=C3n), format(paste(rep('-',C4n-4), collapse=''), width=C4n), '\n', sep='')
        for(a in 2:length(C1)){
            cat(format(C1[a], width=C1n),format(C2[a], width=C2n), format(C3[a], width=C3n), format(C4[a], width=C4n), '\n', sep='')
        }
        cat('\n')
    } else {
        return(resp_known)
    }
}

# fr_setupout
# Utility function to help clean up the code in the various statistic functions
# NB: This setups a single vector, as used by the various dual-duty functional response functions (e.g. rogersII_fit) NOT the output of frair_fit
# Returns a vector of (correctly) named NA's, based on the start and fixed input to the statitic function
fr_setupout <- function(start, fixed, samp){
    out <- c(rep(NA, times=length(names(start))*2), rep(NA, times=length(names(fixed))), samp)
    outnames <- NULL
    # Setup optimised variable output
    if(!is.null(start)){
        for (i in 1:length(names(start))){
            outnames <- c(outnames, names(start)[i], paste(names(start)[i], 'var', sep=''))
        }
    }
    # Setup fixed variable output
    if(!is.null(fixed)){
        for (i in 1:length(names(fixed))){
            outnames <- c(outnames, names(fixed)[i])
        }
    }
    names(out) <- c(outnames, rep('', times=length(samp)))
    return(out)
}

# fr_setpara parallel
# Utility to setup parallel processing in Windows
fr_setpara <- function(boot, windows){
		if(boot && windows){
		emdbook_load <- require(emdbook, warn.conflicts=FALSE, quietly=TRUE)
		bbmle_load <- require(bbmle, warn.conflicts=FALSE, quietly=TRUE)
		if(any(c(emdbook_load, bbmle_load)==FALSE)){
			stop('Error establishing workspace for parallel computing in Windows.')
		}
	}
}

## The startup method
.onAttach <- function(lib, pkg)  {
    packageStartupMessage('This is the development version of frair (v. ', utils::packageDescription("frair", field="Version"), ')', appendLF = TRUE)
}
