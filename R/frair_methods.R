## frair methods

# Print for objects of class frfit
print.frfit <- function(x, ...){
    cat('FUNCTIONAL RESPONSE FIT\n')
    cat(paste0('\nResponse:            ', x$response))
    cat(paste0('\nDescription:         ', as.character(fr_responses()[x$response])))
    cat(paste0('\nOptimised variables: ', paste0(x$optimvars, collapse=', ')))
    cat(paste0('\nFixed variables:     ', ifelse(test=!is.null(x$fixedvars), yes=paste(x$fixedvars, collapse=', '), no='NA')))
    cat('\n')
    cat('\nCoefficients:\n')
    print(round(x$coefficients, 3))
    cat('\nNOTE: It is recomended you inspect the raw fit too (see: ?frair_fit)')
}

print.frboot <- function(x, ...){
    nbootdone <- x$n_boot-x$n_failed
    percsuc <- round(nbootdone/x$n_boot*100,2)
    
    cat('\nBOOTSTRAPPED FUNCTIONAL RESPONSE FIT\n')
    cat(paste0('\nResponse:            ', x$response))
    cat(paste0('\nDescription:         ', as.character(fr_responses()[[x$response]][2])))
    cat(paste0('\nOptimised variables: ', paste0(x$optimvars, collapse=', ')))
    cat(paste0('\nFixed variables:     ', ifelse(test=!is.null(x$fixedvars), yes=paste0(x$fixedvars, collapse=', '), no='NA')))
    cat(paste0('\nBootstrap type:      ', ifelse(test=x$stratified, yes='Stratified', no='Ordinary')))
    cat(paste0('\nFit success:         ', percsuc, '% (', nbootdone, ' of ', x$n_boot, ')', sep=''))
    cat(paste0('\nDuplicated fits:     ', x$n_duplicated))
    cat('\n')
    cat('\nCoefficients (original data):\n')
    print(round(x$coefficients,3))
    cat('\n95% bootstrapped confidence intervals (for more info, see ?confint.frboot):\n')
    btconf <- confint(x)
    
    print(round(btconf,4))
    cat('\nNOTE: It is recomended you inspect the raw fit too (see: ?frair_boot)')
}

plot.frfit <- function(x, xlab=x$xvar, ylab=x$yvar, ...){
    plot(x$x, x$y, xlab=xlab, ylab=ylab, ...)
}

plot.frboot <- function(x, xlab=x$xvar, ylab=x$yvar, ...){
    plot(x$x, x$y, xlab=xlab, ylab=ylab, ...)
}

lines.frfit <- function(x, ...){
    newx <- seq(from=0, to=max(x$x), by=1)
    fitfun <- get(x$response)
    newy <- fitfun(newx, as.list(x$coefficients))
    lines(newx, newy, ...)
}

lines.frboot <- function(x, all_lines=FALSE, bootcol=1, bootalpha=1/sqrt(x$n_boot), ...){
    newx <- seq(from=0, to=max(x$x), by=1)
    fitfun <- get(x$response)
    if(!all_lines){
        # Plot the mean (original) fit
        newy <- fitfun(newx, as.list(x$coefficients))
        lines(newx, newy, ...)
    } else {
        # Plotting bootlines
        # Sort out colour
        if(is.vector(bootcol) && match(length(bootcol),c(3,4),nomatch=0)){
            # Assumed to be RGB
            bootcol[4] <- bootalpha
        } else {
            # Assumed to be another colour spec.
            bootcol <- col2rgb(bootcol, alpha=T)[,1]/255
            bootcol[4] <- bootalpha
        }
        
        bootcoefs <- na.omit(x$bootcoefs)
        outdd <- matrix(ncol=length(newx), nrow=nrow(bootcoefs))
        
        cat('\nPlotting bootlines. Please be patient...\n\n')
        flush.console()
        for(a in 1:nrow(bootcoefs)){
            outdd[a,] <- fitfun(newx, as.list(as.list(bootcoefs[a,])))
        }
        for(a in 1:nrow(outdd)){
            lines(x=newx, y=outdd[a,], col=rgb(bootcol['red'], bootcol['green'], bootcol['blue'], bootcol['alpha']), ...)
        }
    }
}

drawpoly <- function(x, ...) UseMethod("drawpoly")

drawpoly.default <- function(x, ...){
    polygon(x, ...)
}

drawpoly.frboot <- function(x, probs=c(0.025, 0.975), ...){
    newx <- seq(from=0, to=max(x$x), by=1)
    fitfun <- get(x$response)
    bootcoefs <- na.omit(x$bootcoefs)
    outdd <- matrix(ncol=length(newx), nrow=nrow(bootcoefs))
    
    cat('\nCalculating polygons. Please be patient...\n\n')
    flush.console()
    for(a in 1:nrow(bootcoefs)){
        outdd[a,] <- fitfun(newx, as.list(as.list(bootcoefs[a,])))
    }
    
    dd <- apply(outdd, 2, quantile, na.rm=T, probs=probs)
    polygon(x=c(newx, rev(newx), newx[1]), y=c(dd[1,], rev(dd[2,]), dd[1,1]), ...)
}

confint.frboot <- function(object, level=0.95, ...){
    ### SORT THIS OUT! NEED TO DO SOME TESTING ETC...
    # Get boot ci output for each optimvar, capturing warnings
    # Processes each one, as done by print.bootci()
    # Return a list, with all warnings etc bundled up.
    # Define a pring object for this item.
    optimnames <- object$optimvars
    frbcis <- matrix(nrow=5, ncol=length(optimnames)*2)
    cinames <- NULL
    for(x in 1:length(optimnames)){
        cinames <- c(cinames, paste(optimnames[x], '_lower', sep=''))
        cinames <- c(cinames, paste(optimnames[x], '_upper', sep=''))
    }
    citypes <- c("normal","basic", "student", "percent", "bca")
    dimnames(frbcis) <- list(citypes, cinames)
    
    for(x in 1:length(optimnames)){
        loc <- which(names(object$fit$t0)==optimnames[x])
        locvar <- which(names(object$fit$t0)==paste0(optimnames[1], 'var'))
        bootcivals <- boot.ci(object$fit, index=c(loc, locvar), conf=level, type='all')
        frbcis['normal', paste0(optimnames[x], '_lower')] <- bootcivals[['normal']][2]
        frbcis['normal', paste0(optimnames[x], '_upper')] <- bootcivals[['normal']][3]
        for(y in 2:length(citypes)){
            frbcis[citypes[y], paste0(optimnames[x], '_lower')] <- bootcivals[[citypes[y]]][4]
            frbcis[citypes[y], paste0(optimnames[x], '_upper')] <- bootcivals[[citypes[y]]][5]
        }
    }
    return(frbcis)
}