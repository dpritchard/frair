## Friar methods

print.frfit <- function(x, ...){
    cat('\nFUNCTIONAL RESPONSE FIT\n')
    cat(paste('\nResponse:           ', x$response))
    cat(paste('\nDescription:        ', as.character(fr_responses()[x$response])))
    cat(paste('\nOptimised variables:', paste(x$optimvars, collapse=', ')))
    cat(paste('\nFixed variables:    ', ifelse(test=!is.null(x$fixedvars), yes=paste(x$fixedvars, collapse=', '), no='NA')))
    cat('\n')
    
    cat('\nCoefficients:\n')
    print(x$coefficients)
    cat('\nNOTE: It is recomended you inspect the raw fit information too (contained in object$fit)')
}

print.frboot <- function(x, ...){
    cat('\nBOOTSTRAPPED FUNCTIONAL RESPONSE FIT\n')
    cat(paste('\nResponse:           ', x$response))
    cat(paste('\nDescription:        ', as.character(fr_responses()[x$response])))
    cat(paste('\nOptimised variables:', paste(x$optimvars, collapse=', ')))
    cat(paste('\nFixed variables:    ', ifelse(test=!is.null(x$fixedvars), yes=paste(x$fixedvars, collapse=', '), no='NA')))
    nbootdone <- x$n_boot-x$n_failed
    percsuc <- round(nbootdone/x$n_boot*100,2)
    cat(paste('\nFit success:         ', percsuc, '% (', nbootdone, ' of ', x$n_boot, ')', sep=''))
    cat(paste('\nDuplicated fits:    ', x$n_duplicated))
    cat('\n')
    cat('\nCoefficients (original data):\n')
    print(x$coefficients)
    cat(paste('\nNOTE: It is recomended you inspect the raw fit information too (contained in object$fit)', sep=''))
}

plot.frfit <- function(x, xlab=x$xvar, ylab=x$yvar, ...){
    plot(x$x, x$y, xlab=xlab, ylab=ylab, ...)
}

plot.frboot <- function(x, xlab=x$xvar, ylab=x$yvar, ...){
    plot(x$x, x$y, xlab=xlab, ylab=ylab, ...)
}

lines.frfit <- function(x, ...){
    newx <- seq(from=0, to=max(x$x), by=0.1)
    fitfun <- get(x$response)
    newy <- fitfun(newx, as.list(x$coefficients))
    lines(newx, newy, ...)
}

lines.frboot <- function(x, all=FALSE, col=1, alpha=1/sqrt(x$n_boot), ...){
    newx <- seq(from=0, to=max(x$x), by=0.1)
    fitfun <- get(x$response)
    if(!all){
        # Plot the mean (original) fit
        newy <- fitfun(newx, as.list(x$coefficients))
        lines(newx, newy, ...)
    } else {
        # Plotting bootlines
        # Sort out colour
        if(is.vector(col) & match(length(col),c(3,4))){
            # Assumed to be RGB
            col[4] <- alpha
        } else {
            # Assumed to be another colour spec.
            col <- col2rgb(col, alpha=T)
            col[4] <- alpha
        }
        
        bootcoefs <- na.omit(x$bootcoefs)
        outdd <- matrix(ncol=length(newx), nrow=nrow(bootcoefs))
        
        cat('\nPlotting bootlines. Please be patient...\n\n')
        flush.console()
        for(a in 1:nrow(bootcoefs)){
            outdd[a,] <- fitfun(newx, as.list(as.list(bootcoefs[a,])))
        }
        for(a in 1:nrow(outdd)){
            lines(newx, outdd[a,], col=col, ...)
        }
    }
}

polygon.frboot <- function(x, probs=c(0.025, 0.975), ...){
    newx <- seq(from=0, to=max(x$x), by=0.1)
    fitfun <- get(x$response)
    bootcoefs <- na.omit(x$bootcoefs)
    outdd <- matrix(ncol=length(newx), nrow=nrow(bootcoefs))
    
    cat('\nCalculating polygons. Please be patient...\n\n')
    flush.console()
    for(a in 1:nrow(bootcoefs)){
        outdd[a,] <- fitfun(newx, as.list(as.list(bootcoefs[a,])))
    }
    dd <- apply(outdd, 2, quantile, na.rm=T, probs = probs)
    polygon(c(newx, rev(newx), newx[1]), c(dd[1,], rev(dd[2,]), dd[1,1]), ...)
}
