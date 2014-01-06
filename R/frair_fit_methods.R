## frair methods

# Print for objects of class frfit
print.frfit <- function(x, ...){
    cat('FUNCTIONAL RESPONSE FIT\n')
    cat(paste0('\nResponse:            ', x$response))
    cat(paste0('\nDescription:         ', as.character(frair_responses(show=FALSE)[[x$response]][2])))
    cat(paste0('\nOptimised variables: ', paste0(x$optimvars, collapse=', ')))
    cat(paste0('\nFixed variables:     ', ifelse(test=!is.null(x$fixedvars), yes=paste(x$fixedvars, collapse=', '), no='NA')))
    cat('\n')
    cat('\nCoefficients:\n')
    print(round(x$coefficients, 3))
    cat('\nNOTE: It is recomended you inspect the raw fit too (see: ?frair_fit)')
}

plot.frfit <- function(x, xlab=x$xvar, ylab=x$yvar, ...){
    plot(x$x, x$y, xlab=xlab, ylab=ylab, ...)
}

lines.frfit <- function(x, ...){
    newx <- seq(from=1, to=max(x$x), by=1)
    fitfun <- get(x$response)
    newy <- fitfun(newx, as.list(x$coefficients))
    lines(x=newx, y=newy, ...)
}