## Functional Response Functions 
# Each function specification needs to be listed in 'resp_known' with a description.
## resp_known is the master list of usable functions.
## each named entry here must have a corresponding function entry (e.g. fr_rogersII.R) and vice versa!
fr_responses <- function(show=FALSE){
    resp_known <- list("typeI"="A generic linear type I response.", 
                       "rogersII"="Roger's type II decreasing prey function.")
    if(show){
        C1 <- c('Response',names(resp_known))
        C2 <- 'Parameters'
        C3 <- c('Description',as.character(unlist(resp_known)))
        for (a in 1:length(resp_known)) { 
            C2 <- c(C2, paste(names(formals(fun=get(names(resp_known)[a]))), collapse=','))
        }
        C1n <- max(sapply(C1,nchar))+4
        C2n <- max(sapply(C2,nchar))+4
        C3n <- max(sapply(C2,nchar))+4
        # Print
        cat(format(C1[1], width=C1n),format(C2[1], width=C2n), format(C3[1], width=C3n), '\n', sep='')
        cat(format(paste(rep('-',C1n-4), collapse=''), width=C1n),format(paste(rep('-',C2n-4), collapse=''), width=C2n), format(paste(rep('-',C3n-4), collapse=''), width=C3n), '\n', sep='')
        for(a in 2:length(C1)){
            cat(format(C1[a], width=C1n),format(C2[a], width=C2n), format(C3[a], width=C3n), '\n', sep='')
        }
        cat('\n')
    } else {
        return(resp_known)
    }
}

## Plotting Code
fr_plot <- function(fr_model, plotboot=FALSE, bootlines=TRUE, alpha=NaN, ...){
    xdat <- fr_model$data[,fr_model$xvar]
    newx <- seq(from=0, to=max(xdat), by=0.1)
    if(inherits(fr_model, 'fr_fit')){
        # 'Normal' Curve Drawing
        if(fr_model$response=='rogersII'){
            newy <- rogersII(newx, a=fr_model$a0, h=fr_model$h0, P=fr_model$fit@data$P, T=fr_model$fit@data$T)
        } else if(fr_model$response=='typeI'){
            newy <- typeI(newx, c=fr_model$c0)
        } else {
            stop('Impossible')
        }
        lines(newx, newy, ...)
    } else if(inherits(fr_model, 'fr_boot')){
        # Setup Alpha
        if(is.nan(alpha)){
            alpha<-1/sqrt(fr_model$n_boot)
        }
        # Bootstrap lines
        if(fr_model$response=='rogersII'){
            #newy <- NULL
            #bootx <- NULL
            if(plotboot){
                fitteda <- na.omit(fr_model$a)
                fittedh <- na.omit(fr_model$h)
                outdd <- matrix(nrow=length(newx), ncol=length(fitteda))
                for(a in 1:length(fitteda)){
                    ## Hard codes ones! Bad! Bad! Bad!
                    outdd[,a] <- rogersII(newx, a=fr_model$a[a], h=fr_model$h[a], P=1, T=1)
                }
                if(bootlines){
                    # Plot bootstrapped lines
                    for(a in 1:ncol(outdd)){
                        lines(newx, outdd[,a], col=rgb(0,0,0,alpha), ...)
                    }
                } else {
                    # Plot patches ## Hardcoded 95%CI - TODO FIXME!
                    dd <- apply(outdd, 1, quantile, na.rm=T, probs = c(0.025, 0.975))
                    polygon(c(newx, rev(newx), newx[1]), c(dd[1,], rev(dd[2,]), dd[1,1]), border=NA, ...)
                }
            } else {
                ## Hard codes ones! Bad! Bad! Bad! TODO FIXME!
                newy <- rogersII(newx, a=fr_model$a0, h=fr_model$h0, P=1, T=1)
                lines(newx, newy, ...)
            } 
        } else {
            stop('Impossible')
        }
    }
}