## Functional Response Functions 
# Each function specification needs to be listed in 'fr_resp_known' with a description.
## fr_resp_known is the master list of usable functions.
## each named entry here must have a corresponding function entry (e.g. fr_rogersII.R) and vice versa.

frair_responses <- function(show=TRUE){
    fr_resp_known <- list(
        "typeI"=list("typeI_fit", "A generic linear (type I) response", FALSE, 'a'),
        "hollingsII"=list("hollingsII_fit", "Holling's orginal type II function", FALSE, c('a','h')),
        "rogersII"=list("rogersII_fit", "Roger's type II decreasing prey function", TRUE, c('a','h')), 
        "emdII"=list("emdII_fit", "Ecological Models and Data in R type II function", TRUE, c('a','h')),
        "hassIII"=list("hassIII_fit", "Hassell's original type III function (assuming replacement)", FALSE, c('b','c','h')),
        "hassIIIr"=list("hassIIIr_fit", "Hassell's type III function (not assuming replacement)", TRUE, c('b','c','h')),
        "real77"=list("real77_fit", "Real (1977) curve (assuming replacement)", FALSE, c('b','q','h')),
        "real77r"=list("real77r_fit", "Real (1977) curve (not assuming replacement)", TRUE, c('b','q','h'))
    )
    # "bdII"=list("bdII_fit", "Beddington-DeAngelis type II function", TRUE, c('a','h')),
    if(show){
        C1 <- c('Response',names(fr_resp_known))
        C2 <- 'Parameters'
        C3 <- 'Description'
        
        for (a in 1:length(fr_resp_known)) { 
            C2 <- c(C2, paste(names(formals(fun=get(names(fr_resp_known)[a]))), collapse=','))
            C3 <- c(C3, unlist(fr_resp_known[[a]][2]))
        }
        pad <- 2
        C1n <- max(sapply(C1,nchar))+pad
        C2n <- max(sapply(C2,nchar))+pad
        C3n <- max(sapply(C3,nchar))+pad
        # Print
        cat(format(C1[1], width=C1n), format(C2[1], width=C2n), 
            format(C3[1], width=C3n), '\n', sep='')
        cat(format(paste(rep('-',C1n-pad), collapse=''), width=C1n),
            format(paste(rep('-',C2n-pad), collapse=''), width=C2n), 
            format(paste(rep('-',C3n-pad), collapse=''), width=C3n), '\n', sep='')
        
        for(a in 2:length(C1)){
            cat(format(C1[a], width=C1n),format(C2[a], width=C2n), 
                format(C3[a], width=C3n), '\n', sep='')
        }
        cat('\n')
    } 
    invisible(fr_resp_known)
}
