cm2in <- function(x){
    cm <- x*0.393701
    return(cm)
}

compare <- function(v){
    first <- v[1]
    rest <- as.list(v[-1])
    res <- sapply(rest, FUN=function(z){ identical(z, first) })
    return(all(res))
}

make_key <- function(string, subs = "takR_"){
    key <- as.character(string)
    key <- stringr::str_trim(key)
    key <- stringr::str_replace_all(key, "\\W", "_") # Find anything that is not a word and replace
    key <- stringr::str_replace_all(key, "\\_{2,}", "_") # Replace multiple underscores
    key <- stringr::str_trim(key)
    #key <- make.names(key)
    key <- make.unique(key, sep="_")
    key <- stringr::str_to_lower(key)
    key <- stringr::str_replace(key, "^\\_", paste0(subs, "_")) # Replace underscores at the beginning with TAK_
    key <- stringr::str_replace(key, "^(\\d)", paste0(subs, "_\\1")) # Replace numbers at the beginning with TAK_#
    return(key)
}

error_bar <- function(x, y, upper, lower=upper, length=0.05, dir='y', ...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("Vectors must be same length")
    if(all(is.factor(x))){
        x <- as.numeric(x)
    }
    if(dir=='y'){
        suppressWarnings(arrows(x,y+upper,x,y-lower, angle=90, code=3, length=length, ...))
    } else if(dir=='x') {
        suppressWarnings(arrows(x-lower,y,x+upper,y, angle=90, code=3, length=length, ...))
    } else {
        stop('Impossible')
    }
}

se <- function(x, na.rm=FALSE){
    if (!is.vector(x)){
        stop("'x' must be a vector. See ?se for further information.")
    }
    if(na.rm){
        se <- sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))
    } else {
        se <- sd(x)/sqrt(length(x))
    }
    return(se)
}

rnt <- function(min = 0, max = 30, by = 1, nrow = 10, ncol = 10, rowpref = "T", colpref = "Q"){
    samples <- NULL
    for(a in 1:nrow){
        s1 <- sort(sample(seq(from = min, to = max, by = by), ncol))
        samples <- rbind(samples, s1)	
    }
    rownames(samples) <- paste0("T", 1:nrow)
    colnames(samples) <- paste0("Q", 1:ncol)
    return(samples)
}

is_zero <- function(x, strict_int = FALSE){
    if(strict_int){
        return(x == 0L)
    } else {
        return(abs(x) <= .Machine$double.eps)
    }
}

all_is_na <- function(x){
    allis <- all(is.na(x))
    return(allis)
}

mtexti <- function(text, side, off = 0.25,
                   srt = if(side == 2) 90  else
                       if(side == 4) 270 else 0, ...) {
    # dimensions of plotting region in user units
    usr <- par('usr')
    # dimensions of plotting region in inches
    pin <- par('pin')
    # user units per inch
    upi <- c(usr[2]-usr[1],
             usr[4]-usr[3]) / pin
    # default x and y positions
    xpos <- (usr[1] + usr[2])/2
    ypos <- (usr[3] + usr[4])/2
    if(1 == side)
        ypos <- usr[3] - upi[2] * off
    if(2 == side)
        xpos <- usr[1] - upi[1] * off
    if(3 == side)
        ypos <- usr[4] + upi[2] * off
    if(4 == side)
        xpos <- usr[2] + upi[1] * off
    text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
}