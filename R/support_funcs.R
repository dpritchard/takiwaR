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

make_key <- function(string, subs = "takR"){
    key <- as.character(string)
    key <- stringr::str_trim(key)
    key <- stringr::str_replace_all(key, "\\W", "_") # Find anything that is not a word and replace
    key <- stringr::str_replace_all(key, "\\_{2,}", "_") # Replace multiple underscores
    key <- stringr::str_trim(key)
    #key <- make.names(key)
    key <- make.unique(key, sep="_")
    key <- stringr::str_to_lower(key)
    key <- stringr::str_replace(key, "^\\_", paste0(subs, "_")) # Replace underscores at the beginning with takR_
    key <- stringr::str_replace(key, "^(\\d)", paste0(subs, "_\\1")) # Replace numbers at the beginning with takR_#
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

set_par <- function(brewer.n=8, brewer.name="Dark2", cex.lab=1, cex.main=1.2, cex.axis=1, mar=c(2.5,2.5,1.6,1.1), mgp=c(1.5,.5,0), las=1, ...){
    par(mar=mar, mgp=mgp, cex.lab=cex.lab, cex.main=cex.main, cex.axis=cex.axis, las=las)
    par(...)
    pal <- c("#000000", RColorBrewer::brewer.pal(brewer.n, brewer.name))
    palette(pal)
}

set_large_par <- function(brewer.n=8, brewer.name="Dark2", cex.lab=2, cex.main=2, cex.axis=1.5, mar=c(5.1,5.1,3.5,2.1), mgp=c(3,1,0), las=1, ...){
    par(mar=mar, mgp=mgp, cex.lab=cex.lab, cex.main=cex.main, cex.axis=cex.axis, las=las)
    par(...)
    pal <- c("#000000", RColorBrewer::brewer.pal(brewer.n, brewer.name))
    palette(pal)
}

setup_plot <- function(x, y, yerr=NA, xlab="", ylab="", ...) {
    xvals <- c(min(x, na.rm = T), max(x, na.rm = T))
    yvals <- c(min(y, na.rm = T), max(y, na.rm = T))
    if(!all(is.na(yerr))){
        yvals[1] <- yvals[1]-yerr
        yvals[2] <- yvals[2]+yerr
    } 
    plot(xvals, yvals, axes=F, type="n", xlab=xlab, ylab=ylab, ...)
    box()
    axis(1, lwd = 0, lwd.ticks = 1)
    axis(2, lwd = 0, lwd.ticks = 1)
}
