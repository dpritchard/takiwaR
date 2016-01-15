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

mtexti <- function(text, side, off = 0.25, srt = NULL, ...) {
    if(is.null(srt)){
        srt <- if(side == 2) 90 else 
            if(side == 4) 270 else 
                0
    }
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

set_par <- function(brewer.n=8, brewer.name="Dark2", 
                    cex.lab=1, cex.main=1.2, cex.axis=1, 
                    mar=c(2.5,2.5,1.6,1.1), mgp=c(1.5,.5,0), las=1, ...){
    par(mar=mar, mgp=mgp, cex.lab=cex.lab, cex.main=cex.main, cex.axis=cex.axis, las=las)
    par(...)
    pal <- c("#000000", RColorBrewer::brewer.pal(brewer.n, brewer.name))
    palette(pal)
}

setup_plot <- function(x, y, yerr = NA, xlab = "", ylab = "", ...) {
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

theme_takiwaR <- function(base_size=12, base_family="") {
    grey <- "#737373"
    black <- "#000000"
    theme_bw(base_size=base_size, base_family=base_family) +
        theme(
            line = element_line(colour = grey),
            rect = element_rect(fill = "white", colour = NA),
            text = element_text(colour = black),
            axis.ticks = element_line(colour = black),
            legend.key = element_rect(colour = NA),
            ## Examples do not use grid lines
            panel.border = element_rect(colour = black),
            panel.grid = element_blank(),
            plot.background=element_blank(),
            strip.background = element_rect(fill="white", colour=NA)
        )
}