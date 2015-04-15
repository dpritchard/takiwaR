cm2in <- function(x){
    cm <- x*0.393701
    return(cm)
}

error_bar <- function(x, y, upper, lower=upper, length=0.05, dir='y',...){
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

length2 <- function(x, na.rm=T){
    if(na.rm){
        return(length(na.omit(x)))
    } else {
        return(length(x))
    }
}


all_is_na <- function(x){
    allis <- all(is.na(x))
    return(allis)
}