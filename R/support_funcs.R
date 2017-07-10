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

rnt <- function(min = 0, max = 30, by = 1, nrow = 10, ncol = 10, 
                rowpref = "T", colpref = "Q"){
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

map_attributes <- function(x, out, regex = "^(takRmeta_|takRsec_)") {
    # Add back attributes beginning with "takRmeta_" or "takRsec_" from `x` to `out`
    indx <- stringr::str_detect(names(attributes(x)), regex)
    if(any(indx)){
        attr_names <- names(attributes(x))[indx]
        for(a in 1:length(attr_names)){
            attr(out, attr_names[a]) <- attr(x, attr_names[a])
        }
    }
    return(out)
}

sum_on_col <- function(x){
    # Sum columns with matching names
    # This works equally well if x is a list becuase melt operates by melting the components of a list...
    x_m <- reshape2::melt(x, as.is=TRUE)
    x_m[is.na(x_m$value)] <- 0
    # Cast the data as an array that is row-by-col, summing accross matches....
    x_out <- reshape2::acast(x_m, Var1~Var2, fun.aggregate=sum)
    if(inherits(x, "takRwide")){
        # If it came in as a takRwide, send it back as the same....
        class(x_out) <- class(x)
    } else {
        # Otherwise, send back a takRwide object
        class(x_out) <- c("takRwide", class(x_out))
    }
    # Map any "takR*" attributes
    x_out <- map_attributes(x, x_out)
    return(x_out)
}