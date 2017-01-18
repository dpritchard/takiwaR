cm2in <- function(x){
    cm <- x*0.393701
    return(cm)
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

# Add a named class to an object.
add_class <- function(x, class, prepend = TRUE){
    if(inherits(x, class)){
        return(x)
    }
    if(prepend){
        class(x) <- c(class, class(x))
    } else {
        class(x) <- c(class(x), class)
    }
    return(x)
}