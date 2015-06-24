## Methods
# bt_combine... Combine is for combining data from subquadrats into larger quadrats
# e.g. Low vis means divers are working small sections of a larger quadrat.
# The assumption is that there are equal numbers of (sub)quadrats in each dataset
bt_combine <- function(x1, x2, ...) UseMethod("bt_combine")

bt_combine.bt_raw <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    keys <- unique(unlist(lapply(all_inp, names)))
    # Merge each of the objects. Relies on Method dispatching
    out <- list()
    for(key in keys){
        key_dat <- lapply(all_inp, 'getElement', key)
        #print(str(key_dat))
        out[[key]] <- do.call(bt_combine, key_dat)
    }
    class(out) <- c("bt_raw", class(out))
    return(out)
}

bt_combine.bt_meta <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    # No need to remove empty sections... Meta sections should never be empty...
    keys <- unique(unlist(lapply(all_inp, names)))
    out_meta <- setNames(do.call(mapply, c(FUN=c, lapply(all_inp, '[', keys))), keys)
    # Handle some special cases...
    if('quad_size' %in% names(out_meta)){
        out_meta <- bt_set_meta(out_meta, list(
            "quad_size" = sum(out_meta[['quad_size']]))
        )
    }
    # Assuming all special cases return only 1 value, return just unique values
    out_meta <- lapply(out_meta, unique) # TODO make this nicer!
    out_meta <- bt_set_meta(out_meta, list("is_merged" = TRUE))
    class(out_meta) <- c("bt_meta", class(out_meta))
    return(out_meta)
}

bt_combine.bt_perc <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Needed for averaging, below
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='bt_empty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('bt_empty', 'bt_perc', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
#     for(a in 1:length(all_inp)){
#         if(inherits(all_inp[a], what = 'bt_empty')){
#             all_inp[a] <- NULL
#         }
#     }
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    # cbind all the data
    all_dat <- NULL
    for(a in 1:length(all_inp)){
        all_dat <- cbind(all_dat, all_inp[[a]])
    }
    # Convert NA's to zeros (OK for summing)
    all_dat[is.na(all_dat)] <- 0
    all_dat <- sapply(unique(colnames(all_dat)), function(x){
        rowSums(all_dat[, grep(x, colnames(all_dat)), drop=FALSE])}
    )
    #all_dat[all_dat==0] <- NA # Reconvert zeros to NA
    all_dat <- all_dat/original_length # Convert to a mean based on original input length
    class(all_dat) <- c("bt_perc", class(all_dat))
    return(all_dat)
}

bt_combine.bt_count <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Not really needed...
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='bt_empty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('bt_empty', 'bt_count', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    # cbind all the data
    all_dat <- NULL
    for(a in 1:length(all_inp)){
        all_dat <- cbind(all_dat, all_inp[[a]])
    }
    # Convert NA's to zeros (OK for summing)
    all_dat[is.na(all_dat)] <- 0
    all_dat <- sapply(unique(colnames(all_dat)), function(x){
        rowSums(all_dat[, grep(x, colnames(all_dat)), drop=FALSE])}
    )
    #all_dat[all_dat==0] <- NA # Reconvert zeros to NA
    class(all_dat) <- c("bt_count", class(all_dat))
    return(all_dat)
}

bt_combine.bt_sf <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Not really needed
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='bt_empty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('bt_empty', 'bt_sf', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    all_dat <- do.call(cbind, all_inp)
    class(all_dat) <- c("bt_sf", class(all_dat))
    return(all_dat)
}

# Summary
summary.bt_raw <- function(object, ...){
    out <- list('meta' = object[['meta']])
    keys <- names(object)
    keys <- keys[!keys=='meta']
    for(key in keys){
        out[[key]] <- summary(object[[key]])
    }
    class(out) <- c("bt_summary", class(out))
    return(out)
}

summary.bt_perc <- function(object, ...){
    if(inherits(object, what = "bt_empty")){
        out <- NA
        class(out) <- c("bt_empty", class(out))
        return(out)
    }
    dat_mean <- apply(object, 2, mean)
    dat_median <- apply(object, 2, median)
    dat_n <- apply(object, 2, length)
    dat_sd <- apply(object, 2, sd)
    dat_se <- dat_sd/sqrt(dat_n)
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    return(out)
}

summary.bt_count <- function(object, ...){
    if(inherits(object, what = "bt_empty")){
        out <- NA
        class(out) <- c("bt_empty", class(out))
        return(out)
    }
    dat_mean <- apply(object, 2, mean)
    dat_median <- apply(object, 2, median)
    dat_n <- apply(object, 2, length)
    dat_sd <- apply(object, 2, sd)
    dat_se <- dat_sd/sqrt(dat_n)
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    return(out)
}

summary.bt_sf <- function(object, ...){
    if(inherits(object, what = "bt_empty")){
        out <- NA
        class(out) <- c("bt_empty", class(out))
        return(out)
    }
    object <- object[!is.na(object)]
    dat_mean <- mean(object)
    dat_median <- median(object)
    dat_n <- length(na.omit(object))
    dat_sd <- sd(object)
    dat_se <- dat_sd/sqrt(dat_n)
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    rownames(out) <- c("All")
    return(out)
}

# Print
print.bt_empty <- function(x, ...){
    cat("< No Data >\n")
}

print.bt_meta <- function(x, ...){
    keys <- names(x)
    pad_len <- max(stringr::str_length(keys))+3
    for(key in keys){
        cat(stringr::str_pad(paste0(key, " : "), pad_len, side="left"), 
            paste0(x[[key]], collapse = ", "), "\n", sep="")
    }
}

print.bt_summary <- function(x, ...){
    keys <- names(x)
    #pad_len <- max(stringr::str_length(keys))+3
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
        print(x[[key]], ...)
    }
}

print.bt_raw <- function(x, ...){
    keys <- names(x)
    #pad_len <- max(stringr::str_length(keys))+3
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
        print(x[[key]], ...)
    }
}

print.bt_perc <- function(x, ...){
    print.table(x, na.print = "NA", ...)
}

print.bt_count <- function(x, ...){
    print.table(x, na.print = "NA", ...)
}

print.bt_sf <- function(x, ...){
    print.table(x, na.print = "", ...)
}

# bt_long
# Long generates a "long form" dataframe, columns: Quadrat, Variable, Value and one each for meta data
bt_long <- function(x, ...) UseMethod("bt_long")

bt_long.bt_meta <- function(x, nrow=1, ...){
    keys <- names(x)
    out  <- list()
    # Sort out duplicate keys
    for(key in keys){
        vals <- x[[key]]
        if(length(vals) > 1){ key <- paste0(key, "_", 1:length(vals))} # TODO unify "make keys" in takiwaR
        for(a in 1:length(vals)){
            out[[key[a]]] <- rep(vals[a], times=nrow)
        }
    }
    out <- as.data.frame(out)
    return(out)
}

bt_long.bt_perc <- function(x, meta=list(), ...){
    out <- reshape2::melt(x)
    names(out) <- c("bt_quad", "bt_var", "bt_val")
    return(out)
}

bt_long.bt_count <- function(x, meta=list(), ...){
    out <- reshape2::melt(x)
    names(out) <- c("bt_quad", "bt_var", "bt_val")
    if(length(meta) > 0){
        out <- cbind(out, bt_long(meta, ncol=nrow(out)))
    }
    out <- as.data.frame(out)
    return(out)
}

bt_long.bt_sf <- function(x, meta=list(), ...){
    out <- reshape2::melt(x, na.rm = TRUE)
    out <- out[,-2]
    names(out) <- c("bt_quad", "bt_val")
    return(out)
}

# Plot
plot.bt_sf <- function(x, main = NULL, xlab = "Size", ylab = "Frequency", las = 1, ...){
    dat <- x[!is.na(x)]
    old_par_mar <- par()$mar
    par(mar=c(5,4,1,1))
    hist(dat, main = NULL, xlab="Size", ylab="Frequency", las = 1, ...)
    par(mar=old_par_mar)
}

plot.bt_perc <- function(x, ...){
    xval <- rep(1:ncol(x), each=nrow(x))
    yval <- as.vector(x)
    labs <- colnames(x)
    xmar <- max(stringr::str_length(labs))/2
    old_par_mar <- par()$mar
    par(mar=c(xmar,4,1,1))
    plot(xval, yval, ann=F, axes=F, pch=19, col=rgb(0,0,0,0.2), ...)
    box()
    axis(1, lwd=0, lwd.ticks=1, labels = labs, at=1:ncol(x), las=2)
    axis(2, lwd=0, lwd.ticks=1, las=1)
    mtext(text = "% Cover", side = 2, line=2.5)
    par(mar=old_par_mar)
}

plot.bt_count <- function(x, ...){
    xval <- rep(1:ncol(x), each=nrow(x))
    yval <- as.vector(x)
    labs <- colnames(x)
    xmar <- max(stringr::str_length(labs))/2
    old_par_mar <- par()$mar
    par(mar=c(xmar,4,1,1))
    plot(xval, yval, ann=F, axes=F, pch=19, col=rgb(0,0,0,0.2), ...)
    box()
    axis(1, lwd=0, lwd.ticks=1, labels = labs, at=1:ncol(x), las=2)
    axis(2, lwd=0, lwd.ticks=1, las=1)
    mtext(text = "Number", side = 2, line=2.5)
    par(mar=old_par_mar)
}