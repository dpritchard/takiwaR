## Methods
# takRvalidate
takRvalidate <- function(x, ...) UseMethod("takRvalidate")

takRvalidate.takRmeta <- function(x, ...){
    # Deal with required values
    req_vals <- c("site", "date", "depth", "n_quad", "quad_size", "gps_lat", "gps_long")
    val_locs <- which(req_vals %in% names(x))
    if(length(val_locs) == 0){
        missing <- req_vals
    } else {
        missing <- req_vals[-val_locs]
    }
    if(length(missing) > 0){
        stop("The following required values are missing from the metadata: ", paste(missing, collapse=", "), call. = FALSE)
    }
    #Deal with reccomended values
    if(!'collected_by' %in% names(x)){
        warning("The 'collected_by' field is missing from the metadata. It is recommended.", immediate. = TRUE, call. = FALSE)
    }
    if(!'entered_by' %in% names(x)){
        warning("The 'entered_by' field is missing from the metadata. It is recommended.", immediate. = TRUE, call. = FALSE)
    }
    # Date parsing? etc.
    #class(x) <- c("takRmeta", class(x))
    return(x)
}

takRvalidate.takRperc <- function(x, ...){
    if(all(is.na(x))){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        class(x) <- c("takRempty", class(x))
        return(x)
    }
    if(any(is.na(x))){
        # Has NA's
        warning("This section has NAs. This shouldn't be possible: Implicit zeros are assumed.", immediate. = TRUE, call. = FALSE)
    }
    ## Tests for percent cover data
    toohigh <- which(x > 100, arr.ind=T)
    # Test that no (non NA) value is > 100
    if(nrow(toohigh) > 0){
        warning("At least one value has been entered with > 100% cover. Are you sure?", immediate. = TRUE, call. = FALSE)
    }
    #class(x) <- c("takRperc", class(x))
    return(x)
}

takRvalidate.takRcount <- function(x, ...){
    if(all(is.na(x))){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        class(x) <- c("takRempty", class(x))
        return(x)
    }
    if(any(is.na(x))){
        # Has NA's
        warning("This section has NAs. This shouldn't be possible: Implicit zeros are assumed.", immediate. = TRUE, call. = FALSE)
    }
    ## Tests for count data
    ## Currently we have none?
    # Should test for integers?
    #class(x) <- c("takRcount", class(x))
    return(x)
}

takRvalidate.takRsf <- function(x, ...){
    if(all(is.na(x))){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        class(x) <- c("takRempty", class(x))
        return(x)
    }
    units <- ifelse(is.null(attr(x, "units")), "", paste0(" ", attr(x, "units")))
    if(!is.null(attr(x, "range"))){
        minsize <- min(attr(x, "range"))
        maxsize <- max(attr(x, "range"))
        toosmall <- which(x < minsize, arr.ind=TRUE)
        if(nrow(toosmall) > 0){
            warning("At least one creature is less than ", minsize, units, ". Are you sure?", immediate. = TRUE, call. = FALSE)
        }
        toobig <- which(x > maxsize, arr.ind=TRUE)
        if(nrow(toobig) > 0){
            warning("At least one creature is greater than ", maxsize, units, ". Are you sure?", immediate. = TRUE, call. = FALSE)
        }
    }
    #class(x) <- c("takRsf", class(x))
    return(x)
}

# takRcombine... Combine is for combining data from subquadrats into larger quadrats
# e.g. Low vis means divers are working small sections of a larger quadrat.
# The assumption is that there are equal numbers of (sub)quadrats in each dataset
takRcombine <- function(x1, x2, ...) UseMethod("takRcombine")

takRcombine.takRbt <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    keys <- unique(unlist(lapply(all_inp, names)))
    # Merge each of the objects. Relies on Method dispatching
    out <- list()
    for(key in keys){
        key_dat <- lapply(all_inp, 'getElement', key)
        #print(str(key_dat))
        out[[key]] <- do.call(takRcombine, key_dat)
    }
    class(out) <- c("takRbt", class(out))
    return(out)
}

takRcombine.takRmeta <- function(x1, x2, ...){
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
    class(out_meta) <- c("takRmeta", class(out_meta))
    return(out_meta)
}

takRcombine.takRperc <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Needed for averaging, below
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='takRempty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('takRempty', 'takRperc', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
#     for(a in 1:length(all_inp)){
#         if(inherits(all_inp[a], what = 'takRempty')){
#             all_inp[a] <- NULL
#         }
#     }
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!takR_compare(rowcount)){
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
    class(all_dat) <- c("takRperc", class(all_dat))
    return(all_dat)
}

takRcombine.takRcount <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Not really needed...
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='takRempty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('takRempty', 'takRcount', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!takR_compare(rowcount)){
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
    class(all_dat) <- c("takRcount", class(all_dat))
    return(all_dat)
}

takRcombine.takRsf <- function(x1, x2, ...){
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Not really needed
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='takRempty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('takRempty', 'takRsf', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!takR_compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    all_dat <- do.call(cbind, all_inp)
    class(all_dat) <- c("takRsf", class(all_dat))
    return(all_dat)
}

# Summary
summary.takRbt <- function(object, ...){
    out <- list('meta' = object[['meta']])
    keys <- names(object)
    keys <- keys[!keys=='meta']
    for(key in keys){
        out[[key]] <- summary(object[[key]])
    }
    class(out) <- c("takRsummary", class(out))
    return(out)
}

summary.takRperc <- function(object, ...){
    if(inherits(object, what = "takRempty")){
        out <- NA
        class(out) <- c("takRempty", class(out))
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

summary.takRcount <- function(object, ...){
    if(inherits(object, what = "takRempty")){
        out <- NA
        class(out) <- c("takRempty", class(out))
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

summary.takRsf <- function(object, ...){
    if(inherits(object, what = "takRempty")){
        out <- NA
        class(out) <- c("takRempty", class(out))
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
print.takRsec <- function(x, ...){
    if(length(x)==0){
        cat("< Empty Section List >\n")
    } else {
        headers <- sapply(x, "[[", "text")
        padwidth1 <- max(stringr::str_length(headers))
        for(a in 1:length(x)){
            cat(stringr::str_pad(headers[a], width = padwidth1), " ", sep="")
            cat("    name: ", names(x[a]) , "\n", sep="")
            cat(stringr::str_pad("", width = padwidth1+1), "   class: ", x[[a]]$class, sep="")
            if(!is.null(x[[a]]$quad_dir)){
                dirtext <- ifelse(x[[a]]$quad_dir=="col", "in columns", "in rows")
                cat("\n", stringr::str_pad("", width = padwidth1+1), "quadrats: ", dirtext, sep="")
            }
            if(!is.null(x[[a]]$range)){
                cat("\n", stringr::str_pad("", width = padwidth1+1), "   range: ", x[[a]]$range[1], " to ", x[[a]]$range[2], sep="")
            } 
            if(!is.null(x[[a]]$units)){
                cat(" ", x[[a]]$units, sep="")
            }
            cat("\n")
        }
    }
}

print.takRempty <- function(x, ...){
    cat("< No Data >\n")
}

print.takRmeta <- function(x, ...){
    keys <- names(x)
    pad_len <- max(stringr::str_length(keys))+3
    for(key in keys){
        cat(stringr::str_pad(paste0(key, " : "), pad_len, side="left"), 
            paste0(x[[key]], collapse = ", "), "\n", sep="")
    }
}

print.takRsummary <- function(x, ...){
    keys <- names(x)
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
        print(x[[key]], ...)
    }
}

print.takRbt <- function(x, ...){
    keys <- names(x)
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
        print(x[[key]], ...)
    }
}

print.takRperc <- function(x, ...){
    print.table(x, na.print = "NA", ...)
}

print.takRcount <- function(x, ...){
    print.table(x, na.print = "NA", ...)
}

print.takRsf <- function(x, ...){
    print.table(x, na.print = "", ...)
}

# takRlong
# Long generates a "long form" dataframe, columns: Quadrat, Variable, Value and one each for meta data
takRlong <- function(x, ...) UseMethod("takRlong")

takRlong.takRmeta <- function(x, nrow=1, ...){
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

takRlong.takRperc <- function(x, meta=list(), ...){
    out <- reshape2::melt(x)
    names(out) <- c("bt_quad", "bt_var", "bt_val")
    return(out)
}

takRlong.takRcount <- function(x, meta=list(), ...){
    out <- reshape2::melt(x)
    names(out) <- c("bt_quad", "bt_var", "bt_val")
    if(length(meta) > 0){
        out <- cbind(out, takRlong(meta, ncol=nrow(out)))
    }
    out <- as.data.frame(out)
    return(out)
}

takRlong.takRsf <- function(x, meta=list(), ...){
    out <- reshape2::melt(x, na.rm = TRUE)
    out <- out[,-2]
    names(out) <- c("bt_quad", "bt_val")
    return(out)
}

# Plot
plot.takRsf <- function(x, main = NULL, xlab = "Size", ylab = "Frequency", las = 1, ...){
    dat <- x[!is.na(x)]
    old_par_mar <- par()$mar
    par(mar=c(5,4,1,1))
    hist(dat, main = NULL, xlab="Size", ylab="Frequency", las = 1, ...)
    par(mar=old_par_mar)
}

plot.takRperc <- function(x, ...){
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

plot.takRcount <- function(x, ...){
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