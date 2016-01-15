## Methods
# takRvalidate
takRvalidate <- function(x, ...) {
    if(inherits(x, "takRempty")){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        invisible(x)
    } else {
        UseMethod("takRvalidate")
    }
}

takRvalidate.takRmeta <- function(x, req_vals = NULL, ...){
    # Fall back to old behaviour
    if(all(is.null(req_vals))){
        req_vals <- c("site", "date", "depth", "n_quad", "quad_size", "gps_lat", "gps_long")
    }
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
    invisible(x)
}

takRvalidate.takRperc <- function(x, ...){
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
    invisible(x)
}

takRvalidate.takRcount <- function(x, ...){
    if(any(is.na(x))){
        # Has NA's
        warning("This section has NAs. This shouldn't be possible: Implicit zeros are assumed.", immediate. = TRUE, call. = FALSE)
    }
    ## Tests for count data
    ## Currently we have none?
    # Should test for integers?
    invisible(x)
}

takRvalidate.takRsf <- function(x, ...){
    units <- ifelse(is.null(attr(x, "units")), "", paste0(" ", attr(x, "units")))
    if(!is.null(attr(x, "takRsec_range"))){
        minsize <- min(attr(x, "takRsec_range"))
        maxsize <- max(attr(x, "takRsec_range"))
        toosmall <- which(x < minsize, arr.ind=TRUE)
        if(nrow(toosmall) > 0){
            warning("At least one creature is less than ", minsize, units, ". Are you sure?", immediate. = TRUE, call. = FALSE)
        }
        toobig <- which(x > maxsize, arr.ind=TRUE)
        if(nrow(toobig) > 0){
            warning("At least one creature is greater than ", maxsize, units, ". Are you sure?", immediate. = TRUE, call. = FALSE)
        }
    }
    invisible(x)
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
        out[[key]] <- do.call(takRcombine, key_dat) # TODO - Tidy this up? Use UseMethod?
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
        out_meta <- set_takRmeta(out_meta, list(
            "quad_size" = sum(out_meta[['quad_size']]))
        )
    }
    # Assuming all special cases return only 1 value, return just unique values
    out_meta <- lapply(out_meta, unique) # TODO make this nicer!
    out_meta <- set_takRmeta(out_meta, list("is_merged" = TRUE))
    class(out_meta) <- c("takRmeta", class(out_meta))
    return(out_meta)
}

takRcombine.takRperc <- function(x1, x2, ...){
    all_meta <- extract_takRmeta(x1, x2, ...) # First grab all the metadata
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Needed for averaging, below
    # Find out which objects are null
    null_obj <- unlist(lapply(all_inp, is.null))
    # Find out which objects are empty, if any.
    empty_obj <- unlist(lapply(all_inp, inherits, what='takRempty'))
    # Join objects
    null_empty <- as.logical(null_obj+empty_obj)
    if(all(null_empty)){
        all_dat <- NA
        class(all_dat) <- c('takRperc', 'takRempty', class(all_dat))
        return(all_dat)
    }
    # Remove null or empty sections
    all_inp <- all_inp[!null_empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    # Melt and then reshape the input data by summing the double-ups...
    all_dat <- sum_on_col(all_inp)
    
    # Convert to a mean based on original input length
    all_dat <- all_dat/original_length 
    
    all_dat <- toattr_takRmeta(all_dat, all_meta) # Add back the combined metadata
    class(all_dat) <- c("takRperc", "takRwide", class(all_dat))
    return(all_dat)
}

takRcombine.takRcount <- function(x1, x2, ...){
    all_meta <- extract_takRmeta(x1, x2, ...) # First grab all the metadata
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Not really needed...
    # Find out which objects are null
    null_obj <- unlist(lapply(all_inp, is.null))
    # Find out which objects are empty, if any.
    empty_obj <- unlist(lapply(all_inp, inherits, what='takRempty'))
    # Join objects
    null_empty <- as.logical(null_obj+empty_obj)
    if(all(null_empty)){
        all_dat <- NA
        class(all_dat) <- c('takRcount', 'takRempty', class(all_dat))
        return(all_dat)
    }
    # Remove null or empty sections
    all_inp <- all_inp[!null_empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    # Melt and then reshape the input data by summing the double-ups...
    all_dat <- sum_on_col(all_inp)
    
    all_dat <- toattr_takRmeta(all_dat, all_meta) # Add back the combined metadata
    class(all_dat) <- c("takRcount", "takRwide", class(all_dat))
    return(all_dat)
}

takRcombine.takRsf <- function(x1, x2, ...){
    all_meta <- extract_takRmeta(x1, x2, ...) # First grab all the metadata
    all_inp <- c(list(x1), list(x2), list(...))
    original_length <- length(all_inp) # Not really needed
    # Find out which objects are null
    null_obj <- unlist(lapply(all_inp, is.null))
    # Find out which objects are empty, if any.
    empty_obj <- unlist(lapply(all_inp, inherits, what='takRempty'))
    # Join objects
    null_empty <- as.logical(null_obj+empty_obj)
    if(all(null_empty)){
        all_dat <- NA
        class(all_dat) <- c('takRsf', 'takRempty', class(all_dat))
        return(all_dat)
    }
    # Remove null or empty sections
    all_inp <- all_inp[!null_empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    all_dat <- do.call(cbind, all_inp)
    all_dat <- toattr_takRmeta(all_dat, all_meta) # Add back the combined metadata
    class(all_dat) <- c("takRsf", "takRwide", class(all_dat))
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
    class(out) <- c("summary.takRbt", class(out))
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
    out <- cbind("mean"=dat_mean, "median"=dat_median, 
                 "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    class(out) <- c("summary.takRperc", class(out))
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
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, 
                 "sd"=dat_sd, "se"=dat_se)
    class(out) <- c("summary.takRcount", class(out))
    return(out)
}

summary.takRsf <- function(object, ...){
    out <- list()
    n_quad <- attr(x = object, which = "takRmeta_n_quad", exact = TRUE)
    quad_size <- attr(x = object, which = "takRmeta_quad_size", exact = TRUE)
    takRsec_units <- attr(x = object, which = "takRsec_units", exact = TRUE)
    
    if(is.null(n_quad)){warning("n_quad not supplied as an attibute.", 
                                call. = FALSE, immediate. = TRUE)}
    if(is.null(quad_size)){warning("quad_size not supplied as an attibute.", 
                                   call. = FALSE, immediate. = TRUE)}
    
    if(inherits(object, what = "takRempty")){
        if(is.null(n_quad) || is.na(n_quad)){
            out[["density"]] <- NA
            out[["density_summary"]] <- NA
        } else {
            out[["density"]] <- rep(0, times = n_quad)
            out[["density_summary"]] <- cbind("mean" = 0, "median" = 0, 
                                          "n" = n_quad, "sd" = 0, "se" = 0)
        }
        out[["data_summary"]] <- NA
        class(out) <- c("takRempty", class(out))
        return(out)
    }
    
    # If not empty, we need to do some math!
    not_na <- object[!is.na(object)]
    
    # Density by quadrat (i.e. row) 
    den_quad <- apply(object, 1, function(x){sum(!is.na(x))})
    den_quad <- den_quad*(1/quad_size)
    out[["density"]] <- den_quad
    
    # Density summary
    #den_all <- length(not_na)/n_quad 
    #den_all <- den_all*(1/quad_size) # Overall, no error. Same as mean, below.
    den_mean <- mean(den_quad)
    den_median <- median(den_quad)
    den_n <- length(den_quad)
    den_sd <- sd(den_quad)
    den_se <- den_sd/sqrt(den_n)
    out[["density_summary"]] <- cbind("mean"=den_mean, "median"=den_median, 
                                  "n"=den_n, "sd"=den_sd, "se"=den_se)
    rownames(out[["density_summary"]]) <- "All"
    
    # Data (usually "size") summary
    dat_mean <- mean(not_na)
    dat_median <- median(not_na)
    dat_n <- length(not_na)
    dat_sd <- sd(not_na)
    dat_se <- dat_sd/sqrt(dat_n)
    out[["data_summary"]] <- cbind("mean"=dat_mean, "median"=dat_median, 
                               "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    rownames(out[["data_summary"]]) <- "All"
    attr(out[["data_summary"]], "takRsec_units") <- takRsec_units
    
    class(out) <- c("summary.takRsf", class(out))
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
            if(!is.null(x[[a]]$required)){
                cat("\n", stringr::str_pad("", width = padwidth1+1), "required: ", 
                    paste0(x[[a]]$required, collapse = ", "), sep="")
            }
            if(!is.null(x[[a]]$takRsec_range)){
                cat("\n", stringr::str_pad("", width = padwidth1+1), "   range: ", x[[a]]$takRsec_range[1], 
                    " to ", x[[a]]$takRsec_range[2], sep="")
            } 
            if(!is.null(x[[a]]$takRsec_units)){
                cat(" ", x[[a]]$takRsec_units, sep="")
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

print.summary.takRbt <- function(x, ...){
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
    NextMethod("print", na.print = "NA", ...)
}

print.summary.takRperc <- function(x, digits = getOption("digits")-3, ...){
    print.table(x, digits = digits)
}

print.takRcount <- function(x, ...){
    NextMethod("print", na.print = "NA", ...)
}

print.summary.takRcount <- function(x, digits = getOption("digits")-3, ...){
    print.table(x, digits = digits)
}

print.takRsf <- function(x, ...){
    NextMethod("print", na.print = "", ...)
}

print.summary.takRsf <- function(x, digits = getOption("digits")-3, ...){
    if(all(is.na(x[["density"]]))){
        cat("< Density Information Unavailable >\n")
    } else {
        cat("Density (per m^2)\n")
        print.table(x[["density_summary"]], digits = digits)
        cat("\n")
    }
    cat(paste0("Data (", attr(x[["data_summary"]], "takRsec_units", exact = TRUE), ")\n"))
    print.table(x[["data_summary"]], digits = digits)
}

print.takRwide <- function(x, ...){
    print.table(x, ...)
}

# takRlong
# Long generates a "long form" dataframe, columns: Quadrat, Variable, Value
takRlong <- function(x, ...) UseMethod("takRlong")

# Not sure there is much need to do this....
# takRlong.takRmeta <- function(x, nrow=1, ...){
#     keys <- names(x)
#     out  <- list()
#     # Sort out duplicate keys
#     for(key in keys){
#         vals <- x[[key]]
#         if(length(vals) > 1){ key <- make_key(key) }
#         for(a in 1:length(vals)){
#             out[[key[a]]] <- rep(vals[a], times=nrow)
#         }
#     }
#     out <- as.data.frame(out)
#     return(out)
# }

takRlong.takRperc <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    #out <- as.matrix(out)
    #dimnames(out) <- list(NULL, c("takR_quad", "takR_var", "takR_val"))
    names(out) <- c("takR_quad", "takR_var", "takR_val")
    out <- map_attributes(x, out)
    return(out)
}

takRlong.takRcount <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    #out <- as.matrix(out)
    #dimnames(out) <- list(NULL, c("takR_quad", "takR_var", "takR_val"))
    names(out) <- c("takR_quad", "takR_var", "takR_val")
    out <- map_attributes(x, out)
    return(out)
}

takRlong.takRsf <- function(x, ...){
    out <- reshape2::melt(x, na.rm = TRUE, as.is = T)
    out <- out[,-2]
    #out <- as.matrix(out)
    #dimnames(out) <- list(NULL, c("takR_quad", "takR_val"))
    names(out) <- c("takR_quad", "takR_val")
    out <- map_attributes(x, out)
    return(out)
}

# Plot
plot.takRsf <- function(x, main = NULL, xlab = "Size", ylab = "Frequency", las = 1, ...){
    dat <- x[!is.na(x)]
    old_par_mar <- par()$mar
    par(mar=c(5,4,1,1))
    hist(dat, main = NULL, xlab="Size", ylab="Frequency", las = las, ...)
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