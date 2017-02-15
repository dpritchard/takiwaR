# Print Methods
print.takRcollection <- function(x, ...){
    keys <- names(x)
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
            print(x[[key]], ...)
    }
}

print.takRdef <- function(x, ...){
    if(length(x)==0){
        cat("< Empty Definition List >\n")
    } else {
        headers <- names(x)
        f_obj <- "text" # First object to print
        f_obj_t <- "text:" # Text label for the first object
        f_obj_len <- stringr::str_length(f_obj_t)
        xtra <- 6 # xtra == extra space
        pw <- max(stringr::str_length(headers))+xtra # pw == padding width
        
        for(a in 1:length(x)){
            cat(stringr::str_pad(headers[a], width = pw, side = "right"), sep="")
            cat("text:", x[[a]]$text , "\n")
            cat(stringr::str_pad("str_in:", width = pw+f_obj_len), x[[a]]$str_in, "\n")
            cat(stringr::str_pad("str_out:", width = pw+f_obj_len), x[[a]]$str_out)
            if(!is.null(x[[a]]$fill)){
                cat("\n")
                cat(stringr::str_pad("fill:", width = pw+f_obj_len), x[[a]]$fill)
            }
            if(!is.null(x[[a]]$n)){
                cat("\n")
                cat(stringr::str_pad("n:", width = pw+f_obj_len), x[[a]]$n)
            }
            cat("\n")
        }
    }
}

print.takRempty <- function(x, ...){
    cat("< Empty Data Block >\n")
}

print.takRmeta <- function(x, ...){
    NextMethod("print", na.print = "NA", ...)
}

print.takRrow <- function(x, ...){
    keys <- names(x)
    pad_len <- max(stringr::str_length(keys))+3
    for(key in keys){
        cat(stringr::str_pad(paste0(key, " : "), pad_len, side="left"), 
            paste0(x[[key]], collapse = ", "), "\n", sep="")
    }
}

print.takRperc <- function(x, ...){
    
}

print.takRcount <- function(x, ...){
    NextMethod("print", na.print = "NA", ...)
}

print.takRsf <- function(x, ...){
    NextMethod("print", na.print = "", ...)
}

print.takRwide <- function(x, ...){
    print.table(x, ...)
}


print.takRpredicate <- function(x, ..., context_length = NULL, pad = 4){
    # context_length is the width of the string length of the context.
    # It might be supplied, if there are potentially multiple contexts
    # In which case it will be the maximum length of any context...
    # Otherwise, we calculate it here.  
    if(is.null(context_length)){
        context_length <- stringr::str_length(x[["context"]])
    }
    result <- logi_to_predicate(x[["logi"]])
    
    # The first line is made up of [context+padding][pad]["result"][": "][result]
    # We need to calculate the length of the [context+padding][pad]["result"] part
    # pad is the amount of padding between the context and the label of the first object.
    part1 <- paste0(stringr::str_pad(x[["context"]], context_length+pad, side = "right"), "result")
    part1_len <- stringr::str_length(part1)
    
    cat(part1, ": ", result, "\n", sep="")
    
    # TODO: Treat myself to nice "getting and setting functions" for errors, warnings and messages (objects of takRpredicate, generally)
    lab <- ifelse(length(x[["err"]]) > 1, "errors", "error")
    print_ewm(x[["err"]], label = lab, pad = part1_len)
    
    lab <- ifelse(length(x[["wrn"]]) > 1, "warnings", "warning")
    print_ewm(x[["wrn"]], label = lab, pad = part1_len)
    
    lab <- ifelse(length(x[["msg"]]) > 1, "messages", "message")
    print_ewm(x[["msg"]], label = lab, pad = part1_len)
    
}

print.takRpredicates <- function(x, ..., only_fails = TRUE, header = NULL){
    if(only_fails){
        indx <- unlist(lapply(x, "[[", "logi"))
        x <- x[!indx]
    }
    if(length(x)){
        if(!is.null(header)){
            cat(header, "\n")
        }
        contexts <- unlist(lapply(x, "[[", "context"))
        max_context_len <- max(stringr::str_length(contexts))
        for(a in seq_along(x)){
            print(x[[a]], context_length = max_context_len)
        }
    }
}

# takRlong
# Long generates a long-form dataframe, columns: Quadrat, Variable, Value
takRlong <- function(x, ...) UseMethod("takRlong")

takRlong.takRperc <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    names(out) <- c("row_name", "col_name", "value")
    return(out)
}

takRlong.takRcount <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    names(out) <- c("row_name", "col_name", "value")
    return(out)
}

takRlong.takRsf <- function(x, ...){
    out <- reshape2::melt(x, na.rm = TRUE, as.is = T)
    out <- out[,-2]
    names(out) <- c("row_name", "value")
    return(out)
}

takRlong.takRwide <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    names(out) <- c("row_name", "col_name", "value")
    return(out)
}

takRlong.takRcol <- function(x, ...){
    return(x)
}

takRlong.takRrow <- function(x, ...){
    stop("The 'takRlong' method is not supported for objects of class 'takRrow'")
}

extract_long <- function(x, what, what_meta=NULL){
    # Extract a named item as a long-form data frame.
    # Optionally include meta data in columns.
    if(!is.character(what)){
        stop("`what` must be a string")
    }
    if(is.null(x[[what]])){
        msg <- sprintf("'%s' does not exist, returning NULL", what)
        warning(msg)
        return(NULL)
    }
    if(all(is.na(x[[what]]))){
        msg <- sprintf("'%s' contains only NAs, returning NULL", what)
        warning(msg)
        return(NULL)
    }
    tmp <- takRlong(x[[what]])
    if(!is.null(what_meta)){
        indx <- stringr::str_detect(lapply(x, class), "takRmeta")
        if(!any(indx)){
            stop("Metadata requested, but `x` does not have an object of class 'takRmeta'")
        }
        meta <- x[[which(indx)[1]]]
        for(a in 1:length(what_meta)){
            metadat <- meta[[what_meta[a]]]
            if(length(metadat) > 1){
                metanames <- c(paste0(what_meta[a], "_", 1:length(metadat)))
            } else {
                metanames <- what_meta[a]
            }
            for(b in 1:length(metanames)){
                tmp[metanames[b]] <- metadat[b]
            }
        }
    }
    return(tmp)
}