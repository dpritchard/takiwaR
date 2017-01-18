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
    keys <- names(x)
    pad_len <- max(stringr::str_length(keys))+3
    for(key in keys){
        cat(stringr::str_pad(paste0(key, " : "), pad_len, side="left"), 
            paste0(x[[key]], collapse = ", "), "\n", sep="")
    }
}

print.takRperc <- function(x, ...){
    NextMethod("print", na.print = "NA", ...)
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

# takRlong
# Long generates a long-form dataframe, columns: Quadrat, Variable, Value
takRlong <- function(x, ...) UseMethod("takRlong")

takRlong.takRperc <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    names(out) <- c("takR_quad", "takR_var", "takR_val")
    out <- map_attributes(x, out)
    return(out)
}

takRlong.takRcount <- function(x, ...){
    out <- reshape2::melt(x, as.is = T)
    names(out) <- c("takR_quad", "takR_var", "takR_val")
    out <- map_attributes(x, out)
    return(out)
}

takRlong.takRsf <- function(x, ...){
    out <- reshape2::melt(x, na.rm = TRUE, as.is = T)
    out <- out[,-2]
    names(out) <- c("takR_quad", "takR_val")
    out <- map_attributes(x, out)
    return(out)
}