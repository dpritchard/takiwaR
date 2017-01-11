# For the future, we could provide a 'cause' or 'msg' for failures
# as_false <- function(cause = ""){
#     out <- FALSE
#     attr(out, "cause") <- cause
#       or
#     attr(out, "msg") <- cause
#     return(out)
# }

has_empty_names <- function(x){
    if(!assertive::has_names(x)){
        stop("Does not have names, so can not determine if they are empty")
    }
    out <- !nzchar(names(x))
    return(out)
}

has_empty_colnames <- function(x){
    if(!assertive::has_colnames(x)){
        stop("Does not have colnames, so can not determine if they are empty")
    }
    out <- !nzchar(colnames(x))
    return(out)
}

has_empty_rownames <- function(x){
    if(!assertive::has_rownames(x)){
        stop("Does not have rownnames, so can not determine if they are empty")
    }
    out <- !nzchar(rownames(x))
    return(out)
}

has_unique_names <- function(x){
    if(!assertive::has_names(x)){
        stop("Does not have names, so can not determine if they are unique")
    }
    out <- has_no_duplicates(names(x))
    return(out)
}

has_unique_colnames <- function(x){
    if(!assertive::has_colnames(x)){
        stop("Does not have colnames, so can not determine if they are unique")
    }
    out <- has_no_duplicates(colnames(x))
    return(out)
}

has_unique_rownames <- function(x){
    if(!assertive::has_rownames(x)){
        stop("Does not have rownnames, so can not determine if they are unique")
    }
    out <- has_no_duplicates(rownames(x))
    return(out)
}






is_na_names <- function(x){
    if(lacks_names(x)){return(FALSE)}
    out <- is_na(names(x))
    return(out)
}

is_not_empty_string <- function(x){
    out <- !is_empty_string(x)
    return(out)
}
is_na <- function(x){
    out <- is.na(x)
    return(out)
}
is_not_na <- function(x){
    out <- !is_na(x)
    return(out)
}
is_zero <- function(x){
    out <- abs(x) <= .Machine$double.eps
    return(out)
}

all_is_na <- function(x){
    out <- all(is_na(x))
    return(out)
}