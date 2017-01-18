# For the future, we could provide a 'cause' or 'msg' for failures
# as_false <- function(cause = ""){
#     out <- FALSE
#     attr(out, "cause") <- cause
#       or
#     attr(out, "msg") <- cause
#     return(out)
# }

negate <- function(func){
    function(...) !func(...) 
}

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
    out <- assertive::has_no_duplicates(names(x))
    return(out)
}
has_unique_colnames <- function(x){
    if(!assertive::has_colnames(x)){
        stop("Does not have colnames, so can not determine if they are unique")
    }
    out <- assertive::has_no_duplicates(colnames(x))
    return(out)
}
has_unique_rownames <- function(x){
    if(!assertive::has_rownames(x)){
        stop("Does not have rownnames, so can not determine if they are unique")
    }
    out <- assertive::has_no_duplicates(rownames(x))
    return(out)
}

has_specified_named_object <- function(x, what){
    if(!assertive::has_names){
        stop("Does not have names, so cannot search them.")
    }
    out <- what %in% names(x)
    return(out)
}

# Specialist fucntions for takRdef
is_readdef <- function(x){
    if(!inherits(x, what = "takRdef")){
        message("Does not inherit from `takRdef`")
        return(FALSE)
    }
    are_takRblock <- unlist(lapply(x, inherits, "takRblock"))
    if(!all(are_takRblock)){
        message("Does not contain exclusively objects of takRblock.")
        message("Object is not a 'read' definition object.")
        return(FALSE)
    }
    return(TRUE)
}
readdef_has_unique_text <- function(x){
    if(!is_readdef(x)){
        message("Test is not relevant.")
        out <- logical(0)
    } else {
        text_vals <- unlist(lapply(x, "[[", "text"))
        out <- assertive::has_no_duplicates(text_vals)
    }
    return(out)
}
readdef_has_meta <- function(x){
    if(!is_readdef(x)){
        message("Test is not relevant.")
        out <- logical(0)
    } else {
        out <- "meta" %in% names(x)
    }
    return(out)
}
has_takRvalidators <- function(x){
    !is.null(attr(x, "takRvalidators"))
}

# Do not exist in assertive
is_zero <- function(x){
    out <- abs(x) <= .Machine$double.eps
    return(out)
}

has_na_names <- function(x){
    if(!assertive::has_names(x)){return(FALSE)}
    out <- assertive::is_na(names(x))
    return(out)
}

all_is_na <- function(x){
    out <- all(assertive::is_na(x))
    return(out)
}