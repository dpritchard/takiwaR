# For the future, we could provide a 'cause' or 'msg' for failures
# as_false <- function(cause = ""){
#     out <- FALSE
#     attr(out, "cause") <- cause
#       or
#     attr(out, "msg") <- cause
#     return(out)
# }

has_names <- function(x){
    out <- !is.null(names(x))
    return(out)
}
lacks_names <- function(x){
    out <- !has_names(x)
    return(out)
}
is_empty_names <- function(x){
    if(lacks_names(x)){
        return(FALSE)
    }
    out <- is_empty_string(names(x))
    return(out)
}
is_not_empty_names <- function(x){
    out <- !is_empty_names(x)
    return(out)
}
is_na_names <- function(x){
    if(lacks_names(x)){return(FALSE)}
    out <- is_na(names(x))
    return(out)
}
is_not_na_names <- function(x){
    out <- !is_na_names(x)
    return(out)
}
is_empty_string <- function(x){
    out <- x == ""
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