str.takRdef <- function(object, ...){
    cat(sprintf("takiwaR definition object with %s item%s\n", 
                length(object),
                ifelse(length(object) != 1), "", "s"))
    invisible(NextMethod("str", give.length=FALSE,...))
}

str.takRblock <- function(object, ...){
    cat(sprintf(" \"%s\"\n", object$text))
}


str.takRpredicates <- function(object, ...){
    cat(sprintf("List of %s predicates\n", 
                length(object)))
    invisible(NextMethod("str", give.length=FALSE,  ...))
}

str.takRpredicate <- function(object, ...){
    cat(sprintf(" %s == %s\n", object$context, 
                ifelse(length(object$logi) == 0, "UNKNOWN", object$logi)))
}

str.takRvalidators <- function(object, ...){
    cat(sprintf("List of %s validators\n", 
                length(object)))
    invisible(NextMethod("str", give.length=FALSE,  ...))
}

str.takRvalidator <- function(object, ...){
    cat(sprintf(" %s\n", object$context))
}