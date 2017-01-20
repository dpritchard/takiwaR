str.takRdef <- function(object, ...){
    plural <- ifelse((length(object) != 1), "s", "")
    cat(sprintf("takiwaR definition object with %s item%s\n", length(object), plural))
    invisible(NextMethod("str", no.list = TRUE, ...))
}

str.takRblock <- function(object, ...){
    cat(sprintf(" \"%s\"\n", object$text))
    invisible(NextMethod("str", no.list = TRUE, ...))
}

str.takRpredicates <- function(object, ...){
    plural <- ifelse((length(object) != 1), "s", "")
    cat(sprintf(" List of %s predicate%s\n", length(object), plural))
    #invisible(NextMethod("str", no.list = TRUE, ...))
}

str.takRpredicate <- function(object, ...){
    outcome <- ifelse(length(object$logi) == 0, "UNKNOWN", object$logi)
    cat(sprintf(" %s == %s\n", object$context, outcome))
}

str.takRvalidators <- function(object, ...){
    plural <- ifelse((length(object) != 1), "s", "")
    cat(sprintf(" List of %s validator%s\n", length(object), plural))
    #invisible(NextMethod("str", no.list = TRUE, ...))
}

str.takRvalidator <- function(object, ...){
    cat(sprintf(" %s\n", object$context))
}