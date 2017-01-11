# Validatiors
## validate - verb (to validate)
## validator - noun (something that validates)
## validators - plural noun (things that validate)
## 
## predicate - noun (the outcome of a logical argument, a declaration)
## predicates - plural noun (multiple outcomes or declarations)

# Remember:
## collections: Must contain metadata, and may contain data or another collection
## meta: Metadata objects of class takRmeta
## data: Data objects of a takR* class

# In the context of takiwaR:
## A validator is an overloaded predicate function that produces a single predicate.

# Exported functions
make_validator <- function(f, ..., what = c("all", "any"), context = NULL, treat_na_as = FALSE){
    force(f)
    # Try to guess the context (this will not work sometimes)
    # TODO: Can we improve this?
    if(is.null(context)){
        context <- deparse(substitute(f))
    }
    any_or_all <- match.fun(match.arg(what))
    func <- function(x){
        err <- wrn <- msg <- NULL
        # Run the function, capturing errors, warnings and messages
        res <- withCallingHandlers(
            tryCatch(f(x, ...), error=function(e) {
                err <<- conditionMessage(e)
                FALSE
            }), warning=function(w) {
                wrn <<- append(wrn, stringr::str_trim(conditionMessage(w)))
                invokeRestart("muffleWarning")
            }, message = function(m) {
                msg <<- append(msg, stringr::str_trim(conditionMessage(m)))
                invokeRestart("muffleMessage")
            })
        
        # If there are NAs, then treat them based on the value of "treat_na_as"
        if(any(is.na(res))){
            res[is.na(res)] <- treat_na_as
            wrn <- append(paste0("Validator replaced NA(s) with ", treat_na_as), wrn)
        }
        
        # Check for logical
        if(!is.logical(res)){
            err <- append(paste0("Validator did not return a logical (returned: ", mode(res),")"), err)
            res <- FALSE
        }
        
        # Apply ALL or ANY
        res <- any_or_all(res)
        
        # Build out object
        out <- make_predicate(logi = res, context = context, err = err, wrn = wrn, msg = msg)
        return(out)
    }
    class(func) <- c("takRvalidator", class(func))
    return(func)
}

# Methods
validate <- function(x, ...) {
    UseMethod("validate")
}

validate.default <- function(x, ...) {
    attr(x, "takRpredicates") <- run_validators(x)
    return(x)
}

validate.takRcollection <- function(x, ...) {
    # Need to apply validation and then loop to validate objects...
}

validate.takRdata <- function(x, ...) {
    attr(x, "takRpredicates") <- run_validators(x)
    return(x)
}

validate.takRmeta <- function(x, ...) {
    attr(x, "takRpredicates") <- run_validators(x)
    return(x)
}

validate.takRdef <- function(x, ...){
    attr(x, "takRpredicates") <- run_validators(x)
    return(x)
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

print.takRpredicates <- function(x, ..., only_fails = TRUE){
    if(only_fails){
        indx <- unlist(lapply(x, "[[", "logi"))
        x <- x[!indx]
    }
    if(length(x)){
        contexts <- unlist(lapply(x, "[[", "context"))
        
        max_context_len <- max(stringr::str_length(contexts))
        for(a in seq_along(x)){
            print(x[[a]], context_length = max_context_len)
        }
    }
}

# Not exported below this point.

## make_predicate() produces a empty (defualt) predicate
make_predicate <- function(logi = logical(0), context = "", 
                             err = NULL, wrn = NULL, msg = NULL){
    out <- list("logi" = logi, "context" = context, 
                "err" = err, "wrn" = wrn, "msg" = msg)
    class(out) <- c("takRpredicate", class(out))
    return(out)
}

## run_validators() runs the validator(s) and returns a (list of) predicate(s)
run_validators <- function(x) {
    out <- list()
    class(out) <- c(class(out), "takRpredicates")
    # Checking the 
    if(is.null(attr(x, "takRvalidators"))){
        out[[1]] <- make_predicate(context = "Verifying validators", 
                                   msg="No validators present. No action taken")
        return(out)
    }
    funcs <- validators(x)
    if(!inherits(funcs, "list")){
        out[[1]] <- make_predicate(logi = FALSE, context = "Verifying validators", 
                                   err = "The 'takRvalidators' attribute does not contain a list")
        return(out)
    }
    v <- all(unlist(lapply(funcs, inherits, what = "takRvalidator")))
    if(!v){
        out[[1]] <- make_predicate(logi = FALSE, context = "Verifying validators", 
                               err = "Some (or all) of the validators are not of class 'takRvalidator'",
                               msg = "Use make_validator()")
        return(out)
    }
    v <- all(unlist(lapply(funcs, inherits, what = "function")))
    if(!v){
        out <- make_predicate(logi = FALSE, context = "Verifying validators", 
                          err = "Some (or all) of the validators are not functions",
                          msg = "Use make_validator()")
        return(out)
    }
    
    # If we get this far then it all checks out, then apply the fucntions and return the outcome(s) 
    for(a in seq_along(funcs)){
        out[[a]] <- funcs[[a]](x)
    }
    return(out)
}

## validators() get or set validators
validators <- function(x){
    attr(x, "takRvalidators")
}

`validators<-` <- function(x, value){
    if(!is.list(value)){
        stop("You must supply a list of validators")
    }
    v <- all(unlist(lapply(value, inherits, what = "takRvalidator")))
    if(!v){
        stop("All objects must be of class `takRvalidator`")
    }
    attr(x, "takRvalidators") <- value
    return(x)
}

predicates <- function(x){
    attr(x, "takRpredicates")
}

`predicates<-` <- function(x, value){
    if(!is.list(value)){
        stop("You must supply a list of predciates")
    }
    v <- all(unlist(lapply(value, inherits, what = "takRpredicate")))
    if(!v){
        stop("All objects must be of class `takRpredicate`")
    }
    attr(x, "takRpredicates") <- value
    return(x)
}


## Maps a logical to a predicate string
logi_to_predicate <- function(logi){
    result <- NULL
    if(length(logi) == 0L){
        result <- "UNDEFINED" # This is possible, but not normal: logical(0)
    } else if(logi){
        result <- "PASS"
    } else {
        result <- "FAIL"
    }
    return(result)
}

# Prints errs, wrns and msgs
print_ewm <- function(x, label, pad){
    if(is.null(x)){return(invisible(NULL))} # If x is NULL, do nothing
    cat(stringr::str_pad(label, pad, side = "left"), ": ", sep="")
    for(a in seq_along(x)){
        if(a == 1L){
            cat(x[a], "\n", sep="")
        } else {
            full_pad <- pad+stringr::str_length(label)+3 # TODO: Why "3"? It works, but...
            cat(stringr::str_pad(x[a], full_pad, side = "left"), "\n", sep="")
        }
    }
}



