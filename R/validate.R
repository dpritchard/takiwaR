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
make_validator <- function(f, ..., any_or_all = c("all", "any"),
                           context = NULL, description = NULL, treat_na_as = FALSE){
    if(!inherits(f, what = "function")){
        stop("`f` is not a function")
    }
    force(f)
    # Try to guess the context (this will not work correctly sometimes)
    # TODO: Can we improve this? Hvae a look at assertive::get_name_in_parent
    if(is.null(context)){
        context <- deparse(substitute(f))
    }
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
        out <- list("res" = res, "err" = err, "wrn" = wrn, "msg" = msg)
        return(out)
    }
    any_or_all <- match.arg(any_or_all)
    any_or_all_f <- match.fun(any_or_all)
    # TODO: Make use of "description"
    out <- list("v" = func, "any_or_all" = any_or_all, "any_or_all_f" = any_or_all_f, 
                "context" = context, "description" = description,
                "treat_na_as" = treat_na_as)
    out <- add_class(out, "takRvalidator")
    return(out)
}

## run_validators() runs the validator(s) and returns a (list of) predicate(s)
run_validators <- function(x) {
    out <- list()
    out <- add_class(out, "takRpredicates")
    # Checking the 
    if(!has_takRvalidators(x)){
        out[[1]] <- make_predicate(context = "Verifying validators", 
                                   msg="No validators present. No action taken")
        return(out)
    }
    if(!inherits(validators(x), "list")){
        out[[1]] <- make_predicate(logi = FALSE, context = "Verifying validators", 
                                   err = "The 'takRvalidators' attribute does not contain a list")
        return(out)
    }
    v <- all(unlist(lapply(validators(x), inherits, what = "takRvalidator")))
    if(!v){
        out[[1]] <- make_predicate(logi = FALSE, context = "Verifying validators", 
                                   err = "Some (or all) of the validators are not of class 'takRvalidator'",
                                   msg = "Use validators()")
        return(out)
    }
    # v <- all(unlist(lapply(validators(x), inherits, what = "function")))
    # if(!v){
    #     out <- make_predicate(logi = FALSE, context = "Verifying validators", 
    #                           err = "Some (or all) of the validators are not functions",
    #                           msg = "Use make_validator()")
    #     return(out)
    # }
    
    # If we get this far then it all checks out, then apply the fucntions and return the outcome(s) 
    for(a in seq_along(validators(x))){
        out[[a]] <- run_validator(x, validators(x)[[a]])
    }
    return(out)
}

run_validator <- function(x, validator){
    if(!inherits(validator$v, what = "function")){
        out <- make_predicate(logi = FALSE, context = validator$context,
                              err = "The validator does not contain a function",
                              msg = "Use make_validator()")
        return(out)
    }
    out <- validator$v(x)
    res <- out[['res']]
    err <- out[['err']]
    wrn <- out[['wrn']]
    msg <- out[['msg']]
    
    
    # err <- wrn <- msg <- NULL
    # # Run the function, capturing errors, warnings and messages
    # #full_args <- c(validator$arguments)
    # # TODO: Need to figur eout how to pass arguments here
    # res <- withCallingHandlers(
    #     tryCatch(validator$f(x, `...` = validator$arguments), error=function(e) {
    #         err <<- conditionMessage(e)
    #         return(FALSE)
    #     }), warning=function(w) {
    #         wrn <<- append(wrn, stringr::str_trim(conditionMessage(w)))
    #         invokeRestart("muffleWarning")
    #     }, message = function(m) {
    #         msg <<- append(msg, stringr::str_trim(conditionMessage(m)))
    #         invokeRestart("muffleMessage")
    #     })
    
    # If there are NAs, then treat them based on the value of "treat_na_as"
    if(any(is.na(res))){
        res[is.na(res)] <- validator$treat_na_as
        wrn <- append(wrn, sprintf("Validator replaced NA(s) with %s", validator$treat_na_as))
    }
    
    # Check for logical
    if(!is.logical(res)){
        err <- append(err, sprintf("Validator did not return a logical (returned: %s)", mode(res)))
        res <- FALSE
    }
    
    # Apply ALL or ANY (if needed)
    if(length(res) > 1){
        res <- validator$any_or_all_f(res)
    }
    
    # # If the result is not indeterminate (i.e. logical(0)), then check it meets expectations
    # if(length(res) && !(res == validator$expect)){
    #     err <- append(err, sprintf("Result did not meet expection. Got %s. Expected %s", res, validator$expect))
    #     res <- FALSE
    # }
    
    # Build out object
    out <- make_predicate(logi = res, context = validator$context,
                          err = err, wrn = wrn, msg = msg)
    return(out)
}

# Get the default validators
get_default_validators <- function(x){
    ndx <- match(class(x), names(default_takRvalidators))
    if(all(is.na(ndx))){
        warning(sprintf("No default validators for objects of class('%s').", class(x)))
        return(NULL)
    }
    ndx <- ndx[!is.na(ndx)]
    out <- list()
    for(a in rev(seq_along(ndx))){
        out <- c(out, default_takRvalidators[[ndx[a]]])
    }
    names(out) <- NULL
    out <- add_class(out, "takRvalidators")
    return(out)
}

# Methods
validate <- function(x, ...){
    UseMethod("validate")
}

validate.default <- function(x, ...) {
    attr(x, "takRpredicates") <- run_validators(x)
    print(attr(x, "takRpredicates"), ...)
    invisible(x)
}

validate.takRcollection <- function(x, ...) {
    # Need to apply validation and then loop to validate objects...
    for(a in seq_along(x)){
        rgs <- list(...)
        if(!is.null(names(x))){
            rgs[['header']] <- names(x)[a]
        } else {
            rgs[['header']] <- paste0("[[", a, "]]")
        }
        rgs[["x"]] <- x[[a]]
        x[[a]] <- do.call("validate", args = rgs)
    }
    attr(x, "takRpredicates") <- run_validators(x)
    print(attr(x, "takRpredicates"), ...)
    invisible(x)
}

validate.takRdef <- function(x, ...){
    for(a in seq_along(x)){
        rgs <- list(...)
        if(!is.null(names(x))){
            rgs[['header']] <- names(x)[a]
        } else {
            rgs[['header']] <- paste0("[[", a, "]]")
        }
        rgs[["x"]] <- x[[a]]
        x[[a]] <- do.call("validate", args = rgs)
    }
    attr(x, "takRpredicates") <- run_validators(x)
    print(attr(x, "takRpredicates"), ...)
    invisible(x)
}

# Not exported below this point.

## make_predicate() produces a empty (defualt) predicate
make_predicate <- function(logi = logical(0), context = "", 
                             err = NULL, wrn = NULL, msg = NULL){
    out <- list("logi" = logi, "context" = context, 
                "err" = err, "wrn" = wrn, "msg" = msg)
    out <- add_class(out, "takRpredicate")
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
    label <- paste0(stringr::str_pad(label, pad, side = "left"), ": ")
    cat(label, sep="")
    for(a in seq_along(x)){
        if(a == 1L){
            cat(x[a], "\n", sep="")
        } else {
            tab <- stringr::str_length(label)
            cat(rep(" ", times = tab), x[a], "\n", sep="")
        }
    }
}

