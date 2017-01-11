# Read definition objects (`takRdef`)
# These things are complicated... But hopefully much of the complexity will be hidden from users.
takRblock <- function(name = NULL, str_in = NULL, str_out = NULL, text = NULL, fill = NULL, n = NULL){
    # If name, str_in and str_out are all NULL, return an empty block
    if(all(is.null(c(name, str_in, str_out)))){
        out <- takRblock_empty(.str_in = str_in, .str_out = str_out, .text = text, .fill = fill, .n = n)
    }
    # If we get this far without returning, we need one of: name, str_in or str_out
    lens <- lengths(list(name, str_in, str_out)) # Should all individually be less than 2 (1 or 0, really)
    if(sum(lens) != 1){
        stop("You must provide one of 'name', 'str_in' or 'str_out'")
    }
    if(!is.null(name)){
        # If name not null, try takRblock_by_name
        out <- takRblock_by_name(.name = name, .str_in = str_in, .str_out = str_out, .text = text, .fill = fill, .n = n)
    } else if(!is.null(str_in)){
        # If str_in not null, try takRblock_by_in_str
        out <- takRblock_by_in_str(.str_in = str_in, .str_out = str_out, .text = text, .fill = fill, .n = n)
    } else {
        # If str_out not null, try takRblock_by_out_str (this is the only remaining option)
        out <- takRblock_by_out_str(.str_in = str_in, .str_out = str_out, .text = text, .fill = fill, .n = n)
    }
    return(out)
}

# HELPER FUCNTIONS: NOT EXPORTED
is_default_block_name <- function(x){
    if(!length(x)){
        return(FALSE)
    }
    match(x, names(takiwaR:::blocks), nomatch = 0) > 0
}
is_default_str_in <- function(x){
    if(!length(x)){
        return(FALSE)
    }
    match(x, names(takiwaR:::in_strs), nomatch = 0) > 0
}
is_default_str_out <- function(x){
    if(!length(x)){
        return(FALSE)
    }
    match(x, names(takiwaR:::out_strs), nomatch = 0) > 0
}
# Get valid output structure(s), based on the input structure type
get_valid_str_out <- function(str_in){
    out <- takiwaR:::out_strs[[str_in]][["str_out"]]
    return(out)
}
# Get valid input structure(s), based on the output structure type
get_valid_str_in <- function(str_out){
    out <- takiwaR:::out_strs[[str_out]][["str_in"]]
    return(out)
}

# Note: takRblock_by_* functions return the **content of** the defualt section blocks (i.e. it is up to the uder to name them)
# Validate input into takRblock_empty and takRblock_by_* family
validate_block_input <- function(.text = NULL, .str_in = NULL, .str_out = NULL, .fill = NULL, .n = NULL){
    lens <- lengths(list(.text, .str_in, .str_out, .fill, .n))
    if(any(lens > 1)){
        stop("Can only process one input at a time")
    }
    if(!is.null(.text) && !is.character(.text)){stop("`text` must be a character string", call. = FALSE)}
    if(!is.null(.str_in) && !is.character(.str_in)){stop("`str_in` must be a character string", call. = FALSE)}
    if(!is.null(.str_out) && !is.character(.str_out)){stop("`str_out` must be a character string", call. = FALSE)}
    # Fill can be anything?
    if(!is.null(.n) && !is.wholenumber(.n)){stop("`n` must be an integer", call. = FALSE)}
    # Checking validity... 
    if(!is.null(.str_in) && !is_default_str_in(.str_in)){
        msg <- paste0("`", .str_in, "` is not a valid input structure\n",
                      "Options are: ", paste(names(takiwaR:::in_strs), collapse = ", "))
        stop(msg)
    }
    if(!is.null(.str_out) && !is_default_str_in(.str_out)){
        msg <- paste0("`", .str_out, "` is not a valid output structure\n",
                      "Options are: ", paste(names(takiwaR:::out_strs), collapse = ", "))
        stop(msg)
    }
    if(is.null(.n) && !is.null(.str_in) && .str_in %in% c("mrow", "mcol")){
        stop("`n` can not be NULL for matrix input structures")
    }
}

# Generate a single block. 
# If called with no arguments, it creates and empty block. Otherwise the provided options are used.
# EXPORTED
takRblock_empty <- function(.text = NULL, .str_in = NULL, .str_out = NULL, .fill = NULL, .n = NULL){
    # The following required to overcome recursive promises...
    text <- .text
    str_in <-  .str_in
    str_out <- .str_out
    fill <- .fill
    n <- .n
    validate_block_input(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill, .n = n)
    out <- list("text" = text, "str_in" = str_in, "str_out" = str_out, "fill" = fill, "n" = n)
    out <- add_class(out, "takRblock")
    return(out)
}

# Generate a single block (section) based on a named input
# EXPORTED
takRblock_by_name <- function(.name, .text = NULL, .str_in = NULL, .str_out = NULL, .fill = NULL, .n = NULL){
    # The following required to overcome recursive promises...
    name <- .name
    text <- .text
    str_in <-  .str_in
    str_out <- .str_out
    fill <- .fill
    n <- .n
    validate_block_input(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill, .n = n)
    # Check that name is a default block name
    if(!is_default_block_name(name)){
        msg <- paste0("`", name, "` is not the name of a default block\n",
                      "Options are: ", paste(names(takiwaR:::blocks), collapse = ", "))
        stop(msg, call. = FALSE)
    }
    out <- takiwaR:::blocks[[name]]
    if(!is.null(text)) out[["text"]] <- text
    if(!is.null(str_in)) out[["str_in"]] <- str_in
    if(!is.null(str_out)) out[["str_out"]] <- str_out
    if(!is.null(fill)) out[["fill"]] <- fill
    # stop("Fuckup here. Fixme!")
    # We should be using validate here! But the defualt validation section is not ready yet
    if(!is.null(n)) out[["n"]] <- n
    return(out)
}

# Generate a single block based on an input type, assuming defaults
# EXPORTED
takRblock_by_in_str <- function(.str_in, .text = NULL, .str_out = NULL, .fill = NULL, .n = NULL){
    # The following required to overcome recursive promises...
    text <- .text
    str_in <-  .str_in
    str_out <- .str_out
    fill <- .fill
    n <- .n
    validate_block_input(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill, .n = n)
    # If str_out is NULL, get the default for this input structure
    if(is.null(str_out)){
        str_out <- get_valid_str_out(str_in)
        if(length(str_out) > 1){
            msg <- paste0("There are multiple valid output structures for this input structure.\n",
                          "Defaulting to `", str_out[1], "`.")
            warning(msg, call. = FALSE)
            str_out <- str_out[1]
        }
    }
    # If fill is NULL, get the default.
    if(is.null(fill)){
        fill <- takiwaR:::out_strs[[str_out]][["fill"]]
    }
    # If the input is a matrix type create an object with n. Otherwise, don't.
    if(str_in %in% c("mrow", "mcol")){
        out <- takRblock_empty(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill, .n = n)
    } else {
        out <- takRblock_empty(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill)
    }
    # TODO: SET VALIDATORS e.g. Enforce n for matirx objects
    return(out)
}

# Generate a single section based on an output type, assuming defaults
# EXPORTED
takRblock_by_out_str <- function(.str_out, .text = NULL, .str_in = NULL, .fill = NULL, .n = NULL){
    # The following required to overcome recursive promises...
    text <- .text
    str_in <-  .str_in
    str_out <- .str_out
    fill <- .fill
    n <- .n
    validate_block_input(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill, .n = n)
    # If str_in is NULL, get the default for this output structure
    if(is.null(str_in)){
        str_in <- get_valid_str_in(str_out)
        if(length(str_in) > 1){
            msg <- paste0("There are multiple valid output structures for this input structure.\n",
                          "Defaulting to `", str_in[1], "`.")
            warning(msg, call. = FALSE)
            str_in <- str_in[1]
        }
    }
    # If fill is NULL, get the default.
    if(is.null(fill)){
        fill <- takiwaR:::out_strs[[str_out]][["fill"]]
    }
    # If the input is a matrix type create an object with n. Otherwise, don't.
    if(str_in %in% c("mrow", "mcol")){
        out <- takRblock_empty(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill, .n = n)
    } else {
        out <- takRblock_empty(.text = text, .str_in = str_in, .str_out = str_out, .fill = fill)
    }
    # TODO: SET VALIDATORS e.g. Enforce n matrix objects
    return(out)
}

# Generate a complete section definition item, optionally taking names of known default sections
# EXPORTED
new_read_definition <- function(sec_names = NULL, n = NULL){
    def <- takRdef_empty()
    # If sec_names is NULL, return just a metadata section.
    if(all(is.null(sec_names))){
        def[["meta"]] <- takiwaR:::blocks[["meta"]]
        return(def)
    }
    # Otherwise, we need to build the section object from the input and defaults
    # All must be unique
    if(length(sec_names)!=length(unique(sec_names))){
        stop("All of the objects in 'sec_names' must be unique")
    }
    # Once we have confirmed they are unique, we:
    # 1) Check that "meta" is requested. If not, add it.
    if(!"meta"%in%sec_names){
        sec_names <- c("meta", sec_names)
        msg <- "A 'meta' section was automatically added."
        warning(msg, call. = FALSE)
    }
    
    # 2) Add all sections, fill with defualts, if the name exists
    empty_blocks <- NULL
    for(a in seq_along(sec_names)){
        nme <- sec_names[a]
        if(is_default_block_name(nme)){
            def[[nme]] <- takRblock_by_name(nme)
        } else {
            def[[nme]] <- takRblock_empty()
            empty_blocks <- append(empty_blocks, nme)
        }
    }
    if(!is.null(empty_blocks)){
        msg <- paste0("Empty blocks substitued for ", paste(empty_blocks, collapse = ", "), "\n", 
                      "Manual intervention will be required")
        warning(msg, call. = FALSE)
    }
    
    # # 3) Add section objects with defaults if they exist.
    # ndx_default <- match(sec_names, names(takiwaR:::blocks))
    # indx <- is.na(ndx_default)
    # # If there are NA's in ndx_default, it's becuase there are no defualts
    # if(any(indx)){
    #     msg <- paste("Empty blocks substitued for one or more sections.\nManual intervention will be required.")
    #     warning(msg, call. = FALSE)
    # }
    # default_nmes <- sec_names[!indx]
    # for(a in seq_along(default_nmes)){
    #     def[[default_nmes[a]]] <- takiwaR:::blocks[[default_nmes[a]]]
    # }
    # # OK. So at this point we have a placeholder section list.
    # # 4) If the any object inherits from takRwide, then n is compulsory
    # # Check and apply
    # indx <- sapply(def, out_str_inherits, what = "takRwide")
    # if(any(indx, na.rm = TRUE)){
    #     if(is.null(n) || is.na(n)){
    #         stop("`n` cannot be NULL or NA for objects of takRwide")
    #     }
    #     for(a in which(indx)){
    #         def[[a]][["n"]] <- n
    #     }
    # }
    return(def)
}

# A simple function to build a full inherited class definition from the above...
# NOT EXPORTED
out_str_class <- function(x){
    if(!x %in% names(takiwaR:::out_strs)){
        return(NULL)
    }
    out_class <- c(x, takiwaR:::out_strs[[x]][['inherits']])
    return(out_class)
}

# Check if a single output structure (x) would inherit a particular another output structure
# NOT EXPORTED
out_str_inherits <- function(x, what, which = FALSE){
    x_out <- x[["str_out"]]
    if(is.null(x_out)){
        return(NA)
    }
    if(x_out %in% names(takiwaR:::out_strs)){
        x_out <- c(x_out, takiwaR:::out_strs[[x_out]][["inherits"]])
    }
    indx <- stringr::str_detect(x_out, what)
    if(which){
        return(indx)
    } else {
        return(any(indx))
    }
}

# Generate an empty definition object. Helper function.
# NOT EXPORTED
takRdef_empty <- function(){
    def <- list()
    def <- add_class(def, "takRdef")
    return(def)
}