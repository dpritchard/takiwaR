# Read Functions
## Functions to read data (excel spreadsheets)

read_takRexcel <- function(file, sections, use_logfile = FALSE){
    ## Close the logfile on exit, even (perhaps, especially) if there is an error.
    on.exit(expr = {
        if(use_logfile){
            sink(type="message")
            close(logfile)
        }
    })
    ## Use a logfile, if requested
    if(use_logfile){
        logfile <- paste0(tools::file_path_sans_ext(file), ".TAKlog")
        logfile <- file(logfile, open="wt")
        sink(file = logfile, type = "message")
    }
    
    ## Start reading
    message("Reading file '", file, "'")
    raw <- read_raw_xl(file)
    
    ## Find sections
    message("Finding sections...")
    sections <- find_sections(raw, sections)
    
    ## Extract sections
    message("Extracting data into sections...")
    out <- extract_raw(raw, sections)
    
    ## Clean data
    message("Cleaning sections...")
    out <- clean(out)
    
    ## Validate data
    message("Validating sections...")
    out <- validate(out, header = "Main Collection")
    
    message("Done.")
    return(out)
}

# SUPPORT FUNCTIONS: NOT EXPORTED BELOW HERE
read_raw_xl <- function(file){
    # Sorting out readxl's guessing of column numbers...
    # The main issue is that currently (2016-10-09) `readxl:::xls_col_types` only considers the top 100 rows..
    # This can result on some data being lost, at the end of the sheet.
    # This only applies to XLS files (not XLSX), becuase we get the required information from `xlsx_dim` for these files.
    # Also we are forcing all data to be read as "text". We will coerce it later.
    ext <- tools::file_ext(file)
    if(ext == "xlsx"){
        col_types <- rep("text", readxl:::xlsx_dim(path = file)[2])
        col_names <- paste0("col", 1:length(col_types))
    } else if(ext =="xls"){
        warning("Old Excel file format, assuming less than 1000 rows.",
                immediate. = TRUE, call. = FALSE)
        col_types <- readxl:::xls_col_types(path = file, n = 1000L)
        col_types <- rep("text", length(col_types))
        col_names <- paste0("col", 1:length(col_types))
    } else {
        stop("Only excel files are supported. I know. Sorry.")
    }
    raw <- readxl::read_excel(file, col_names = col_names, col_types = col_types)
    raw <- data.frame(raw)
    return(raw)
}

find_sections <- function(raw, sections, eof="EOF"){
    if(!inherits(sections, "takRdef")){
        stop("'sections' must be of class takRdef (use `new_read_definition`)")
    }
    
    # Search for the sections in the first column
    sec_text <- sapply(sections, FUN = "[[", 1)
    sec_name <- names(sections)
    def_search <- tolower(stringr::str_replace_all(sec_text, "\\s", ""))
    eof_search <- tolower(stringr::str_replace_all(eof, "\\s", ""))
    firstcol <- tolower(stringr::str_replace_all(raw[,1], "\\s", ""))
    
    sec_matches <- match(def_search, firstcol)
    eof_match <- match(eof_search, firstcol)
    
    ## Check eof is matched
    if(is.na(eof_match)){
        message <- paste0("End of file marker (", eof, ") not found.")
        stop(message)
    }
    
    if(any(is.na(sec_matches))){
        missing <- sec_text[is.na(sec_matches)]
        message <- paste("The following section, or sections, were not found: \n", paste(missing, collapse = ", "))
        #warning(message, immediate. = TRUE, call. = FALSE)
        stop(message)
    }
    
    if(any(sec_matches > eof_match)){
        willexclude <- sec_text[sec_matches > eof_match]
        message <- paste("The following section, or sections, are found after the EOF and will be excluded: \n",
                         paste(willexclude, collapse = ", "))
        #warning(message, immediate. = TRUE, call. = FALSE)
        stop(message)
    }
    
    # Combine section start values into sections...
    for(a in 1:length(sections)){
        sections[[a]]['start'] <- sec_matches[a]
    }
    
    # Sort by start values
    sections <- sections[order(sapply(sections, '[[', 'start'))]
    
    # Sort out end values
    secminlast <- length(sections)-1
    # Use the next sections start value minus 1
    for(a in 1:secminlast){
        sections[[a]]['end'] <- sections[[a+1]][['start']]-1
    }
    # Except for the last section, which will be EOF minus 1
    sections[[length(sections)]]['end'] <- eof_match-1
    
    return(sections)
}

extract_raw <- function(raw, sections){
    # Get ready for the output
    out <- list() # Prepare an "out" object
    # Get names, start and end points
    snmes <- names(sections) # snmes = Section Names
    stext <- sapply(sections, "[[", "text") # stext = Section Text
    start <- sapply(sections, "[[", "start")
    end <- sapply(sections, "[[", "end")
    # Now loop over these
    for(a in 1:length(snmes)){
        message("Extracting ", stext[a], " as '", snmes[a], "'.")
        out[[snmes[a]]] <- raw[start[a]:end[a], ]
        # Assign the input class
        out[[snmes[a]]] <- add_class(out[[snmes[a]]], sections[[snmes[a]]]$str_in)
        # Append the section metadata (temporary only)
        attr(out[[snmes[a]]], 'takRsmeta') <- sections[[snmes[a]]]
    }
    out <- add_class(out, "takRcollection")
    return(out)
}

## NEW / EMPTY OBJETCS...
new_takRempty <- function(){
    x <- NA
    x <- add_class(x, "takRempty")
    return(x)
}

munge_raw_matrix <- function(x, smeta){
    # A fucntion to reduce duplication for matrix munging.
    # Takes a raw object (x) in the "wide" direction and returns a clean matrix.
    # Coerce to a character matrix 
    x <- as.matrix(x)
    
    col_nmes <- x[1, ] # Grab the header row...
    row_nmes <- x[, 1] # Grab the first col
    x <- x[-1, -1, drop=FALSE] # Drop the first row and col.
    col_nmes <- col_nmes[-1] # The first item is a section name
    row_nmes <- row_nmes[-1] # As above
    
    
    # Next check there is at least enough rows to proceed
    if(nrow(x) < smeta$n){
        msg <- sprintf("There aren't %s %s in the supplied data.", 
                       smeta$n, 
                       switch(smeta$str_in, mrow = "rows", mcol = "columns"))
        stop(msg)
    }
    
    # Drop extra rows, with a warning, if they exist
    if(nrow(x) > smeta$n){
        drop_ndx <- seq(from = smeta$n+1, to = nrow(x), by = 1)
        dropped <- x[drop_ndx,]
        if(!all_is_na(dropped)){
            msg <- sprintf("Data exist in %s that were excluded using n = %s.",
                           switch(smeta$str_in, mrow = "rows", mcol = "columns"), 
                           smeta$n)
            warning(msg, immediate. = TRUE, call. = FALSE)
        }
        x <- x[-drop_ndx,, drop=FALSE] # Drop the rows outside the data range
        row_nmes <- row_nmes[-drop_ndx] # Update the "row_nmes" variable
    }
    
    drop_indx <- apply(x, 2, all_is_na) # Get columns that have nothing
    x <- x[, !drop_indx, drop=FALSE] # Drop empty columns
    col_nmes <- col_nmes[!drop_indx] # Update the col_nmes info
    
    # If there are no rows at this point, we can just return NA
    if(is_zero(ncol(x))) return(new_takRempty())
    
    # Coerce what remains into a named numeric matrix
    x <- apply(x, c(1, 2), as.numeric) # TODO: Allow for flexible coercion.  
    if(all_is_na(row_nmes)) row_nmes <- 1:nrow(x)
    if(all_is_na(col_nmes)) col_nmes <- NULL
    dimnames(x) <- list(as.character(row_nmes), as.character(col_nmes))
    
    # Replace NA's with the fill variable
    if(!is.null(smeta$fill)){
        x[is.na(x)] <- smeta$fill
    }
    return(x)
}

munge_raw_df <- function(x, smeta){
    x <- as.matrix(x)
    col_nmes <- as.character(x[1, ]) # Grab the header row...
    row_nmes <- as.character(x[, 1]) # Grab the first col
    x <- x[-1, -1, drop=FALSE] # Drop the first row and col.
    col_nmes <- col_nmes[-1] # The first item is a section name
    row_nmes <- row_nmes[-1] # As above
    
    # Drop columns without names 
    drop_indx <- is.na(col_nmes)
    x <- x[, !drop_indx, drop=FALSE] # Drop empty columns
    col_nmes <- col_nmes[!drop_indx] # Update the col_nmes info
    
    # If there are no columns at this point, we can just return NA
    if(is_zero(ncol(x))) return(new_takRempty())
    
    # Drop rows
    drop_indx <- apply(x, 1, all_is_na) # Get rows that have nothing
    drop_indx <- drop_indx & is.na(row_nmes)
    x <- x[!drop_indx, , drop=FALSE] # Drop empty rows with no names
    row_nmes <- row_nmes[!drop_indx] # Update the row_nmes info
    
    # If there are no rows at this point, we can just return NA
    if(is_zero(nrow(x))) return(new_takRempty())
    
    # Replace NA's with the fill variable
    if(!is.null(smeta$fill)){
        x[is.na(x)] <- smeta$fill
    }
    
    # Coerce what remains into data.frame
    if(all_is_na(row_nmes)) row_nmes <- 1:nrow(x)
    if(all_is_na(col_nmes)) col_nmes <- NULL
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    names(x) <- col_nmes
    row.names(x) <- row_nmes
    
    return(x)
}


## THE "CLEAN" METHOD
clean <- function(x){
    # clean() should take an object with one of the "str_in" classes and:
    #   1) Munge / coerce to the required output structure.
    #   2) Replace / set the "str_out" classes.  
    #   3) Add default validators for that (those) "str_out" class(es).
    #   4) Remove the section metadata from the attributes.  
    UseMethod("clean")
}

clean.default <- function(x){
    msg <- sprintf("Don't know how to clean an object of class %s", 
                   paste0(x, collapse = "/"))
    warning(msg, call. = FALSE, immediate. = TRUE)
    return(x)
}

clean.takRcollection <- function(x){
    ## IS THERE ANYTHING TO DO TO THE CORE OBJECT?
    for(a in seq_along(x)){
        x[[a]] <- clean(x[[a]])
    }
    return(x)
}

clean.row <- function(x){
    smeta <- attr(x, "takRsmeta")
    attr(x, "takRsmeta") <- NULL # Save, then strip the section metadata
    
    x <- x[-1,] # Drop row 1 as it is always an empty row for these objects
    emptyrows <- apply(x, 1, all_is_na) # Detect totally empty (NA only) rows
    x <- x[!emptyrows,] # Remove empty rows
    
    x_out <- list()
    for(a in 1:nrow(x)){
        key <- make_key(x[a,1])
        vals <- x[a,-1]
        vals <- vals[!is.na(vals)] # Drop empty cells
        vals <- as.character(vals) # Ensure we are still a character
        vals <- stringr::str_trim(vals)
        vals <- type.convert(vals, as.is=TRUE)
        if(length(vals) > 0){
            x_out[[key]] <- vals
        }
    }
    #   2) Replace / set the "str_out" classes.  
    x_out <- add_class(x_out, out_str_class(smeta$str_out))
    
    #   3) Add default validators for that (those) "str_out" class(es).
    validators(x_out) <- get_default_validators(x_out)
    
    #   4) Remove the section metadata from the attributes. 
    #   No need here.
    return(x_out)
}

clean.col <- function(x){
    smeta <- attr(x, "takRsmeta")
    attr(x, "takRsmeta") <- NULL # Save, then strip the section metadata
    
    x <- munge_raw_df(x, smeta)
    if(inherits(x, what = "takRempty")) return(x)
    
    # Replace / set the "str_out" classes.  
    x <- add_class(x, out_str_class(smeta$str_out))
    
    # Add default validators
    validators(x) <- get_default_validators(x)
    return(x)
}

clean.mrow <- function(x){
    smeta <- attr(x, "takRsmeta")
    attr(x, "takRsmeta") <- NULL # Save, then strip the section metadata
    
    x <- munge_raw_matrix(x, smeta)
    if(inherits(x, what = "takRempty")) return(x)
    
    # Replace / set the "str_out" classes.  
    x <- add_class(x, out_str_class(smeta$str_out))
    
    # Add default validators
    validators(x) <- get_default_validators(x)
    return(x)
}

clean.mcol <- function(x){
    smeta <- attr(x, "takRsmeta")
    attr(x, "takRsmeta") <- NULL # Save, then strip the section metadata
    
    x <- t(x)
    x <- munge_raw_matrix(x, smeta)
    if(inherits(x, what = "takRempty")) return(x)
    
    # Replace / set the "str_out" classes.  
    x <- add_class(x, out_str_class(smeta$str_out))
    
    # Add default validators
    validators(x) <- get_default_validators(x)
    return(x)
    
}


