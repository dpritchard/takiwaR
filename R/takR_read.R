# Read Functions
## Functions to read data (excel spreadsheets)

# Relies on definitions in 

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
    return(raw)
}

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
    sections <- find_sec(raw, sections)
    
    ## Extract sections
    out <- extract_raw(raw, sections)
    
    ## Clean data
    out <- clean_raw(out)
    
    ## Validate data
    #out <- validate_raw(out)
    
}

find_sec <- function(raw, sections, eof="EOF"){
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
        # Add section metadata
        attr(out[[snmes[a]]], 'takRsmeta') <- sections[[snmes[a]]]
    }
    return(out)
}

clean_raw <- function(raw){
    out <- list() # Prepare an "out" object
    snmes <- names(raw) # snmes = Section Names
    for(a in seq_along(snmes)){
        tdat <- raw[[snmes[a]]] # tdat = temp dat
        smeta <- attr(x = tdat, which = 'takRsmeta', exact = TRUE)
        if(smeta$class == 'takRmeta' || smeta$input_dir == 'keyval'){
            print(paste(snmes[a], ": KEYVAL"))
            out[[snmes[a]]] <- clean_keyval(tdat)
        } else if(smeta$input_dir == 'mixed') {
            print(paste(snmes[a], ": MIXED"))
        } else if(smeta$input_dir == 'row') {
            print(paste(snmes[a], ": ROW"))
        } else if(smeta$input_dir == 'col') {
            print(paste(snmes[a], ": COL"))
        } else {
            print(paste(snmes[a], ": UNABLE TO CLEAN"))
        }
        
    }
    return(out)
}

clean_keyval <- function(raw){
    raw <- raw[-1,] # Drop row 1 as it is always an empty row
    emptyrows <- apply(raw, 1, all_is_na) # Detect totally empty (NA only) rows
    raw <- raw[!emptyrows,] # Remove empty rows
    raw_out <- list()
    for(a in 1:nrow(raw)){
        key <- make_key(raw[a,1])
        vals <- raw[a,-1]
        vals <- vals[!is.na(vals)]
        vals <- as.character(vals)
        vals <- stringr::str_trim(vals)
        vals <- type.convert(vals, as.is=TRUE)
        if(length(vals) > 0){
            raw_out[[key]] <- vals
        }
    }
    return(raw_out)
}

clean_takRdata <- function(dat, quad_dir, n_quad, data_pfix = FALSE, implicit_zeros = TRUE){
    # quad_dir="col" means quadrats along columns
    # quad_dir="row" means quadrats down rows
    
    # Returns an object of class 'takRwide' (or 'takRempty')
    
    quad_dir <- tolower(quad_dir)
    # First drop the first row (which is always a header row)
    dat <- dat[-1,]
    # Extract and drop the first column.
    first_col <- dat[,1] # Extract data names from the first column
    first_col <- stringr::str_trim(first_col) # Strip whitespace
    dat <- dat[,-1] # Drop it
    
    # Next check there is at least enough columns to proceed
    if(quad_dir == "col" & ncol(dat) < n_quad){
        stop(paste("There aren't", n_quad, "columns in the supplied data."))
    }
    if(quad_dir == "row" & nrow(dat) < n_quad){
        stop(paste("There aren't", n_quad, "rows in the supplied data."))
    }
    
    # Deal with the data in "quadrats in columns" direction.
    if(quad_dir == "col"){
        if(ncol(dat) > n_quad){
            dropcols <- seq(from = n_quad+1, to = ncol(dat), by = 1)
            droped <- dat[,dropcols]
            if(!all_is_na(droped)){
                warning(paste0("Data exist in columns that were excluded using n_quad = ", n_quad, "."), immediate. = TRUE, call. = FALSE)
            }
            dat <- dat[,-dropcols, drop=FALSE]
        }
        droprows <- apply(dat, 1, all_is_na) # Get rows that have nothing
        dat <- dat[!droprows, , drop=FALSE] # Drop empty rows
        ## If there are no rows at this point, we can just return NA
        if(is_zero(nrow(dat))){
            out <- NA
            class(out) <- c("takRempty", class(out))
            return(out)
        }
        
        first_col <- first_col[!droprows] # Update the first column info
        # Coerce what remains into a numeric matrix
        dat <- apply(dat, 2, as.numeric)
        if(implicit_zeros){
            # Replace NA's with zeros
            dat[is.na(dat)] <- 0
        }
        if(is.matrix(dat)){
            dat <- t(dat) # Transpose "wide" data, so that quadrats are in rows
        } else {
            dat <- as.matrix(dat)
        }
    }
    
    # Deal with the data in "quadrats in rows" direction.
    if(quad_dir == "row"){
        if(nrow(dat) > n_quad){
            droprows <- seq(from = n_quad+1, to = nrow(dat), by = 1)
            droped <- dat[droprows,]
            if(!all_is_na(droped)){
                warning(paste0("Data exist in rows that were excluded using n_quad = ", n_quad, "."),
                        immediate. = TRUE, call. = FALSE)
            }
            dat <- dat[-droprows, , drop=FALSE]
            first_col <- first_col[!droprows] # Update the first column info
        }
        dropcols <- apply(dat, 2, all_is_na) # Get columns that have nothing
        dat <- dat[,!dropcols, drop=FALSE] # Drop empty columns
        ## If there are no columns at this point, we can just return NA
        if(is_zero(ncol(dat))){
            out <- NA
            class(out) <- c("takRempty", class(out))
            return(out)
        }
        
        # Coerce what remains into a numeric matrix
        dat <- apply(dat, 2, as.numeric)
        if(implicit_zeros){
            # Replace NA's with zeros
            dat[is.na(dat)] <- 0
        }
    }
    
    ## Name rows (quadrats)
    # Always named 001:n
    rownames(dat) <- sprintf("%03d", 1:nrow(dat))
    
    ## TODO: Document this. Enforce this in all objects of 'takRwide'
    
    ## Name columns (data)
    if(ncol(dat) > 0){
        if(is.character(data_pfix)){
            colnames(dat) <- paste0(data_pfix, sprintf("%01d", 1:ncol(dat)))
        } else if (!all_is_na(first_col) & quad_dir == "col") {
            colnames(dat) <- first_col
        } else {
            colnames(dat) <- NULL
        }
    }
    
    ## Construct the return object
    out <- NULL
    if(ncol(dat) > 0){
        out <- dat # Data is an object with nquad rows
        class(out) <- c("takRwide", class(out))
    } else {
        # This might be unneccesary... See above...
        out <- NA
        class(out) <- c("takRempty", class(out))
        return(out)
    }
    return(out)
}





