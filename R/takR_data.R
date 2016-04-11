read_takRbt <- function(file, use_logfile = FALSE, sections = default_takRsec("takRbt")){
    ## Close the logfile on exit, even (perhaps, especially) if there is an error.  
    on.exit(expr = {
        if(use_logfile){
            sink(type="message")
            close(logfile)
        }
    })
    
    if(use_logfile){
        logfile <- paste0(tools::file_path_sans_ext(file), ".TAKlog")
        logfile <- file(logfile, open="wt")
        sink(file = logfile, type = "message")
    }
    message("Reading file '", file, "'")
    
    ## Sorting out readxl's guessing of column numbers
    # Also forcing all data to be read as "text". We will coerce it later.  
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
    raw <- data.frame(raw) # Fucking datatable bullshit
    
    # Now, finding sections
    message("Finding sections...")
    sections <- find_takRsec(raw, sections)
    
    # Get ready for the output
    out <- list() # Prepare an "out" object
    
    # Get names, start and end points 
    section_names <- names(sections)
    section_text <- sapply(sections, "[[", "text")
    start <- sapply(sections, "[[", "start")
    end <- sapply(sections, "[[", "end")
    
    # Now loop over these
    for(a in 1:length(section_names)){
        message("Extracting ", section_text[a], " as '", section_names[a], "'.")
        out[[section_names[a]]] <- raw[start[a]:end[a], ]
    }
    
    ## Metadata...
    message("Processing meta...")
    out[['meta']] <- clean_takRmeta(out[['meta']])
    # Add the "file_name" to the metadata
    out[['meta']] <- add_takRmeta(out[['meta']], add=list("file_name" = file))
    # Add the class
    class(out[['meta']]) <- c("takRmeta", class(out[['meta']]))
    # Validate metadata
    message("Validating meta...")
    out[['meta']] <- takRvalidate(out[['meta']], req_vals = sections$meta$required)
    # Extract n_quad, needed to process data...
    n_quad <- out[['meta']][['n_quad']]
    
    # Process Data...
    data_sections <- sections[!sapply(sections, function(x){x$class=="takRmeta"})]
    
    for(a in 1:length(data_sections)){
        ss <- data_sections[[a]] # ss = Single Section
        ss_name <- names(data_sections[a]) # Names of these single sections
        iz <- ifelse(ss[["class"]]=="takRsf", FALSE, TRUE) # Implicit zeros for everything except size-frequency
        
        message("Processing section '", ss_name, "'...")
        cd <- clean_takRdata(out[[ss_name]], quad_dir = ss[["quad_dir"]], 
                             n_quad = n_quad, implicit_zeros = iz)
        # Add the class 
        class(cd) <- c(ss[["class"]], class(cd))
        # Add optional section data attributes
        opt_names <- names(vals_takRsec(show=FALSE)$opt)
        for(a in 1:length(opt_names)){
            attr(cd, opt_names[a]) <- ss[[opt_names[a]]]
        }
        # Add meta data as data attributes (for portability)
        cd <- toattr_takRmeta(cd, meta=out[["meta"]])
        
        # Validate the data    
        message("Validating data in section '", ss_name, "'...")
        vd <- takRvalidate(cd)
        out[[ss_name]] <- vd
    }
    
    # Return this
    class(out) <- c("takRbt", class(out))
    return(out)
}

vals_takRsec <- function(show=TRUE){
    req <- list(text="The text to search for",
                class="The takiwaR class of the section",
                quad_dir="Quadrat direction. Either 'col' or 'row'. Not required for 'meta'")
    opt <- list(takRsec_range="The maximum and minimum that the values in this section can take", 
                takRsec_units="The units of values in this section")
    if(show){
        #cat("Supported attributes in takiwaR sections\n")
        #cat("----------------------------------------\n")
        max_pad <- max(stringr::str_length(c(names(req), names(opt))))
        cat("Required:\n")
        for(a in 1:length(req)){
            cat(stringr::str_pad(names(req[a]), max_pad), ": ", req[[a]], ".\n", sep="")
        }
        cat("\nOptional:\n")
        for(a in 1:length(opt)){
            cat(stringr::str_pad(names(opt[a]), max_pad), ": ", opt[[a]], ".\n", sep="")
        }
    }
    invisible(list(req=req, opt=opt))
}

default_takRsec <- function(type = NULL){
    sections <- list()
    class(sections) <- c("takRsec", class(sections))
    if(is.null(type)){
        return(sections)
    } else if(type == "takRbt"){
        sections[["meta"]] <- list(text="takiwaR Metadata", class="takRmeta", 
                                   required = c("site","date","depth","n_quad","quad_size","gps_lat","gps_long"))
        sections[["substrate"]] <- list(text="Substrate (% Cover)", class="takRperc", quad_dir="col")
        sections[["prim_prod_p"]] <- list(text="Primary Producers (% Cover)", class="takRperc", quad_dir="col")
        sections[["prim_prod_c"]] <- list(text="Primary Producers (Counts)", class="takRcount", quad_dir="col")
        sections[["creat_p"]] <- list(text="Creatures (% Cover)", class="takRperc", quad_dir="col")
        sections[["creat_c"]] <- list(text="Creatures (Counts)", class="takRcount", quad_dir="col")
        sections[["iris_sf"]] <- list(text="Iris size frequency", class="takRsf", quad_dir="row", 
                                      takRsec_range=c(10,300), takRsec_units="mm")
        sections[["australis_sf"]] <- list(text="Australis size frequency", class="takRsf", quad_dir="row", 
                                           takRsec_range=c(10,150), takRsec_units="mm")
        sections[["chloroticus_sf"]] <- list(text="Chloroticus size frequency", class="takRsf", quad_dir="row", 
                                             takRsec_range=c(10,500), takRsec_units="mm")
        return(sections)
    } else {
        stop("There is no default for that type.")
    }
}

find_takRsec <- function(raw, sections, eof="EOF"){
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
        stop(paste0("End of file marker (", eof, ") not found."))
    }
    
    if(any(is.na(sec_matches))){
        missing <- sec_text[is.na(sec_matches)]
        message <- paste("The following section, or sections, were not found: \n", paste(missing, collapse = ", "))
        warning(message, immediate. = TRUE, call. = FALSE)
        # Remove sections and section start values
        sections <- sections[!is.na(sec_matches)]
        sec_matches <- sec_matches[!is.na(sec_matches)]
    }
    
    if(any(sec_matches > eof_match)){
        willexclude <- sec_text[sec_matches > eof_match]
        message <- paste("The following section, or sections, are found after the EOF and will be excluded: \n", 
                         paste(willexclude, collapse = ", "))
        warning(message, immediate. = TRUE, call. = FALSE)
        # Remove sections and section start values
        sections <- sections[!sec_matches > eof_match]
        sec_matches <- sec_matches[!sec_matches > eof_match]
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
    
    class(sections) <- c("takRsec", class(sections))
    return(sections) 
}

clean_takRmeta <- function(meta){
    meta <- meta[-1,] # Drop row 1
    emptyrows <- apply(meta, 1, all_is_na) # Detect totally empty (NA only) rows
    meta <- meta[!emptyrows,] # Remove empty rows
    meta_out <- list()
    for(a in 1:nrow(meta)){
        key <- make_key(meta[a,1])
        vals <- meta[a,-1]
        vals <- vals[!is.na(vals)]
        vals <- as.character(vals)
        vals <- stringr::str_trim(vals)
        vals <- type.convert(vals, as.is=TRUE)
        if(length(vals) > 0){
            meta_out[[key]] <- vals  
        }
    }
    return(meta_out)
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

## Potential methods? Not yet... Only objects are only classed after validation
add_takRmeta <- function(meta, add=list()){
    if(!is.list(add) | is_zero(length(names(add)))){
        stop("'add' must be a named list")
    }
    if(is_zero(length(add))){
        stop("'add' cannot be an empty list.")
    }
    for(name in names(add)){
        key <- make_key(name)
        if(key %in% names(meta)){
            meta[[key]] <- c(meta[[key]], add[[name]])
        } else {
            meta[[key]] <- add[[name]]
        }
    }
    return(meta)
}

set_takRmeta <- function(meta, set=list()){
    if(!is.list(set) | is_zero(length(names(meta)))){
        stop("'set' must be a named list")
    }
    if(is_zero(length(set))){
        stop("'set' cannot be an empty list.")
    }
    for(name in names(set)){
        key <- make_key(name)
        meta[[key]] <- set[[name]]
    }
    return(meta)
}

extract_takRmeta <- function(x1, ...){
    # Extract meta data from attributes in a table...
    # Returns a list of class 'takRmeta'
    all_inp <- c(list(x1), list(...))
    for(a in 1:length(all_inp)){
        dat <- all_inp[[a]]
        temp_out <- list()
        indx <- stringr::str_detect(names(attributes(dat)), "^(takRmeta_|takRsec_)")
        if(any(indx)){
            attr_names <- names(attributes(dat))[indx]
            #print(attr_names)
            for(b in 1:length(attr_names)){
                meta_name <- stringr::str_replace(attr_names[b], "^(takRmeta_|takRsec_)", "")
                temp_out[[meta_name]] <- attr(dat, attr_names[b])
            }
        }
        if(a == 1){
            ext_out <- temp_out
            class(ext_out) <- c("takRmeta", class(ext_out))
        } else {
            ext_out <- takRcombine(ext_out, temp_out)
        }
    }
    return(ext_out)
}

toattr_takRmeta <- function(obj, meta=list()){
    # Take an item of takRmeta and add it to the object's attributes...
    # Returns the object with the attributes added.  
    if(!inherits(meta, "takRmeta")){
        stop("'meta' must be of class 'takRmeta'")
    }
    meta_names <- names(meta)
    for(a in 1:length(meta_names)){
        atr_name <- paste0("takRmeta_", meta_names[a])
        attr(obj, atr_name) <- meta[[meta_names[a]]]
    }
    return(obj)
}

extract_long <- function(x, what, what_meta=NULL){
    # Extract a named item as a long-form data frame.
    # Optionally include meta data in columns.  
    if(is.null(x[[what]])){
        return(NULL)
    }
    if(all(is.na(x[[what]]))){
        return(NULL)
    }
    tmp <- takRlong(x[[what]])
    if(!is.null(what_meta)){
        for(a in 1:length(what_meta)){
            metadat <- attr(tmp, paste0("takRmeta_", what_meta[a]))
            if(length(metadat) > 1){
                metanames <- c(what_meta[a], paste0(what_meta[a], "_", 2:length(metadat)))
            } else {
                metanames <- what_meta[a]
            }
            for(b in 1:length(metanames)){
                tmp[metanames[b]] <- metadat[b]
            }
        }
    }
    inx <- stringr::str_detect(names(attributes(tmp)), "^takR")
    attributes(tmp)[inx] <- NULL
    return(tmp)
}
