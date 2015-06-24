read_bt_raw <- function(file, use_logfile = FALSE){
    if(use_logfile){
        logfile <- paste0(tools::file_path_sans_ext(file), ".TAKlog")
        logfile <- file(logfile, open="wt")
        sink(file = logfile, type = "message")
    }
    message("Reading file '", file, "'")
    raw <- readxl::read_excel(file, col_names=FALSE)
    message("Finding sections...")
    sections <- find_sections(raw)
    out <- list() # Prepare an "out" object
    # Get start and end points 
    section_names <- sections[,'names']
    start <- sections[,'start']
    end <- sections[,'end']
    # Now loop over these
    for(a in 1:nrow(sections)){
        message("Extracting ", sections[a,"sections"], " as '", section_names[a], "'.")
        out[[section_names[a]]] <- raw[start[a]:end[a], ]
    }
    
    # Smash out the metadata cleaning now
    message("Processing metadata...")
    out[['meta']] <- bt_meta_clean(out[['meta']])
    # Add the "file_name"  metadata
    out[['meta']] <- bt_add_meta(out[['meta']], add=list("file_name" = file))
    # Need to validate meta here. Or reincorperate validation in cleaning...
    out[['meta']] <- bt_validate_meta(out[['meta']])
    # Extract n_quad, needed to process data...
    n_quad <- out[['meta']][['n_quad']]
    
    # Process data.
    # First % Cover. These are in the "quadrats as columns" direction
    message("Processing '% cover' datatypes...")
    perc_dat_types <- c("substrate", "prim_prod_p", "creat_p")
    for(dat_type in perc_dat_types){
        message("Processing section '", dat_type, "'...")
        clean_data <- bt_clean_data(out[[dat_type]], quad_dir="col", "n_quad" = n_quad, quad_pfix = "Q") 
        message("Validating data in section '", dat_type, "'...")
        clean_data <- bt_validate_perc(clean_data)
        out[[dat_type]] <- clean_data
    }
    
    # Then count data. These are also in the "quadrats as columns" direction
    message("Processing 'count' datatypes...")
    count_dat_types <- c("prim_prod_c", "creat_c")
    for(dat_type in count_dat_types){
        message("Processing section '", dat_type, "'...")
        clean_data <- bt_clean_data(out[[dat_type]], quad_dir="col", "n_quad" = n_quad, quad_pfix = "Q")
        message("Validating data in section '", dat_type, "'...")
        clean_data <- bt_validate_count(clean_data)
        out[[dat_type]] <- clean_data
    }
    
    # Then size frequency data. These are in the "quadrats as rows" direction.  
    message("Processing 'size frequency' datatypes...")
    sf_dat_types <- c("iris_sf", "australis_sf")
    for(dat_type in sf_dat_types){
        message("Processing section '", dat_type, "'...")
        clean_data <- bt_clean_data(out[[dat_type]], quad_dir="row", n_quad = n_quad, quad_pfix = "Q", implicit_zeros = FALSE)
        message("Validating data in section '", dat_type, "'...")
        minsize <- 10
        maxsize <- ifelse(dat_type == "iris_sf", 300, 150)
        clean_data <- bt_validate_sf(clean_data, minsize, maxsize)
        out[[dat_type]] <- clean_data
    }
    
    # Close the logfile
    if(use_logfile){
        sink(type="message")
        close(logfile)
    }
    # Return this
    class(out) <- c("bt_raw", class(out))
    return(out)
}

find_sections <- function(raw, eof="EOF", extras=NULL){
    sections <- c("takiwaR Metadata", "Substrate (% Cover)", "Primary Producers (% Cover)", "Primary Producers (Counts)", "Creatures (% Cover)", "Creatures (Counts)", "Iris size frequency", "Australis size frequency")
    # Will need to pass through the "type" (count, % cover etc...)
    if(!is.null(extras)){
        sections <- c(sections, extras)
        extra_names <- tolower(stringr::str_replace_all(extras, "\\s", ""))
    }
    # Search for the sections in the first column
    def_search <- c(sections, eof)
    def_search <- tolower(stringr::str_replace_all(def_search, "\\s", ""))
    firstcol <- tolower(stringr::str_replace_all(raw[,1], "\\s", ""))
    matches <- match(def_search, firstcol)
    m_length <- length(matches)
    # Name the sections
    section_names <- c("meta", "substrate", "prim_prod_p", "prim_prod_c", "creat_p", "creat_c", "iris_sf", "australis_sf")
    if(!is.null(extras)){
        section_names <- c(section_names, extra_names)
    }
    section_names <- c(section_names)
    out <- cbind(sections, "names" = section_names, "start" = matches[-m_length], "end" = matches[-1]-1)
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    return(out)    
}

bt_meta_clean <- function(meta){
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

bt_clean_data <- function(dat, quad_dir="col", n_quad = 10, quad_pfix = FALSE, data_pfix = FALSE, implicit_zeros = TRUE){
    # quad_dir="col" means quadrats along columns
    # quad_dir="row" means quadrats down rows
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
                warning(paste0("data exist in columns that were excluded using n_quad = ", n_quad, "."), immediate. = TRUE, call. = FALSE)
            }
            dat <- dat[,-dropcols, drop=FALSE]
        }
        droprows <- apply(dat, 1, all_is_na) # Get rows that have nothing
        dat <- dat[!droprows, , drop=FALSE] # Drop empty rows
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
                warning(paste0("data exist in rows that were excluded using n_quad = ", n_quad, "."), immediate. = TRUE, call. = FALSE)
            }
            dat <- dat[-droprows, , drop=FALSE]
            first_col <- first_col[!droprows] # Update the first column info
        }
        dropcols <- apply(dat, 2, all_is_na) # Get columns that have nothing
        dat <- dat[,!dropcols, drop=FALSE] # Drop empty columns
        # Coerce what remains into a numeric matrix
        dat <- apply(dat, 2, as.numeric)
        if(implicit_zeros){
            # Replace NA's with zeros
            dat[is.na(dat)] <- 0
        }
    }
    
    ## If there are no columns at this point, we can just return NA
    if(is.null(ncol(dat))){
        return(NA)
    }
    
    ## Name rows (quadrats)
    if(nrow(dat) > 0){
        if(is.character(quad_pfix)){
            rownames(dat) <- paste0(quad_pfix, sprintf("%02d", 1:nrow(dat)))  
        } else {
            rownames(dat) <- NULL
        }
    }
    
    ## Name columns (data)
    if(ncol(dat) > 0){
        if(is.character(data_pfix)){
            colnames(dat) <- paste0(data_pfix, sprintf("%02d", 1:ncol(dat)))
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
    } else {
        out <- NA
    }
    return(out)
}

## Potential methods for bt_raw
bt_add_meta <- function(meta, add=list()){
    if(!is.list(add)){
        stop("'add' must be a list list.")
    }
    if(length(add) < 1){
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

bt_set_meta <- function(meta, set=list()){
    if(!is.list(set)){
        stop("'set' must be a list list.")
    }
    if(length(set) < 1){
        stop("'set' cannot be an empty list.")
    }
    for(name in names(set)){
        key <- make_key(name)
        meta[[key]] <- set[[name]]
    }
    return(meta)
}

bt_validate_perc <- function(dat){
    if(all(is.na(dat))){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        class(dat) <- c("bt_empty", "bt_perc", class(dat))
        return(dat)
    }
    if(any(is.na(dat))){
        # Has NA's
        warning("This section has NAs. This shouldn't be possible: Implicit zeros are assumed.", immediate. = TRUE, call. = FALSE)
    }
    ## Tests for percent cover data
    toohigh <- which(dat > 100, arr.ind=T)
    # Test that no (non NA) value is > 100
    if(nrow(toohigh) > 0){
        warning("At least one value has been entered with > 100% cover. Are you sure?", immediate. = TRUE, call. = FALSE)
    }
    class(dat) <- c("bt_perc", class(dat))
    return(dat)
}

bt_validate_count <- function(dat){
    if(all(is.na(dat))){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        class(dat) <- c("bt_empty", "bt_count", class(dat))
        return(dat)
    }
    if(any(is.na(dat))){
        # Has NA's
        warning("This section has NAs. This shouldn't be possible: Implicit zeros are assumed.", immediate. = TRUE, call. = FALSE)
    }
    ## Tests for count data
    ## Currently we have none?
    # SHould test for integers?
    class(dat) <- c("bt_count", class(dat))
    return(dat)
}

bt_validate_sf <- function(dat, minsize, maxsize){
    if(all(is.na(dat))){
        # Empty section
        warning('No data in this section.', immediate. = TRUE, call. = FALSE)
        class(dat) <- c("bt_empty", "bt_sf", class(dat))
        return(dat)
    }
    toosmall <- which(dat < minsize, arr.ind=TRUE)
    if(nrow(toosmall) > 0){
        warning("At least one creature is less than ", minsize, " mm. Are you sure?", immediate. = TRUE, call. = FALSE)
    }
    toobig <- which(dat > maxsize, arr.ind=TRUE)
    if(nrow(toobig) > 0){
        warning("At least one creature is greater than ", maxsize, " mm. Are you sure?", immediate. = TRUE, call. = FALSE)
    }
    class(dat) <- c("bt_sf", class(dat))
    return(dat)
}

bt_validate_meta <- function(meta){
    # Deal with required values
    req_vals <- c("site", "date", "depth", "n_quad", "quad_size", "gps_lat", "gps_long")
    val_locs <- which(req_vals %in% names(meta))
    if(length(val_locs) == 0){
        missing <- req_vals
    } else {
        missing <- req_vals[-val_locs]
    }
    if(length(missing) > 0){
        stop("The following required values are missing from the metadata: ", paste(missing, collapse=", "), call. = FALSE)
    }
    #Deal with reccomended values
    if(!'collected_by' %in% names(meta)){
        warning("The 'collected_by' field is missing from the metadata. It is recommended.", immediate. = TRUE, call. = FALSE)
    }
    if(!'entered_by' %in% names(meta)){
        warning("The 'entered_by' field is missing from the metadata. It is recommended.", immediate. = TRUE, call. = FALSE)
    }
    # Date parsing? etc.
    class(meta) <- c("bt_meta", class(meta))
    return(meta)
}

compare <- function(v){
    all(sapply(as.list(v[-1]), FUN=function(z){
        identical(z, v[1])
        }))
}