find_sections <- function(raw, eof="EOF"){
    sections <- c("takiwaR Metadata", "Substrate (% Cover)", "Primary Producers (% Cover)", "Primary Producers (Counts)", "Creatures (% Cover)", "Creatures (Counts)", "Iris size frequency", "Australis size frequency")
    extras=NULL # Can add funcitonality for "extra" sections later
    # Will need to pass through the "type" (count, % cover etc...)
    if(!is.null(extras)){
        sections <- c(sections, extras)
        extra_names <- tolower(str_replace_all(extras, "\\s", ""))
    }
    # Search for the sections in the first column
    def_search <- c(sections, eof)
    def_search <- tolower(str_replace_all(def_search, "\\s", ""))
    firstcol <- tolower(str_replace_all(raw[,1], "\\s", ""))
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
        vals <- str_trim(vals)
        vals <- type.convert(vals, as.is=TRUE)
        if(length(vals) > 0){
            meta_out[[key]] <- vals  
        }
    }
    return(meta_out)
}

make_key <- function(string){
    key <- str_trim(string)
    key <- make.names(key)
    key <- str_replace_all(key, "\\.", "_")
    return(key)
}

bt_clean_data <- function(dat, quad_dir="col", n_quad = 10, quad_pfix = FALSE, data_pfix = FALSE, implicit_zeros = TRUE){
    # quad_dir="col" means quadrats along columns
    # quad_dir="row" means quadrats down rows
    quad_dir <- tolower(quad_dir)
    # First drop the first row (which is always a header row)
    dat <- dat[-1,]
    # Extract and drop the first column. 
    first_col <- dat[,1] # Extract data names from the first column
    first_col <- str_trim(first_col) # Strip whitespace
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
        dat <- t(dat) # Transpose "wide" data, so that quadrats are in rows
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

read_bt_raw <- function(file, use_logfile = FALSE){
    if(use_logfile){
        logfile <- paste0(tools::file_path_sans_ext(file), ".TAKlog")
        logfile <- file(logfile, open="wt")
        sink(file = logfile, type = "message")
    }
    message("Reading file '", file, "'")
    raw <- read_excel(file, col_names=FALSE)
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

## Methods
# bt_combine... Combine is for combining data from subquadrats into larger quadrats
# e.g. Low vis means divers are working small sections of a larger quadrat.
# The assumption is that there are equal numbers of (sub)quadrats in each dataset
bt_combine <- function(x1, x2, ...) UseMethod("bt_combine")

bt_combine.bt_raw <- function(raw1, raw2, ...){
    all_inp <- c(list(raw1), list(raw2), list(...))
    keys <- unique(unlist(lapply(all_inp, names)))
    # Merge each of the objects. Relies on Method dispatching
    out <- list()
    for(key in keys){
        key_dat <- lapply(all_inp, 'getElement', key)
        #print(str(key_dat))
        out[[key]] <- do.call(bt_combine, key_dat)
    }
    class(out) <- c("bt_raw", class(out))
    return(out)
}

bt_combine.bt_meta <- function(meta1, meta2, ...){
    all_inp <- c(list(meta1), list(meta2), list(...))
    # No need to remove empty sections... Meta sections should never be empty...
    keys <- unique(unlist(lapply(all_inp, names)))
    out_meta <- setNames(do.call(mapply, c(FUN=c, lapply(all_inp, '[', keys))), keys)
    # Handle some special cases...
    if('quad_size' %in% names(out_meta)){
        out_meta <- bt_set_meta(out_meta, list(
            "quad_size" = sum(out_meta[['quad_size']]))
        )
    }
    # Assuming all special cases return only 1 value, return just unique values
    out_meta <- lapply(out_meta, unique) # TODO make this nicer!
    out_meta <- bt_set_meta(out_meta, list("is_merged" = TRUE))
    class(out_meta) <- c("bt_meta", class(out_meta))
    return(out_meta)
}

bt_combine.bt_perc <- function(perc1, perc2, ...){
    all_inp <- c(list(perc1), list(perc2), list(...))
    original_length <- length(all_inp) # Needed for averaging, below
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='bt_empty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('bt_empty', 'bt_perc', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
#     for(a in 1:length(all_inp)){
#         if(inherits(all_inp[a], what = 'bt_empty')){
#             all_inp[a] <- NULL
#         }
#     }
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    # cbind all the data
    all_dat <- NULL
    for(a in 1:length(all_inp)){
        all_dat <- cbind(all_dat, all_inp[[a]])
    }
    # Convert NA's to zeros (OK for summing)
    all_dat[is.na(all_dat)] <- 0
    all_dat <- sapply(unique(colnames(all_dat)), function(x){
        rowSums(all_dat[, grep(x, colnames(all_dat)), drop=FALSE])}
    )
    #all_dat[all_dat==0] <- NA # Reconvert zeros to NA
    all_dat <- all_dat/original_length # Convert to a mean based on original input length
    class(all_dat) <- c("bt_perc", class(all_dat))
    return(all_dat)
}

bt_combine.bt_count <- function(count1, count2, ...){
    all_inp <- c(list(count1), list(count2), list(...))
    original_length <- length(all_inp) # Not really needed...
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='bt_empty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('bt_empty', 'bt_count', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    # cbind all the data
    all_dat <- NULL
    for(a in 1:length(all_inp)){
        all_dat <- cbind(all_dat, all_inp[[a]])
    }
    # Convert NA's to zeros (OK for summing)
    all_dat[is.na(all_dat)] <- 0
    all_dat <- sapply(unique(colnames(all_dat)), function(x){
        rowSums(all_dat[, grep(x, colnames(all_dat)), drop=FALSE])}
    )
    #all_dat[all_dat==0] <- NA # Reconvert zeros to NA
    class(all_dat) <- c("bt_count", class(all_dat))
    return(all_dat)
}

bt_combine.bt_sf <- function(sf1, sf2, ...){
    all_inp <- c(list(sf1), list(sf2), list(...))
    original_length <- length(all_inp) # Not really needed
    #Find out which objects are empty, if any.
    empty <- unlist(lapply(all_inp, inherits, what='bt_empty'))
    if(all(empty)){
        all_dat <- NA
        class(all_dat) <- c('bt_empty', 'bt_sf', class(all_dat))
        return(all_dat)
    }
    # Remove empty sections
    all_inp <- all_inp[!empty]
    rowcount <- unlist(lapply(all_inp, nrow))
    if(!compare(rowcount)){
        stop("All inputs must have the same number of rows (i.e. quadrats).")
    }
    all_dat <- do.call(cbind, all_inp)
    class(all_dat) <- c("bt_sf", class(all_dat))
    return(all_dat)
}

summary.bt_raw <- function(raw_dat, ...){
    out <- list('meta' = raw_dat[['meta']])
    keys <- names(raw_dat)
    keys <- keys[!keys=='meta']
    for(key in keys){
        out[[key]] <- summary(raw_dat[[key]])
    }
    class(out) <- c("bt_summary", class(out))
    return(out)
}

summary.bt_perc <- function(perc1, ...){
    if(inherits(perc1, what = "bt_empty")){
        out <- NA
        class(out) <- c("bt_empty", class(out))
        return(out)
    }
    dat_mean <- apply(perc1, 2, mean)
    dat_median <- apply(perc1, 2, median)
    dat_n <- apply(perc1, 2, length)
    dat_sd <- apply(perc1, 2, sd)
    dat_se <- dat_sd/sqrt(dat_n)
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    return(out)
}

summary.bt_count <- function(count1, ...){
    if(inherits(count1, what = "bt_empty")){
        out <- NA
        class(out) <- c("bt_empty", class(out))
        return(out)
    }
    dat_mean <- apply(count1, 2, mean)
    dat_median <- apply(count1, 2, median)
    dat_n <- apply(count1, 2, length)
    dat_sd <- apply(count1, 2, sd)
    dat_se <- dat_sd/sqrt(dat_n)
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    return(out)
}

summary.bt_sf <- function(sf1, ...){
    if(inherits(sf1, what = "bt_empty")){
        out <- NA
        class(out) <- c("bt_empty", class(out))
        return(out)
    }
    dat_mean <- mean(sf1, na.rm=T)
    dat_median <- median(sf1, na.rm=T)
    dat_n <- length2(sf1, na.rm=T)
    dat_sd <- sd(sf1, na.rm=T)
    dat_se <- dat_sd/sqrt(dat_n)
    out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
    rownames(out) <- c("All")
    return(out)
}

print.bt_empty <- function(x, ...){
    cat("< No Data >\n")
}

print.bt_meta <- function(x, ...){
    keys <- names(x)
    pad_len <- max(str_length(keys))+3
    for(key in keys){
        cat(str_pad(paste0(key, " : "), pad_len, side="left"), 
            paste0(x[[key]], collapse = ", "), "\n", sep="")
    }
}

print.bt_summary <- function(x, ...){
    keys <- names(x)
    #pad_len <- max(str_length(keys))+3
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=str_length(key)), "\n", sep="")
        print(x[[key]], ...)
    }
}

print.bt_raw <- function(x, ...){
    keys <- names(x)
    #pad_len <- max(str_length(keys))+3
    for(key in keys){
        cat("\n", key, "\n", sep="")
        cat(rep("-", times=str_length(key)), "\n", sep="")
        print(x[[key]], ...)
    }
}

print.bt_perc <- function(x, ...){
    print.table(x, na.print = "NA", ...)
}

print.bt_count <- function(x, ...){
    print.table(x, na.print = "NA", ...)
}

print.bt_sf <- function(x, ...){
    print.table(x, na.print = "", ...)
}

## Long generates a "long form" dataframe, columns: Quadrat, Variable, Value and one each for meta data
bt_long <- function(x1, ...) UseMethod("bt_long")

bt_long.bt_meta <- function(meta, nrow=1, ...){
    keys <- names(meta)
    out  <- list()
    # Sort out duplicate keys
    for(key in keys){
        vals <- meta[[key]]
        if(length(vals) > 1){ key <- paste0(key, "_", 1:length(vals))} # TODO unify "make keys" in takiwaR
        for(a in 1:length(vals)){
            out[[key[a]]] <- rep(vals[a], times=nrow)
        }
    }
    out <- as.data.frame(out)
    return(out)
}

bt_long.bt_perc <- function(perc1, meta=list(), ...){
    out <- melt(perc1)
    names(out) <- c("bt_quad", "bt_var", "bt_val")
    return(out)
}

bt_long.bt_count <- function(count1, meta=list(), ...){
    out <- melt(count1)
    names(out) <- c("bt_quad", "bt_var", "bt_val")
    if(length(meta) > 0){
        out <- cbind(out, bt_long(meta, ncol=nrow(out)))
    }
    out <- as.data.frame(out)
    return(out)
}

bt_long.bt_sf <- function(sf1, meta=list(), ...){
    out <- melt(sf1, na.rm = TRUE)
    out <- out[,-2]
    names(out) <- c("bt_quad", "bt_val")
    return(out)
}

plot.bt_sf <- function(x, main = NULL, xlab = "Size", ylab = "Frequency", las = 1, ...){
    dat <- x[!is.na(x)]
    old_par_mar <- par()$mar
    par(mar=c(5,4,1,1))
    hist(dat, main = NULL, xlab="Size", ylab="Frequency", las = 1, ...)
    par(mar=old_par_mar)
}

plot.bt_perc <- function(x, ...){
    xval <- rep(1:ncol(x), each=nrow(x))
    yval <- as.vector(x)
    labs <- colnames(x)
    xmar <- max(str_length(labs))/2
    old_par_mar <- par()$mar
    par(mar=c(xmar,4,1,1))
    plot(xval, yval, ann=F, axes=F, pch=19, col=rgb(0,0,0,0.2), ...)
    box()
    axis(1, lwd=0, lwd.ticks=1, labels = labs, at=1:ncol(x), las=2)
    axis(2, lwd=0, lwd.ticks=1, las=1)
    mtext(text = "% Cover", side = 2, line=2.5)
    par(mar=old_par_mar)
}

plot.bt_count <- function(x, ...){
    xval <- rep(1:ncol(x), each=nrow(x))
    yval <- as.vector(x)
    labs <- colnames(x)
    xmar <- max(str_length(labs))/2
    old_par_mar <- par()$mar
    par(mar=c(xmar,4,1,1))
    plot(xval, yval, ann=F, axes=F, pch=19, col=rgb(0,0,0,0.2), ...)
    box()
    axis(1, lwd=0, lwd.ticks=1, labels = labs, at=1:ncol(x), las=2)
    axis(2, lwd=0, lwd.ticks=1, las=1)
    mtext(text = "Number", side = 2, line=2.5)
    par(mar=old_par_mar)
}

compare <- function(v){
    all(sapply(as.list(v[-1]), FUN=function(z){
        identical(z, v[1])
        }))
}