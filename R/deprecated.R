# error_bar <- function(x, y, upper, lower=upper, length=0.05, dir='y', ...){
#     if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
#         stop("Vectors must be same length")
#     if(all(is.factor(x))){
#         x <- as.numeric(x)
#     }
#     if(dir=='y'){
#         suppressWarnings(arrows(x,y+upper,x,y-lower, angle=90, code=3, length=length, ...))
#     } else if(dir=='x') {
#         suppressWarnings(arrows(x-lower,y,x+upper,y, angle=90, code=3, length=length, ...))
#     } else {
#         stop('Impossible')
#     }
# }

# mtexti <- function(text, side, off = 0.25, srt = NULL, ...) {
#     if(is.null(srt)){
#         srt <- if(side == 2) 90 else 
#             if(side == 4) 270 else 
#                 0
#     }
#     # dimensions of plotting region in user units
#     usr <- par('usr')
#     # dimensions of plotting region in inches
#     pin <- par('pin')
#     # user units per inch
#     upi <- c(usr[2]-usr[1],
#              usr[4]-usr[3]) / pin
#     # default x and y positions
#     xpos <- (usr[1] + usr[2])/2
#     ypos <- (usr[3] + usr[4])/2
#     if(1 == side)
#         ypos <- usr[3] - upi[2] * off
#     if(2 == side)
#         xpos <- usr[1] - upi[1] * off
#     if(3 == side)
#         ypos <- usr[4] + upi[2] * off
#     if(4 == side)
#         xpos <- usr[2] + upi[1] * off
#     text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
# }

# set_par <- function(brewer.n=8, brewer.name="Dark2", 
#                     cex.lab=1, cex.main=1.2, cex.axis=1, 
#                     mar=c(2.5,2.5,1.6,1.1), mgp=c(1.5,.5,0), las=1, ...){
#     par(mar=mar, mgp=mgp, cex.lab=cex.lab, cex.main=cex.main, cex.axis=cex.axis, las=las)
#     par(...)
#     pal <- c("#000000", RColorBrewer::brewer.pal(brewer.n, brewer.name))
#     palette(pal)
# }

# setup_plot <- function(x, y, yerr = NA, xlab = "", ylab = "", ...) {
#     xvals <- c(min(x, na.rm = T), max(x, na.rm = T))
#     yvals <- c(min(y, na.rm = T), max(y, na.rm = T))
#     if(!all(is.na(yerr))){
#         yvals[1] <- yvals[1]-yerr
#         yvals[2] <- yvals[2]+yerr
#     } 
#     plot(xvals, yvals, axes=F, type="n", xlab=xlab, ylab=ylab, ...)
#     box()
#     axis(1, lwd = 0, lwd.ticks = 1)
#     axis(2, lwd = 0, lwd.ticks = 1)
# }

# read_takRbt <- function(file, use_logfile = FALSE, sections = default_takRdef("takRbt")){
#     ## Close the logfile on exit, even (perhaps, especially) if there is an error.  
#     on.exit(expr = {
#         if(use_logfile){
#             sink(type="message")
#             close(logfile)
#         }
#     })
#     
#     if(use_logfile){
#         logfile <- paste0(tools::file_path_sans_ext(file), ".TAKlog")
#         logfile <- file(logfile, open="wt")
#         sink(file = logfile, type = "message")
#     }
#     message("Reading file '", file, "'")
#     
#     ## Sorting out readxl's guessing of column numbers
#     # Also forcing all data to be read as "text". We will coerce it later.  
#     ext <- tools::file_ext(file)
#     if(ext == "xlsx"){
#         col_types <- rep("text", readxl:::xlsx_dim(path = file)[2])
#         col_names <- paste0("col", 1:length(col_types))
#     } else if(ext =="xls"){
#         warning("Old Excel file format, assuming less than 1000 rows.", 
#                 immediate. = TRUE, call. = FALSE)
#         col_types <- readxl:::xls_col_types(path = file, n = 1000L)
#         col_types <- rep("text", length(col_types))
#         col_names <- paste0("col", 1:length(col_types))
#     } else {
#         stop("Only excel files are supported. I know. Sorry.")
#     }
#     
#     raw <- readxl::read_excel(file, col_names = col_names, col_types = col_types)
#     raw <- data.frame(raw) # Fucking datatable bullshit
#     
#     # Now, finding sections
#     message("Finding sections...")
#     sections <- find_takRdef(raw, sections)
#     
#     # Get ready for the output
#     out <- list() # Prepare an "out" object
#     
#     # Get names, start and end points 
#     section_names <- names(sections)
#     section_text <- sapply(sections, "[[", "text")
#     start <- sapply(sections, "[[", "start")
#     end <- sapply(sections, "[[", "end")
#     
#     # Now loop over these
#     for(a in 1:length(section_names)){
#         message("Extracting ", section_text[a], " as '", section_names[a], "'.")
#         out[[section_names[a]]] <- raw[start[a]:end[a], ]
#     }
#     
#     ## Metadata...
#     message("Processing meta...")
#     out[['meta']] <- clean_takRmeta(out[['meta']])
#     # Add the "file_name" to the metadata
#     out[['meta']] <- add_takRmeta(out[['meta']], add=list("file_name" = file))
#     # Add the class
#     class(out[['meta']]) <- c("takRmeta", class(out[['meta']]))
#     # Validate metadata
#     message("Validating meta...")
#     out[['meta']] <- takRvalidate(out[['meta']], req_vals = sections$meta$required)
#     # Extract n_quad, needed to process data...
#     n_quad <- out[['meta']][['n_quad']]
#     
#     # Process Data...
#     data_sections <- sections[!sapply(sections, function(x){x$class=="takRmeta"})]
#     
#     for(a in 1:length(data_sections)){
#         ss <- data_sections[[a]] # ss = Single Section
#         ss_name <- names(data_sections[a]) # Names of these single sections
#         iz <- ifelse(ss[["class"]]=="takRsf", FALSE, TRUE) # Implicit zeros for everything except size-frequency
#         
#         message("Processing section '", ss_name, "'...")
#         cd <- clean_takRdata(out[[ss_name]], quad_dir = ss[["quad_dir"]], 
#                              n_quad = n_quad, implicit_zeros = iz)
#         # Add the class 
#         class(cd) <- c(ss[["class"]], class(cd))
#         # Add optional section data attributes
#         opt_names <- names(vals_takRdef(show=FALSE)$opt)
#         for(a in 1:length(opt_names)){
#             attr(cd, opt_names[a]) <- ss[[opt_names[a]]]
#         }
#         # Add meta data as data attributes (for portability)
#         cd <- toattr_takRmeta(cd, meta=out[["meta"]])
#         
#         # Validate the data    
#         message("Validating data in section '", ss_name, "'...")
#         vd <- takRvalidate(cd)
#         out[[ss_name]] <- vd
#     }
#     
#     # Return this
#     class(out) <- c("takRbt", class(out))
#     return(out)
# }

# vals_takRdef <- function(show=TRUE){
#     req <- list(text="The text to search for",
#                 class="The takiwaR class of the section",
#                 quad_dir="Quadrat direction. Either 'col' or 'row'. Not required for 'meta'")
#     opt <- list(takRdef_range="The maximum and minimum that the values in this section can take", 
#                 takRdef_units="The units of values in this section")
#     if(show){
#         #cat("Supported attributes in takiwaR sections\n")
#         #cat("----------------------------------------\n")
#         max_pad <- max(stringr::str_length(c(names(req), names(opt))))
#         cat("Required:\n")
#         for(a in 1:length(req)){
#             cat(stringr::str_pad(names(req[a]), max_pad), ": ", req[[a]], ".\n", sep="")
#         }
#         cat("\nOptional:\n")
#         for(a in 1:length(opt)){
#             cat(stringr::str_pad(names(opt[a]), max_pad), ": ", opt[[a]], ".\n", sep="")
#         }
#     }
#     invisible(list(req=req, opt=opt))
# }

# default_takRdef <- function(type = NULL){
#     sections <- list()
#     class(sections) <- c("takRdef", class(sections))
#     if(is.null(type)){
#         return(sections)
#     } else if(type == "takRbt"){
#         sections[["meta"]] <- list(text="takiwaR Metadata", class="takRmeta", 
#                                    required = c("site","date","depth","n_quad","quad_size","gps_lat","gps_long"))
#         sections[["substrate"]] <- list(text="Substrate (% Cover)", class="takRperc", quad_dir="col")
#         sections[["prim_prod_p"]] <- list(text="Primary Producers (% Cover)", class="takRperc", quad_dir="col")
#         sections[["prim_prod_c"]] <- list(text="Primary Producers (Counts)", class="takRcount", quad_dir="col")
#         sections[["creat_p"]] <- list(text="Creatures (% Cover)", class="takRperc", quad_dir="col")
#         sections[["creat_c"]] <- list(text="Creatures (Counts)", class="takRcount", quad_dir="col")
#         sections[["iris_sf"]] <- list(text="Iris size frequency", class="takRsf", quad_dir="row", 
#                                       takRdef_range=c(10,300), takRdef_units="mm")
#         sections[["australis_sf"]] <- list(text="Australis size frequency", class="takRsf", quad_dir="row", 
#                                            takRdef_range=c(10,150), takRdef_units="mm")
#         sections[["chloroticus_sf"]] <- list(text="Chloroticus size frequency", class="takRsf", quad_dir="row", 
#                                              takRdef_range=c(10,500), takRdef_units="mm")
#         return(sections)
#     } else {
#         stop("There is no default for that type.")
#     }
# }

# find_takRdef <- function(raw, sections, eof="EOF"){
# # Search for the sections in the first column
# sec_text <- sapply(sections, FUN = "[[", 1)
# sec_name <- names(sections)
# def_search <- tolower(stringr::str_replace_all(sec_text, "\\s", ""))
# eof_search <- tolower(stringr::str_replace_all(eof, "\\s", ""))
# firstcol <- tolower(stringr::str_replace_all(raw[,1], "\\s", ""))
# 
# sec_matches <- match(def_search, firstcol)
# eof_match <- match(eof_search, firstcol)
# 
# ## Check eof is matched
# if(is.na(eof_match)){
#     stop(paste0("End of file marker (", eof, ") not found."))
# }
# 
# if(any(is.na(sec_matches))){
#     missing <- sec_text[is.na(sec_matches)]
#     message <- paste("The following section, or sections, were not found: \n", paste(missing, collapse = ", "))
#     warning(message, immediate. = TRUE, call. = FALSE)
#     # Remove sections and section start values
#     sections <- sections[!is.na(sec_matches)]
#     sec_matches <- sec_matches[!is.na(sec_matches)]
# }
# 
# if(any(sec_matches > eof_match)){
#     willexclude <- sec_text[sec_matches > eof_match]
#     message <- paste("The following section, or sections, are found after the EOF and will be excluded: \n", 
#                      paste(willexclude, collapse = ", "))
#     warning(message, immediate. = TRUE, call. = FALSE)
#     # Remove sections and section start values
#     sections <- sections[!sec_matches > eof_match]
#     sec_matches <- sec_matches[!sec_matches > eof_match]
# }
# 
# # Combine section start values into sections...
# for(a in 1:length(sections)){
#     sections[[a]]['start'] <- sec_matches[a]
# }
# 
# # Sort by start values
# sections <- sections[order(sapply(sections, '[[', 'start'))]
# 
# # Sort out end values
# secminlast <- length(sections)-1
# # Use the next sections start value minus 1
# for(a in 1:secminlast){
#     sections[[a]]['end'] <- sections[[a+1]][['start']]-1
# }
# # Except for the last section, which will be EOF minus 1
# sections[[length(sections)]]['end'] <- eof_match-1
# 
# class(sections) <- c("takRdef", class(sections))
# return(sections) 
# }

# clean_takRmeta <- function(meta){
#     meta <- meta[-1,] # Drop row 1
#     emptyrows <- apply(meta, 1, all_is_na) # Detect totally empty (NA only) rows
#     meta <- meta[!emptyrows,] # Remove empty rows
#     meta_out <- list()
#     for(a in 1:nrow(meta)){
#         key <- make_key(meta[a,1])
#         vals <- meta[a,-1]
#         vals <- vals[!is.na(vals)]
#         vals <- as.character(vals)
#         vals <- stringr::str_trim(vals)
#         vals <- type.convert(vals, as.is=TRUE)
#         if(length(vals) > 0){
#             meta_out[[key]] <- vals  
#         }
#     }
#     return(meta_out)
# }

# clean_takRdata <- function(dat, quad_dir, n_quad, data_pfix = FALSE, implicit_zeros = TRUE){
#     # quad_dir="col" means quadrats along columns
#     # quad_dir="row" means quadrats down rows
#     
#     # Returns an object of class 'takRwide' (or 'takRempty')
#     
#     quad_dir <- tolower(quad_dir)
#     # First drop the first row (which is always a header row)
#     dat <- dat[-1,]
#     # Extract and drop the first column. 
#     first_col <- dat[,1] # Extract data names from the first column
#     first_col <- stringr::str_trim(first_col) # Strip whitespace
#     dat <- dat[,-1] # Drop it
#     
#     # Next check there is at least enough columns to proceed
#     if(quad_dir == "col" & ncol(dat) < n_quad){
#         stop(paste("There aren't", n_quad, "columns in the supplied data."))
#     }
#     if(quad_dir == "row" & nrow(dat) < n_quad){
#         stop(paste("There aren't", n_quad, "rows in the supplied data."))
#     }
#     
#     # Deal with the data in "quadrats in columns" direction.
#     if(quad_dir == "col"){
#         if(ncol(dat) > n_quad){
#             dropcols <- seq(from = n_quad+1, to = ncol(dat), by = 1)
#             droped <- dat[,dropcols]
#             if(!all_is_na(droped)){
#                 warning(paste0("Data exist in columns that were excluded using n_quad = ", n_quad, "."), immediate. = TRUE, call. = FALSE)
#             }
#             dat <- dat[,-dropcols, drop=FALSE]
#         }
#         droprows <- apply(dat, 1, all_is_na) # Get rows that have nothing
#         dat <- dat[!droprows, , drop=FALSE] # Drop empty rows
#         ## If there are no rows at this point, we can just return NA
#         if(is_zero(nrow(dat))){
#             out <- NA
#             class(out) <- c("takRempty", class(out))
#             return(out)
#         }
#         
#         first_col <- first_col[!droprows] # Update the first column info
#         # Coerce what remains into a numeric matrix
#         dat <- apply(dat, 2, as.numeric)
#         if(implicit_zeros){
#             # Replace NA's with zeros
#             dat[is.na(dat)] <- 0
#         }
#         if(is.matrix(dat)){
#             dat <- t(dat) # Transpose "wide" data, so that quadrats are in rows
#         } else {
#             dat <- as.matrix(dat)
#         }
#     }
#     
#     # Deal with the data in "quadrats in rows" direction.
#     if(quad_dir == "row"){
#         if(nrow(dat) > n_quad){
#             droprows <- seq(from = n_quad+1, to = nrow(dat), by = 1)
#             droped <- dat[droprows,]
#             if(!all_is_na(droped)){
#                 warning(paste0("Data exist in rows that were excluded using n_quad = ", n_quad, "."), 
#                         immediate. = TRUE, call. = FALSE)
#             }
#             dat <- dat[-droprows, , drop=FALSE]
#             first_col <- first_col[!droprows] # Update the first column info
#         }
#         dropcols <- apply(dat, 2, all_is_na) # Get columns that have nothing
#         dat <- dat[,!dropcols, drop=FALSE] # Drop empty columns
#         ## If there are no columns at this point, we can just return NA
#         if(is_zero(ncol(dat))){
#             out <- NA
#             class(out) <- c("takRempty", class(out))
#             return(out)
#         }
#         
#         # Coerce what remains into a numeric matrix
#         dat <- apply(dat, 2, as.numeric)
#         if(implicit_zeros){
#             # Replace NA's with zeros
#             dat[is.na(dat)] <- 0
#         }
#     }
#     
#     ## Name rows (quadrats)
#     # Always named 001:n
#     rownames(dat) <- sprintf("%03d", 1:nrow(dat))  
#     
#     ## TODO: Document this. Enforce this in all objects of 'takRwide'
#     
#     ## Name columns (data)
#     if(ncol(dat) > 0){
#         if(is.character(data_pfix)){
#             colnames(dat) <- paste0(data_pfix, sprintf("%01d", 1:ncol(dat)))
#         } else if (!all_is_na(first_col) & quad_dir == "col") {
#             colnames(dat) <- first_col
#         } else {
#             colnames(dat) <- NULL
#         }
#     }
#     
#     ## Construct the return object
#     out <- NULL
#     if(ncol(dat) > 0){
#         out <- dat # Data is an object with nquad rows
#         class(out) <- c("takRwide", class(out))
#     } else {
#         # This might be unneccesary... See above...
#         out <- NA
#         class(out) <- c("takRempty", class(out))
#         return(out)
#     }
#     return(out)
# }

## Potential methods? Not yet... Only objects are only classed after validation
# add_takRmeta <- function(meta, add=list()){
#     if(!is.list(add) | is_zero(length(names(add)))){
#         stop("'add' must be a named list")
#     }
#     if(is_zero(length(add))){
#         stop("'add' cannot be an empty list.")
#     }
#     for(name in names(add)){
#         key <- make_key(name)
#         if(key %in% names(meta)){
#             meta[[key]] <- c(meta[[key]], add[[name]])
#         } else {
#             meta[[key]] <- add[[name]]
#         }
#     }
#     return(meta)
# }

# set_takRmeta <- function(meta, set=list()){
#     if(!is.list(set) | is_zero(length(names(meta)))){
#         stop("'set' must be a named list")
#     }
#     if(is_zero(length(set))){
#         stop("'set' cannot be an empty list.")
#     }
#     for(name in names(set)){
#         key <- make_key(name)
#         meta[[key]] <- set[[name]]
#     }
#     return(meta)
# }

# extract_takRmeta <- function(x1, ...){
#     # Extract meta data from attributes in a table...
#     # Returns a list of class 'takRmeta'
#     all_inp <- c(list(x1), list(...))
#     for(a in 1:length(all_inp)){
#         dat <- all_inp[[a]]
#         temp_out <- list()
#         indx <- stringr::str_detect(names(attributes(dat)), "^(takRmeta_|takRdef_)")
#         if(any(indx)){
#             attr_names <- names(attributes(dat))[indx]
#             #print(attr_names)
#             for(b in 1:length(attr_names)){
#                 meta_name <- stringr::str_replace(attr_names[b], "^(takRmeta_|takRdef_)", "")
#                 temp_out[[meta_name]] <- attr(dat, attr_names[b])
#             }
#         }
#         if(a == 1){
#             ext_out <- temp_out
#             class(ext_out) <- c("takRmeta", class(ext_out))
#         } else {
#             ext_out <- takRcombine(ext_out, temp_out)
#         }
#     }
#     return(ext_out)
# }

# toattr_takRmeta <- function(obj, meta=list()){
#     # Take an item of takRmeta and add it to the object's attributes...
#     # Returns the object with the attributes added.  
#     if(!inherits(meta, "takRmeta")){
#         stop("'meta' must be of class 'takRmeta'")
#     }
#     meta_names <- names(meta)
#     for(a in 1:length(meta_names)){
#         atr_name <- paste0("takRmeta_", meta_names[a])
#         attr(obj, atr_name) <- meta[[meta_names[a]]]
#     }
#     return(obj)
# }

# extract_long <- function(x, what, what_meta=NULL){
#     # Extract a named item as a long-form data frame.
#     # Optionally include meta data in columns.  
#     if(is.null(x[[what]])){
#         return(NULL)
#     }
#     if(all(is.na(x[[what]]))){
#         return(NULL)
#     }
#     tmp <- takRlong(x[[what]])
#     if(!is.null(what_meta)){
#         for(a in 1:length(what_meta)){
#             metadat <- attr(tmp, paste0("takRmeta_", what_meta[a]))
#             if(length(metadat) > 1){
#                 metanames <- c(what_meta[a], paste0(what_meta[a], "_", 2:length(metadat)))
#             } else {
#                 metanames <- what_meta[a]
#             }
#             for(b in 1:length(metanames)){
#                 tmp[metanames[b]] <- metadat[b]
#             }
#         }
#     }
#     inx <- stringr::str_detect(names(attributes(tmp)), "^takR")
#     attributes(tmp)[inx] <- NULL
#     return(tmp)
# }


## The original takRcombine
# takRcombine... Combine is for combining data from subquadrats into larger quadrats
# e.g. Low vis means divers are working small sections of a larger quadrat.
# The assumption is that there are equal numbers of (sub)quadrats in each dataset
# takRcombine <- function(x1, x2, ...) UseMethod("takRcombine")

# takRcombine.takRbt <- function(x1, x2, ...){
#     all_inp <- c(list(x1), list(x2), list(...))
#     keys <- unique(unlist(lapply(all_inp, names)))
#     # Merge each of the objects. Relies on Method dispatching
#     out <- list()
#     for(key in keys){
#         key_dat <- lapply(all_inp, 'getElement', key)
#         #print(str(key_dat))
#         out[[key]] <- do.call(takRcombine, key_dat) # TODO - Tidy this up? Use UseMethod?
#     }
#     class(out) <- c("takRbt", class(out))
#     return(out)
# }

# takRcombine.takRmeta <- function(x1, x2, ...){
#     all_inp <- c(list(x1), list(x2), list(...))
#     # No need to remove empty sections... Meta sections should never be empty...
#     keys <- unique(unlist(lapply(all_inp, names)))
#     out_meta <- setNames(do.call(mapply, c(FUN=c, lapply(all_inp, '[', keys))), keys)
#     # Handle some special cases...
#     if('quad_size' %in% names(out_meta)){
#         out_meta <- set_takRmeta(out_meta, list(
#             "quad_size" = sum(out_meta[['quad_size']]))
#         )
#     }
#     # Assuming all special cases return only 1 value, return just unique values
#     out_meta <- lapply(out_meta, unique) # TODO make this nicer!
#     out_meta <- set_takRmeta(out_meta, list("is_merged" = TRUE))
#     class(out_meta) <- c("takRmeta", class(out_meta))
#     return(out_meta)
# }

# takRcombine.takRperc <- function(x1, x2, ...){
#     all_meta <- extract_takRmeta(x1, x2, ...) # First grab all the metadata
#     all_inp <- c(list(x1), list(x2), list(...))
#     original_length <- length(all_inp) # Needed for averaging, below
#     # Find out which objects are null
#     null_obj <- unlist(lapply(all_inp, is.null))
#     # Find out which objects are empty, if any.
#     empty_obj <- unlist(lapply(all_inp, inherits, what='takRempty'))
#     # Join objects
#     null_empty <- as.logical(null_obj+empty_obj)
#     if(all(null_empty)){
#         all_dat <- NA
#         class(all_dat) <- c('takRperc', 'takRempty', class(all_dat))
#         return(all_dat)
#     }
#     # Remove null or empty sections
#     all_inp <- all_inp[!null_empty]
#     rowcount <- unlist(lapply(all_inp, nrow))
#     if(!compare(rowcount)){
#         stop("All inputs must have the same number of rows (i.e. quadrats).")
#     }
#     # Melt and then reshape the input data by summing the double-ups...
#     all_dat <- sum_on_col(all_inp)
#     
#     # Convert to a mean based on original input length
#     all_dat <- all_dat/original_length 
#     
#     all_dat <- toattr_takRmeta(all_dat, all_meta) # Add back the combined metadata
#     class(all_dat) <- c("takRperc", "takRwide", class(all_dat))
#     return(all_dat)
# }

# takRcombine.takRcount <- function(x1, x2, ...){
#     all_meta <- extract_takRmeta(x1, x2, ...) # First grab all the metadata
#     all_inp <- c(list(x1), list(x2), list(...))
#     original_length <- length(all_inp) # Not really needed...
#     # Find out which objects are null
#     null_obj <- unlist(lapply(all_inp, is.null))
#     # Find out which objects are empty, if any.
#     empty_obj <- unlist(lapply(all_inp, inherits, what='takRempty'))
#     # Join objects
#     null_empty <- as.logical(null_obj+empty_obj)
#     if(all(null_empty)){
#         all_dat <- NA
#         class(all_dat) <- c('takRcount', 'takRempty', class(all_dat))
#         return(all_dat)
#     }
#     # Remove null or empty sections
#     all_inp <- all_inp[!null_empty]
#     rowcount <- unlist(lapply(all_inp, nrow))
#     if(!compare(rowcount)){
#         stop("All inputs must have the same number of rows (i.e. quadrats).")
#     }
#     # Melt and then reshape the input data by summing the double-ups...
#     all_dat <- sum_on_col(all_inp)
#     
#     all_dat <- toattr_takRmeta(all_dat, all_meta) # Add back the combined metadata
#     class(all_dat) <- c("takRcount", "takRwide", class(all_dat))
#     return(all_dat)
# }

# takRcombine.takRsf <- function(x1, x2, ...){
#     all_meta <- extract_takRmeta(x1, x2, ...) # First grab all the metadata
#     all_inp <- c(list(x1), list(x2), list(...))
#     original_length <- length(all_inp) # Not really needed
#     # Find out which objects are null
#     null_obj <- unlist(lapply(all_inp, is.null))
#     # Find out which objects are empty, if any.
#     empty_obj <- unlist(lapply(all_inp, inherits, what='takRempty'))
#     # Join objects
#     null_empty <- as.logical(null_obj+empty_obj)
#     if(all(null_empty)){
#         all_dat <- NA
#         class(all_dat) <- c('takRsf', 'takRempty', class(all_dat))
#         return(all_dat)
#     }
#     # Remove null or empty sections
#     all_inp <- all_inp[!null_empty]
#     rowcount <- unlist(lapply(all_inp, nrow))
#     if(!compare(rowcount)){
#         stop("All inputs must have the same number of rows (i.e. quadrats).")
#     }
#     all_dat <- do.call(cbind, all_inp)
#     all_dat <- toattr_takRmeta(all_dat, all_meta) # Add back the combined metadata
#     class(all_dat) <- c("takRsf", "takRwide", class(all_dat))
#     return(all_dat)
# }

# Summary Methods
# summary.takRbt <- function(object, ...){
#     out <- list('meta' = object[['meta']])
#     keys <- names(object)
#     keys <- keys[!keys=='meta']
#     for(key in keys){
#         out[[key]] <- summary(object[[key]])
#     }
#     class(out) <- c("summary.takRbt", class(out))
#     return(out)
# }

# summary.takRperc <- function(object, ...){
#     if(inherits(object, what = "takRempty")){
#         out <- NA
#         class(out) <- c("takRempty", class(out))
#         return(out)
#     }
#     dat_mean <- apply(object, 2, mean)
#     dat_median <- apply(object, 2, median)
#     dat_n <- apply(object, 2, length)
#     dat_sd <- apply(object, 2, sd)
#     dat_se <- dat_sd/sqrt(dat_n)
#     out <- cbind("mean"=dat_mean, "median"=dat_median, 
#                  "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
#     class(out) <- c("summary.takRperc", class(out))
#     return(out)
# }

# summary.takRcount <- function(object, ...){
#     if(inherits(object, what = "takRempty")){
#         out <- NA
#         class(out) <- c("takRempty", class(out))
#         return(out)
#     }
#     dat_mean <- apply(object, 2, mean)
#     dat_median <- apply(object, 2, median)
#     dat_n <- apply(object, 2, length)
#     dat_sd <- apply(object, 2, sd)
#     dat_se <- dat_sd/sqrt(dat_n)
#     out <- cbind("mean"=dat_mean, "median"=dat_median, "n"=dat_n, 
#                  "sd"=dat_sd, "se"=dat_se)
#     class(out) <- c("summary.takRcount", class(out))
#     return(out)
# }

# summary.takRsf <- function(object, ...){
#     out <- list()
#     n_quad <- attr(x = object, which = "takRmeta_n_quad", exact = TRUE)
#     quad_size <- attr(x = object, which = "takRmeta_quad_size", exact = TRUE)
#     takRdef_units <- attr(x = object, which = "takRdef_units", exact = TRUE)
#     
#     if(is.null(n_quad)){warning("n_quad not supplied as an attibute.", 
#                                 call. = FALSE, immediate. = TRUE)}
#     if(is.null(quad_size)){warning("quad_size not supplied as an attibute.", 
#                                    call. = FALSE, immediate. = TRUE)}
#     
#     if(inherits(object, what = "takRempty")){
#         if(is.null(n_quad) || is.na(n_quad)){
#             out[["density"]] <- NA
#             out[["density_summary"]] <- NA
#         } else {
#             out[["density"]] <- rep(0, times = n_quad)
#             out[["density_summary"]] <- cbind("mean" = 0, "median" = 0, 
#                                               "n" = n_quad, "sd" = 0, "se" = 0)
#         }
#         out[["data_summary"]] <- NA
#         class(out) <- c("takRempty", class(out))
#         return(out)
#     }
#     
#     # If not empty, we need to do some math!
#     not_na <- object[!is.na(object)]
#     
#     # Density by quadrat (i.e. row) 
#     den_quad <- apply(object, 1, function(x){sum(!is.na(x))})
#     den_quad <- den_quad*(1/quad_size)
#     out[["density"]] <- den_quad
#     
#     # Density summary
#     #den_all <- length(not_na)/n_quad 
#     #den_all <- den_all*(1/quad_size) # Overall, no error. Same as mean, below.
#     den_mean <- mean(den_quad)
#     den_median <- median(den_quad)
#     den_n <- length(den_quad)
#     den_sd <- sd(den_quad)
#     den_se <- den_sd/sqrt(den_n)
#     out[["density_summary"]] <- cbind("mean"=den_mean, "median"=den_median, 
#                                       "n"=den_n, "sd"=den_sd, "se"=den_se)
#     rownames(out[["density_summary"]]) <- "All"
#     
#     # Data (usually "size") summary
#     dat_mean <- mean(not_na)
#     dat_median <- median(not_na)
#     dat_n <- length(not_na)
#     dat_sd <- sd(not_na)
#     dat_se <- dat_sd/sqrt(dat_n)
#     out[["data_summary"]] <- cbind("mean"=dat_mean, "median"=dat_median, 
#                                    "n"=dat_n, "sd"=dat_sd, "se"=dat_se)
#     rownames(out[["data_summary"]]) <- "All"
#     attr(out[["data_summary"]], "takRdef_units") <- takRdef_units
#     
#     class(out) <- c("summary.takRsf", class(out))
#     return(out)
# }


# Unneeded Print methods
# print.summary.takRbt <- function(x, ...){
#     keys <- names(x)
#     for(key in keys){
#         cat("\n", key, "\n", sep="")
#         cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
#         print(x[[key]], ...)
#     }
# }

# print.takRbt <- function(x, ...){
#     keys <- names(x)
#     for(key in keys){
#         cat("\n", key, "\n", sep="")
#         cat(rep("-", times=stringr::str_length(key)), "\n", sep="")
#         print(x[[key]], ...)
#     }
# }

# print.summary.takRperc <- function(x, digits = getOption("digits")-3, ...){
#     print.table(x, digits = digits)
# }

# print.summary.takRcount <- function(x, digits = getOption("digits")-3, ...){
#     print.table(x, digits = digits)
# }

# print.summary.takRsf <- function(x, digits = getOption("digits")-3, ...){
#     if(all(is.na(x[["density"]]))){
#         cat("< Density Information Unavailable >\n")
#     } else {
#         cat("Density (per m^2)\n")
#         print.table(x[["density_summary"]], digits = digits)
#         cat("\n")
#     }
#     cat(paste0("Data (", attr(x[["data_summary"]], "takRdef_units", exact = TRUE), ")\n"))
#     print.table(x[["data_summary"]], digits = digits)
# }

# Plot methods, never used
# Plot
# plot.takRsf <- function(x, main = NULL, xlab = "Size", ylab = "Frequency", las = 1, ...){
#     dat <- x[!is.na(x)]
#     old_par_mar <- par()$mar
#     par(mar=c(5,4,1,1))
#     hist(dat, main = NULL, xlab="Size", ylab="Frequency", las = las, ...)
#     par(mar=old_par_mar)
# }

# plot.takRperc <- function(x, ...){
#     xval <- rep(1:ncol(x), each=nrow(x))
#     yval <- as.vector(x)
#     labs <- colnames(x)
#     xmar <- max(stringr::str_length(labs))/2
#     old_par_mar <- par()$mar
#     par(mar=c(xmar,4,1,1))
#     plot(xval, yval, ann=F, axes=F, pch=19, col=rgb(0,0,0,0.2), ...)
#     box()
#     axis(1, lwd=0, lwd.ticks=1, labels = labs, at=1:ncol(x), las=2)
#     axis(2, lwd=0, lwd.ticks=1, las=1)
#     mtext(text = "% Cover", side = 2, line=2.5)
#     par(mar=old_par_mar)
# }

# plot.takRcount <- function(x, ...){
#     xval <- rep(1:ncol(x), each=nrow(x))
#     yval <- as.vector(x)
#     labs <- colnames(x)
#     xmar <- max(stringr::str_length(labs))/2
#     old_par_mar <- par()$mar
#     par(mar=c(xmar,4,1,1))
#     plot(xval, yval, ann=F, axes=F, pch=19, col=rgb(0,0,0,0.2), ...)
#     box()
#     axis(1, lwd=0, lwd.ticks=1, labels = labs, at=1:ncol(x), las=2)
#     axis(2, lwd=0, lwd.ticks=1, las=1)
#     mtext(text = "Number", side = 2, line=2.5)
#     par(mar=old_par_mar)
# }


# map_attributes <- function(x, out, regex = "^(takRmeta_|takRdef_)") {
#     # Add back attributes beginning with "takRmeta_" or "takRdef_" from `x` to `out`
#     indx <- stringr::str_detect(names(attributes(x)), regex)
#     if(any(indx)){
#         attr_names <- names(attributes(x))[indx]
#         for(a in 1:length(attr_names)){
#             attr(out, attr_names[a]) <- attr(x, attr_names[a])
#         }
#     }
#     return(out)
# }

# rep_quiet: Quietly return NULL if x is NULL (while retaining other messages from rep)
# rep_quiet <- function(x, ...){
#     if(is.null(x)){
#         return(NULL)
#     } else {
#         rep(x, ...)
#     }
# }

# is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
#     if(!is.numeric(x)){
#         return(FALSE)
#     } else {
#         indx <- abs(x - round(x)) < tol
#         return(indx)
#     }
# }

# # expand_args: Build an argument list using recycling rules
# expand_args <- function(...){
#     dots <- list(...)
#     max_length <- max(sapply(dots, length))
#     lapply(dots, rep_quiet, length.out = max_length)
# }

# compare <- function(v){
#     first <- v[1]
#     rest <- as.list(v[-1])
#     res <- sapply(rest, FUN=function(z){ identical(z, first) })
#     return(all(res))
# }

# sum_on_col <- function(x){
#     # Sum columns with matching names
#     # This works equally well if x is a list becuase melt operates by melting the components of a list...
#     x_m <- reshape2::melt(x, as.is=TRUE)
#     x_m[is.na(x_m$value)] <- 0
#     # Cast the data as an array that is row-by-col, summing accross matches....
#     x_out <- reshape2::acast(x_m, Var1~Var2, fun.aggregate=sum)
#     if(inherits(x, "takRwide")){
#         # If it came in as a takRwide, send it back as the same....
#         class(x_out) <- class(x)
#     } else {
#         # Otherwise, send back a takRwide object
#         class(x_out) <- c("takRwide", class(x_out))
#     }
#     # Map any "takR*" attributes
#     x_out <- map_attributes(x, x_out)
#     return(x_out)
# }