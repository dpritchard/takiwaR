mchi <- function(safe, spp, habitat, names=NULL){
  if (!is.numeric(safe) || length(safe) != 1){
    stop("safe must be a numeric vector of length 1")
  }
  if (!all(is.numeric(spp)) || !any(ncol(spp) == 3, length(spp) == 3)){
    stop("spp must be a numeric matrix with 3 columns, or a numeric vector of length 3")
  }
  if (!all(is.numeric(habitat)) || !(length(habitat) == 6)){
    stop("habitat must be a numeric vector of length 6")
  }
  if (safe < 0 || safe > 4){
    stop("safe must be in the range 0 to 4")
  }
  if (any(spp < 0) || any(spp > 4)){
    stop("spp values must be in the range 0 to 4")
  }
  if (any(na.omit(habitat) < -2) || any(na.omit(habitat) > 4)){
    stop("habitat values must be NAs or in the range -2 to 4")
  }
  if (all(is.na(habitat))){
    stop("You must answer at least one of the questions on forms H and I")
  }
  if(!is.null(nrow(spp))){
    nspp <- nrow(spp)
  } else {
    nspp <- 1
  }
  if (!is.null(names)){
    if(!is.character(names)){
      stop("names must be a character vector")
    }
    if (length(names) != nspp){
      stop("names must have the same length as the number of rows in spp")
    }
  } else {
    if (!is.null(rownames(spp))){
        names <- rownames(spp)
    } else {
        names <- paste("Unknown Species", c(1:nspp))
    }
  }
  
  # -1 and -2 mean NA in this context
  habitat[habitat < 0] <- NA
  # Calculaute the habitat score (out of 4)
  if (is.null(nrow(habitat))){
    habitat_score <- mean(habitat, na.rm=TRUE)
  } else {
    habitat_score <- rowMeans(habitat, na.rm=TRUE)
  }
  
  # OK, begin calculation
  if (is.null(nrow(spp)) || nrow(spp) == 1){
    spp_health <- (sum(spp[-2], na.rm=TRUE) + habitat_score) * spp[2]
  } else {
    spp_health <- (rowSums(spp[, -2], na.rm=TRUE) + habitat_score) * spp[, 2]
  }
  spp_health_ratio <- spp_health / 48
  spp_with_site <- spp_health * safe
  spp_with_site_ratio <- spp_with_site / 192
  num_spp <- ifelse(test = is.null(nrow(spp)), yes = 1, no = nrow(spp))
  spp_weights <- mchi_wspp(num_spp)
  #spp_weighted <- spp_with_site*spp_weights
  spp_weighted <- cumsum(spp_with_site * spp_weights) / cumsum(spp_weights)
  ## spp_weighted is a cumulative average. 
  ## In the XLS sheet, the final score is effectively the same as the last calculated cumulative average score (with some seriously complex if/elses!)
  ## Lets grab that...
  final_score <- tail(spp_weighted, n = 1)
  final_score_ratio <- final_score / 192
  
  ## To make life easier later, we will format some display strings
  spp_health_str <- paste0(round(spp_health, 2))
  spp_health_perc_str <- paste0(round(spp_health_ratio * 100, 0))
  spp_with_site_str <- paste0(round(spp_with_site, 2))
  spp_with_site_perc_str <- paste0(round(spp_with_site_ratio *100, 0))
  final_score_str <- paste0(round(final_score, 2))
  final_score_perc_str <- paste0(round(final_score_ratio * 100, 0))
  
  
  out <- list()
  out <- list("spp_names" = names, "spp_health" = spp_health, 
              "spp_health_ratio" = spp_health_ratio, "spp_with_site" = spp_with_site, 
              "spp_with_site_ratio" = spp_with_site_ratio, "final_score" = final_score, 
              "final_score_ratio" = final_score_ratio,
              "spp_health_str" = spp_health_str, "spp_health_perc_str" = spp_health_perc_str,
              "spp_with_site_str" = spp_with_site_str, "spp_with_site_perc_str" = spp_with_site_perc_str,
              "final_score_str" = final_score_str, "final_score_perc_str" = final_score_perc_str)
  class(out) <- c("mchi", class(out))
  return(out)
}

mchi_wspp <- function(nspp){
  if (!is.numeric(nspp)){
    stop("nspp must be numeric")
  }
  weights <- 1 * exp(log(0.8) * (seq(1:nspp) - 1))
  return(weights)
}

print.mchi <- function(x, ...){
  cat("MCHI Result\n")
  cat("-----------\n")
  cat(paste0("Final score:     ", x$final_score_str, "\n"))
  cat(paste0("Cultural health: ", x$final_score_perc_str, "%\n\n"))
  maxspplen <- max(stringr::str_length(x$spp_names))+2
  colwidth <- 20
  cat("Species scores (%)\n")
  cat(rep(" ", times=maxspplen+1),
      stringr::str_pad("excluding habitat", colwidth, side="both"), 
      stringr::str_pad("including habitat", colwidth, side="both"),
      "\n", 
      sep="")
  for(a in 1:length(x$spp_health)){
    cat(stringr::str_pad(paste0(x$spp_names[a],": "), maxspplen, side="right"),
        stringr::str_pad(paste0(x$spp_health_str[a], " (", x$spp_health_perc_str[a], "%)"), colwidth, side="both"), 
        stringr::str_pad(paste0(x$spp_with_site_str[a], " (", x$spp_with_site_perc_str[a], "%)"), colwidth, side="both"),
        "\n", 
        sep="")
  }
  
}
