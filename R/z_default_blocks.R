# Data types
## This includes static objects for data and metadata types and default blocks (sections)
## Sections and blocks are the same thing. The distinction between naming is as follows:
### If it is related to reading a file, they are might usefull called "Sections"
### Otherwise they could be refered to as "Blocks". 
### - "Sections" implies some degree of uniquness.
### - "Blocks" implies interchagability (as in "building blocks").
### For this reason they are given a class of 'takRblock'

blocks <- list()
blocks[["meta"]] <- list(text="takiwaR Metadata", str_in="row", str_out="takRmeta", fill = NULL, n = NULL)
blocks[["substrate"]] <- list(text="Substrate (% Cover)", str_in="mcol", str_out="takRperc", fill = 0, n = NULL)
blocks[["prim_prod_p"]] <- list(text="Primary Producers (% Cover)", str_in="mcol", str_out="takRperc", fill = 0, n = NULL)
blocks[["prim_prod_c"]] <- list(text="Primary Producers (Counts)", str_in="mcol", str_out="takRcount", fill = 0, n = NULL)
blocks[["creat_p"]] <- list(text="Creatures (% Cover)", str_in="mcol", str_out="takRperc", fill = 0, n = NULL)
blocks[["creat_c"]] <- list(text="Creatures (Counts)", str_in="mcol", str_out="takRcount", fill = 0, n = NULL)
blocks[["iris_sf"]] <- list(text="Iris size frequency", str_in="mrow", str_out="takRsf", fill = NA, n = NULL)
blocks[["australis_sf"]] <- list(text="Australis size frequency", str_in="mrow", str_out="takRsf", fill = NA, n = NULL)
blocks[["chloroticus_sf"]] <- list(text="Chloroticus size frequency", str_in="mrow", str_out="takRsf", fill = NA, n = NULL)
blocks <- lapply(blocks, add_class, "takRblock")

# Input structures to output structure mappings
in_strs <- list()
in_strs[["row"]] <- list(str_out=c("takRrow"))
in_strs[["col"]] <- list(str_out=c("takRcol"))
in_strs[["mcol"]] <- list(str_out=c("takRwide"))
in_strs[["mrow"]] <- list(str_out=c("takRwide"))

# Note that str_in here is the allowed options
out_strs <- list()
out_strs[["takRrow"]] <- list(inherits=NULL, str_in=c("row"), fill = NULL, n = NULL)
out_strs[["takRcol"]] <- list(inherits=NULL, str_in=c("col"), fill = NA, n = NULL)
out_strs[["takRwide"]] <- list(inherits=NULL, str_in=c("mcol", "mrow"), fill = NA, n = NULL)
out_strs[["takRempty"]] <- list(inherits=NULL, str_in=FALSE, fill = NULL, n = NULL)

out_strs[["takRmeta"]] <- list(inherits="takRrow", str_in=c("row"), fill = NULL, n = NULL)
out_strs[["takRperc"]] <- list(inherits="takRwide", str_in=c("mcol"), fill = 0, n = NULL)
out_strs[["takRcount"]] <- list(inherits="takRwide", str_in=c("mcol"), fill = 0, n = NULL)
out_strs[["takRpa"]] <- list(inherits="takRwide", str_in=c("mcol"), fill = FALSE, n = NULL)
out_strs[["takRsf"]] <- list(inherits="takRwide", str_in=c("mrow"), fill = NA, n = NULL)

