# Default validators
# Needs to be *after* the supporting functions in the package (alphabetically)
default_takRvalidators <- list()

# takRdef - A collection of things to create a "definition" object
# All items are named
# All item names are unique
## **If** takRdef contains exclusively objects of takRblock:
## It is a "read" definition object
## Check all section text is unique
## Check it has section named "meta" 
default_takRvalidators[["takRdef"]] <- list(
    make_validator(f = assertive::has_names),
    make_validator(f = has_unique_names),
    make_validator(f = readdef_has_unique_text),
    make_validator(f = readdef_has_meta)
)

# takRblock - A single building block of a read definition object.
## Must have a "text" field
## "text" must contain a character string
## Check all sections have str_in and str_out
## Check that str_in and str_out are valid takiwaR structure types
## Check for each str_in it is being stored as a valid str_out
## Check for each str_out that it is being specified as a vaild str_in
## Check that fill is valid for the output structure:
default_takRvalidators[["takRblock"]] <- list(
    # TODO
)

# takRrow
default_takRvalidators[["takRrow"]] <- list(
    make_validator(assertive::is_list)
)
    
# takRcol
default_takRvalidators[["takRcol"]] <- list(
    make_validator(assertive::is_data.frame)
)

# takRempty
default_takRvalidators[["takRempty"]] <- list(
    make_validator(assertive::is_na)
)

# takRwide
default_takRvalidators[["takRwide"]] <- list(
    make_validator(assertive::is_matrix),
    make_validator(assertive::has_dimnames),
    make_validator(assertive::has_rownames),
    make_validator(has_unique_rownames)
)

# takRperc
default_takRvalidators[["takRperc"]] <- list(
    make_validator(assertive::is_numeric),
    make_validator(assertive::is_percentage),
    make_validator(assertive::is_inherited_from, classes = "takRwide"),
    make_validator(assertive::has_colnames),
    make_validator(has_unique_colnames)
)

# takRcount
default_takRvalidators[["takRcount"]] <- list(
    make_validator(assertive::is_numeric),
    make_validator(assertive::is_whole_number),
    make_validator(assertive::is_inherited_from, classes = "takRwide"),
    make_validator(assertive::is_non_negative, treat_na_as = TRUE),
    make_validator(assertive::has_colnames),
    make_validator(has_unique_colnames)
)

# takRsf
default_takRvalidators[["takRsf"]] <- list(
    make_validator(assertive::is_numeric),
    make_validator(assertive::is_non_negative, treat_na_as = TRUE),
    make_validator(assertive::is_inherited_from, classes = "takRwide")
)

# takRpa
default_takRvalidators[["takRpa"]] <- list(
    make_validator(assertive::is_inherited_from, classes = "takRwide"),
    make_validator(assertive::is_logical),
    make_validator(assertive::has_colnames),
    make_validator(has_unique_colnames)
)