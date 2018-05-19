# takiwaR 1.1.09.0 (Dev)

- Strip out MCHI methods (moved to NTSTAT)

# takiwaR 1.1.0.0 (Release)

- Fix readxl error

# takiwaR 1.0.0.0 (Release)

- is.zero renamed to is_zero. No aliasing is provided.
- length2 removed. Use `length(na.omit(x))` if this is what you mean.

## make_key

- Makes a key (or name) in a format that I like.

# takiwaR 0.0.1.0

Initial release!
Updates to DESCRIPTION to improve CRAN-worthiness
The Description now reads less like a script for bad infomercial for the Takiwa 3.0 project!

## mchi

- Calculates an Marine Cultural Health Index Score.
- An as.char argument that returns all data as nicely formatted strings.
- A simple print method which prints aforementioned nicely formatted strings.

## mchi_wspp

- Provides a weighting function for species scores.
- Currently faithfully reproduces the provided Excel Spreadsheet (i.e. 20% reduction in relative importance per species).

## mchi_as_char

- A non-exported (i.e. non-user-accessible) function to return numbers as nicely formatted strings.
- Currently rounds percentages to zero decimal places and everything else to two decimal places.  
- Not intended for general use!
