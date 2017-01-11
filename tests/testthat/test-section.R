context("sections") # Tests for selecting / building a section object

expect_is(new_read_definition(), "takRdef")
expect_error(new_read_definition(sec_names = c("meta", "iris_sf")), "`n` cannot be NULL or NA")

expect_is(new_read_definition(sec_names = c("meta", "iris_sf"), n = 10), "takRdef")

sn <- c("meta", "meta")
expect_error(new_read_definition(sec_names = sn), "must be unique")

sn <- c("meta", "cheese")
expect_warning(new_read_definition(sec_names = sn), "Empty values substitued")


