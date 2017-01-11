context("find_takRdef") # Tests for section finding
sections <- default_takRdef("takRbt")

raw <- readxl::read_excel("./bt_test_data/bt_sections.xlsx", col_names=FALSE)
expect_is(find_takRdef(raw, sections), "takRdef")
expect_is(find_takRdef(raw, sections), "list")

raw <- readxl::read_excel("./bt_test_data/bt_sections_unordered.xlsx", col_names=FALSE)
expect_is(find_takRdef(raw, sections), "takRdef")
expect_is(find_takRdef(raw, sections), "list")

raw <- readxl::read_excel("./bt_test_data/bt_sections_EOF_missing.xlsx", col_names=FALSE)
expect_error(find_takRdef(raw, sections), "End of file marker")

raw <- readxl::read_excel("./bt_test_data/bt_sections_missing.xlsx", col_names=FALSE)
expect_warning(find_takRdef(raw, sections), "The following section, or sections, were not found")

raw <- readxl::read_excel("./bt_test_data/bt_sections_EOF_preliminary.xlsx", col_names=FALSE)
expect_warning(find_takRdef(raw, sections), "after the EOF and will be excluded")

