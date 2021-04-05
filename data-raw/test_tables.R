.path_test <- "data-raw/test_tables.xlsx"

table_test0 <- openxlsx::read.xlsx(.path_test, 1)
table_test1 <- openxlsx::read.xlsx(.path_test, 2)
usethis::use_data(table_test0, overwrite = TRUE)
usethis::use_data(table_test1, overwrite = TRUE)
