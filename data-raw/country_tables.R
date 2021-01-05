table_country <- Rcountry::tab_countries %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), stringi::stri_escape_unicode))

usethis::use_data(table_country, overwrite = TRUE)
