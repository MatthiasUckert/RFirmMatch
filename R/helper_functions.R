#' Standrdize Firm Names (Helper Function)
#'
#' @param .name A character string
#'
#' @return A character string
#' @export
#'
#' @examples
#'  standardize_names("BASF Östereich SE.")
standardize_names <- function(.name) {
  . <- NULL
  .name %>%
    stringi::stri_trans_general(., "latin-ascii") %>%
    stringi::stri_replace_all_regex(., "[[:punct:]]", " ") %>%
    gsub("\\s+", " ", .) %>%
    tolower() %>%
    trimws()
}
