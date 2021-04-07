#' Helper Function: Standardize Names
#'
#' @param .name A character string
#'
#' @return A character string
#' @export
#'
#' @examples
#' helper_standardize_names("Äzz--jkl%&%")
helper_standardize_names <- function(.name) {
  . <- NULL
  .name %>%
    stringi::stri_trans_general(., "latin-ascii") %>%
    stringi::stri_enc_toascii(.) %>%
    gsub("\\+|\\&", " and ", .) %>%
    stringi::stri_replace_all_regex(., "[[:punct:]]", "") %>%
    gsub("\\s+", " ", .) %>%
    tolower() %>%
    trimws()
}

#' Helper Function: Cut Strings
#'
#' @param .string A charcter atring
#' @param .min_char Number between 0 and 1 exclusively (Determines the minimum percent of the string to cut)
#'
#' @return A list with cutted strings
#' @export
#'
#' @examples
#' helper_get_substrings_list("TEST STRING", .8)
helper_get_substrings_list <- function(.string, .min_char) {

  if (.min_char <= 0 | .min_char >= 1) {
    stop("'.min_char' should be between 0 and 1 exclusively", call. = FALSE)
  }

  n_char    <- nchar(.string)
  min_char  <- floor(n_char * .min_char)

  lst_char  <- purrr::map2(min_char, (n_char - 1), ~.x:.y)
  lst_char  <- purrr::map(lst_char, ~.x[.x %% 2 == 0 | length(.x) == 1])

  lst_char  <- purrr::map(lst_char, `length<-`, max(lengths(lst_char)))
  lst_char  <- purrr::transpose(lst_char)
  lst_char  <- purrr::map(lst_char, unlist)

  lst_name  <- purrr::map(lst_char, ~ stringi::stri_sub(.string, 1, .x))
  lst_name  <- purrr::map(purrr::transpose(lst_name), unlist)

  purrr::map(lst_name, ~.x[!is.na(.x)])
}


#' Helper Function: Cut String
#'
#' @param .tab A table
#' @param .col_match The column with the string to be cut
#' @param .min_char Number between 0 and 1 exclusively (Determines the minimum percent of the string to cut)
#'
#' @return A Dataframe
#' @export
#'
#' @examples
#'
#' tab <- tibble::tibble(id = 1, name = "BADISCHE ANELIN UND SODAFABRIK")
#' helper_get_substrings_table(tab, name, .5)
#' rm(tab)
helper_get_substrings_table <- function(.tab, .col_match, .min_char) {
  match <- id <- NULL

  .tab %>%
    dplyr::select(id, match = {{ .col_match }}) %>%
    dplyr::mutate(match = helper_get_substrings_list(match, .min_char)) %>%
    tidyr::unnest(match) %>%
    dplyr::mutate(match = trimws(match))
}




# DEBUG
# future::plan("multisession", workers = 2)
# .tab <- table0 <- RFirmMatch::table_test0 %>%
#   prepare_tables2() %>%
#   extract_legal_form2()
# .col_match <- quote(name_clean)
# .min_char  <- .75

# helper_get_substrings <- function(.tab, .col_match, .min_char) {
#   n_char <- min_char <- id_tmp <- NULL
#
#   i_tab <- .tab %>%
#     dplyr::mutate(match := {{ .col_match }}) %>%
#     dplyr::select(match) %>%
#     dplyr::mutate(id_tmp = dplyr::row_number()) %>%
#     dplyr::mutate(
#       n_char = nchar(match),
#       min_char = floor(n_char * .min_char),
#       diff = n_char - min_char,
#       by = ceiling(diff / 5)
#     ) %>%
#     dplyr::filter(n_char >= 5) %>%
#     dplyr::mutate(seq = purrr::pmap(list(min_char, n_char, by), ~seq.int(..1, ..2, ..3))) %>%
#     dplyr::mutate(
#       match = furrr::future_map2(
#         .x = match,
#         .y = seq,
#         .f = ~ unlist(stringi::stri_sub(str = .x, from = 1, to = .y)),
#         .options = furrr::furrr_options(seed = TRUE, globals = FALSE)
#       )
#     ) %>%
#     dplyr::select(id_tmp, match) %>%
#     tidyr::unnest(match) %>%
#     dplyr::mutate(n_char = nchar(match))
# }
