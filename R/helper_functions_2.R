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

helper_get_substrings <- function(.tab, .col_match, .min_char, .workers = 1) {
  n_char <- min_char <- id_tmp <- NULL

  i_tab <- .tab %>%
    dplyr::mutate(match := {{ .col_match }}) %>%
    dplyr::select(match) %>%
    dplyr::mutate(id_tmp = dplyr::row_number()) %>%
    dplyr::mutate(
      n_char = nchar(match),
      min_char = floor(n_char * .min_char),
      diff = n_char - min_char,
      by = ceiling(diff / 5)
    ) %>%
    dplyr::filter(n_char >= 5)

  i_tab <- i_tab %>%
    dplyr::mutate(seq = purrr::pmap(list(min_char, n_char, by), ~seq.int(..1, ..2, ..3)))


  future::plan("multisession", workers = .workers)
  i_tab <- i_tab %>%
    dplyr::mutate(
      match = furrr::future_map2(
        .x = match,
        .y = seq,
        .f = ~ unlist(stringi::stri_sub(str = .x, from = 1, to = .y)),
        .options = furrr::furrr_options(seed = TRUE)
      )
    )
  future::plan("default")

  i_tab %>%
    dplyr::select(id_tmp, match) %>%
    tidyr::unnest(match) %>%
    dplyr::mutate(n_char = nchar(match))
}
