#' Prepare Firm Name Table
#'
#' @param .tab
#' A Dataframe with at least 3 columns: \cr
#' id: A company identifier \cr
#' ...: any number of columns with company names \cr
#' country: The country of incorporation of the firm
#'
#' @param .regex_names A regular expression that identifies the columns for the company names
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' table<- tibble::tibble(
#' id = 1:3,
#' country = c("DEU", NA, "DEU"),
#' name1 = c("BASF AG", "BASF SE", "BASF AG."),
#' name2 = c("BASF Germany AG", "BASF Germany SE", "XXX")
#' )
#'
#' prepare_tables2(table)
#'
#'
prepare_tables2 <- function(.tab, .regex_names = "name") {
  name_orig <- name_clean <- country <- id_name <- id <- dup <- NULL

  # Check if firm id and country code column is available
  if (!"id" %in% colnames(.tab)) {
    stop("Table must have a firm identifier column ('id')")
  }

  if (!"country" %in% colnames(.tab)) {
    stop("Table must have a country column column ('country')")
  }

  # Check if country is in iso-3166 alpha 3 standard
  if (!all(.tab[["country"]] %in% c(ISOcodes::ISO_3166_1[["Alpha_3"]], NA_character_))) {
    stop("Countries must be in ISO 3166 Alpha 3 Format")
  }


  # Check if table contains more than one firm name column
  i_int_names <- sum(grepl(.regex_names, colnames(.tab)))

  # Put table into long format
  i_tab <- .tab
  if (i_int_names == 0) {
    stop("You must provide a regular expression to capture all columns that contain a firm name", call. = FALSE)
  } else {
    i_tab <- i_tab %>%
      tidyr::pivot_longer(dplyr::matches(.regex_names), names_to = "id_name", values_to = "name_orig") %>%
      dplyr::filter(!is.na(name_orig)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(id_name = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  i_tab %>%
    dplyr::mutate(name_clean = helper_standardize_names(name_orig)) %>%
    dplyr::distinct(id, name_clean, country, .keep_all = TRUE) %>%
    dplyr::mutate(dup = duplicated(name_clean)) %>%
    dplyr::select(id, id_name, name_orig, name_clean, country, dup)
}



#' Extract Legal Forms from Company Names
#'
#' @param .tab A table prepared by prepare_tables()
#' @param .tab_lf Additional Legal Form Table
#' @param .workers Number of parallel workers
#'
#' @return A datframe
#' @export
#'
#' @examples
#' .tab <- table <- prepare_tables2(RFirmMatch::table_test0)
#'
#' ## DEBUG
#' .tab_lf = NULL
#' .workers = 1
#'
#'
#' extract_legal_form2(table)
extract_legal_form2 <- function(.tab, .tab_lf = NULL, .workers = 1) {
  id_tmp <- legal_form <- name_clean <- id <- id_name <- name_orig <- name_adj <-
    country <- dup <- loc <- NULL

  if (!is.null(.tab_lf)) {
    i_tab_lf <- dplyr::bind_rows(RFirmMatch::table_legal_forms, .tab_lf)
  } else {
    i_tab_lf <- RFirmMatch::table_legal_forms
  }

  ichr_lf <- unique(i_tab_lf[["legal_form"]])

  future::plan("multisession", workers = .workers)
  # Search for Legal Forms at the end of the string
  i_tab_lf_extract <- furrr::future_map(
    .x = purrr::set_names(ichr_lf, ichr_lf),
    .f = ~ {
      check_end <- which(endsWith(.tab[["name_clean"]], paste0(" ", .x)))
      check_mid <- - which(grepl(paste0(" ", .x, " "), .tab[["name_clean"]], fixed = TRUE))
      return(c(check_end, check_mid))
    }
  ) %>%
    purrr::compact() %>%
    tibble::enframe(name = "legal_form", value = "id_tmp") %>%
    tidyr::unnest(id_tmp) %>%
    dplyr::mutate(
      loc = dplyr::if_else(id_tmp > 0, "end", "mid"),
      id_tmp = abs(id_tmp)
      ) %>%
    dplyr::arrange(loc) %>%
    dplyr::distinct(id_tmp, .keep_all = TRUE)

  future::plan("default")

  .tab %>%
    dplyr::mutate(id_tmp = dplyr::row_number()) %>%
    dplyr::left_join(i_tab_lf_extract, by = "id_tmp") %>%
    dplyr::mutate(name_adj = dplyr::case_when(
      is.na(legal_form) ~ name_clean,
      !is.na(legal_form) ~ name_clean %>%
        stringi::stri_replace_last_fixed(., legal_form, " ") %>%
        gsub("\\s+", " ", .) %>%
        trimws()
    )) %>%
    dplyr::select(id, id_name, name_orig, name_clean, name_adj, country, dup, legal_form, loc)
}


#' Match company names
#'
#' @param .tab0 Company Table
#' @param .tab1 Matching Table
#' @param .col_match Column name used for matching
#' @param .type c("full", "sub", "approx")
#' @param .min_char only used if .type == "sub"
#' @param .max_dist only used if .type == "approx"
#' @param .method only used if .type == "approx"
#' @param .workers Number of parallel workers (only used for .type == "sub")
#'
#' @return A Dataframe
#' @export
#'
#' @examples
#' .tab0 <- table0 <-  RFirmMatch::table_test0 %>%
#' prepare_tables2() %>%
#' extract_legal_form2()
#'
#' .tab1 <- table1 <-  RFirmMatch::table_test1 %>%
#' prepare_tables2() %>%
#' extract_legal_form2()
#'
#' ## DEBUG
#' .col_match <- quote(name_clean)
#' .min_char = 0.25
#' .max_dist = .25
#' .method = "osa"
#' .workers = 1
match_name2 <- function(
  .tab0, .tab1, .col_match, .type = c("full", "sub", "approx"),
  .min_char = 0.25, .max_dist = 2,
  .method = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
  .workers = 1
) {
  id_tmp <- id_tmp_0 <- id_tmp_1 <- . <- match_0 <- match_1 <- tmp_0 <- tmp_1 <-
    name_clean <- n_char_0 <- method <- sim <- char <- id_0 <- id_1 <- id_name_0 <-
    id_name_1 <- name_orig_0 <- name_orig_1 <- name_clean_0 <- name_clean_1 <-
    name_adj_0 <- name_adj_1 <- legal_form_0 <- legal_form_1 <- country_0 <-
    country_1 <- dup_0 <- dup_1 <- max_dist <- NULL


  .type <- match.arg(.type)


  if (.type == "full") {
    i_tab0 <- dplyr::mutate(.tab0, match = {{ .col_match }})
    i_tab1 <- dplyr::mutate(.tab1, match = {{ .col_match }})

    i_tab <- dplyr::inner_join(i_tab0, i_tab1, by = "match", suffix = c("_0", "_1")) %>%
      dplyr::mutate(sim = 1, char = 1, method = "full") %>%
      dplyr::distinct(id_0, id_1, .keep_all = TRUE) %>%
      dplyr::filter(nchar(match) >= 2)

  } else if (.type == "sub") {

    i_tab <- dplyr::inner_join(
      x = helper_get_substrings(.tab0, name_clean, .min_char, .workers),
      y = helper_get_substrings(.tab1, name_clean, .min_char, .workers),
      by = "match",
      suffix = c("_0", "_1")
    ) %>% dplyr::arrange(id_tmp_0, dplyr::desc(n_char_0)) %>%
      dplyr::distinct(id_tmp_0, id_tmp_1, .keep_all = TRUE) %>%
      dplyr::left_join(
        y = dplyr::mutate(.tab0, id_tmp_0 = dplyr::row_number(), tmp = {{ .col_match }}),
        by = "id_tmp_0",
        suffix = c("_0", "_1")
      ) %>%
      dplyr::left_join(
        y = dplyr::mutate(.tab1, id_tmp_1 = dplyr::row_number(), tmp = {{ .col_match }}),
        by = "id_tmp_1",
        suffix = c("_0", "_1")
      ) %>%
      dplyr::mutate(
        sim = stringdist::stringsim(tmp_0, tmp_1),
        char = nchar(match) / n_char_0,
        method = "sub"
      )

  } else if (.type == "approx") {

    i_tab0 <- .tab0 %>%
      dplyr::mutate(match = {{ .col_match }}, tmp = {{ .col_match }}) %>%
      dplyr::filter(nchar(match) >= 2) %>%
      dplyr::mutate(max_dist = floor(nchar(match) * .max_dist)) %>%
      dplyr::filter(max_dist > 0) %>%
      dplyr::rowwise() %>%
      dplyr::group_split()

    i_tab1 <- .tab1 %>%
      dplyr::mutate(match = {{ .col_match }}, tmp = {{ .col_match }}) %>%
      dplyr::filter(nchar(match) >= 2)


    future::plan("multisession", workers = .workers)
    i_tab <- furrr::future_map_dfr(
      .x = i_tab0,
      .f = ~ fuzzyjoin::stringdist_inner_join(
        x = .x,
        y = i_tab1,
        by = "match",
        max_dist = .x[["max_dist"]],
        method = .method,
        ignore_case = TRUE
      ),
      .options = furrr::furrr_options(seed = TRUE)
    ) %>%
      `colnames<-`(gsub("\\.x$", "_0", colnames(.))) %>%
      `colnames<-`(gsub("\\.y$", "_1", colnames(.))) %>%
      dplyr::mutate(sim = stringdist::stringsim(tmp_0, tmp_1)) %>%
      dplyr::mutate(char = NA_character_, match = NA_character_, method = "approx")

    future::plan("default")
  }

  i_tab <- i_tab %>%
    dplyr::select(
      method, match, sim, char, id_0, id_1, id_name_0, id_name_1, name_orig_0, name_orig_1,
      name_clean_0, name_clean_1, name_adj_0, name_adj_1, legal_form_0, legal_form_1,
      country_0, country_1, dup_0, dup_1
    )


  return(i_tab)
}

.col_match <- quote(name_clean)
