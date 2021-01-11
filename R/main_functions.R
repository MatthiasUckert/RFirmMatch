#' Prepare Table for Legal Entity Extraction
#'
#' @param .table A Dataframe with at least 3 columns:\cr
#' column 1: 'id': A unique identifier for firm names\cr
#' column 2: 'country_code': ISO 3166_2 Alpha 3 country code for the firm's home country\cr
#' column 3-n: '...name...': 1-n columns with the prefix or suffix 'name'\cr
#' @param .type
#' Either 'firm' or 'match'\cr
#' In case of a matching table rows w/o a country_code are removes
#'
#' @return
#' A dataframe with 5 columns: \cr
#' Column 1: 'id' \<chr\>: A column with the ID(s) \cr
#' Column 2: 'name' \<chr\>: A column with standardized names \cr
#' Column 3: 'country_code' \<chr\>: A column with ISO 3166_2 Alpha 3 country codes \cr
#' Column 4: 'continent_code' \<chr\>: A column with continent codes \cr
#' Column 5: 'n_id' \<int\>: A column with the number of matched IDs to the firm name
#' @export
#'
#' @examples
#' table <- tibble::tibble(
#'  id = 1:2,
#'  country_code = c("DEU", NA),
#'   name1 = c("BASF AG", "BASF SE"),
#'   name2 = c("BASF Germany AG", "BASF Germany SE")
#' )
#' prepare_tables(table, "firm")
#' prepare_tables(table, "match")
prepare_tables <- function(.table, .type = c("firm", "match")) {
  name <- country_code <- id <- iso3 <- continent_code <- NULL

  .type <- match.arg(.type)
  itab <- .table %>%
    tidyr::pivot_longer(dplyr::matches("name"), names_to = "var", values_to = "name") %>%
    dplyr::mutate(name = standardize_names(name)) %>%
    dplyr::arrange(country_code) %>%
    dplyr::distinct(id, name, .keep_all = TRUE) %>%
    dplyr::left_join(
      y = dplyr::distinct(RFirmMatch::table_country, iso3, continent_code),
      by = c("country_code" = "iso3")
    ) %>%
    dplyr::select(id, name, country_code, continent_code) %>%
    dplyr::filter(!is.na(name))

  if (.type == "match") {
    itab <- dplyr::filter(itab, !is.na(country_code))
  }

  itab %>%
    dplyr::group_by(name, country_code, continent_code) %>%
    dplyr::summarise(id = paste(id, collapse = "|"), .groups = "drop") %>%
    dplyr::select(id, name, country_code, continent_code) %>%
    dplyr::mutate(n_id = lengths(id))

}


#' Extract Legal Forms from Company Names
#'
#' @param .table A table prepared by prepare_tables()
#' @param .table_le Additional Legal Entity Table
#' @param .progress Show progress bar?
#'
#' @return
#' A Dataframe with 10 columns: \cr
#' Column 1: 'table_id' \<int\>: ... \cr
#' Column 2: 'id' \<chr\>: A column with the ID(s) \cr
#' Column 3: 'name' \<chr\>: A column with standardized names \cr
#' Column 4: 'name_adj' \<chr\>: A column with firm names w/o legal forms \cr
#' Column 5: 'country_code' \<chr\>: A column with ISO 3166_2 Alpha 3 country codes \cr
#' Column 6: 'continent_code' \<chr\>: A column with continent codes \cr
#' Column 7: 'n_id' \<int\>: A column with the number of matched IDs to the firm name \cr
#' Column 8: 'legal_form' \<chr\>: A column with extracted legal form \cr
#' Column 9: 'legal_form_id' \<list\>: A column with legal form IDs \cr
#' column 10: country_code_legal_form' \<list\>: A column with corresponding ISO 3166_2 Alpha 3 country codes to the legal forms
#' @export
#'
#' @examples
#' table <- tibble::tibble(
#'  id = 1:2,
#'  id2 = 3:4,
#'  country_code = c("DEU", NA),
#'   name1 = c("BASF AG", "BASF SE"),
#'   name2 = c("BASF Germany AG", "BASF Germany SE")
#' )
#' .table <- tab1 <- prepare_tables(table, "firm")
#' tab2 <- prepare_tables(table, "match")
#'
#' extract_legal_form(tab1)
#' extract_legal_form(tab2)
extract_legal_form <- function(.table, .table_le = NULL, .progress = FALSE) {
  table_id <- legal_form <- iso3 <- continent_code <- legal_form_id <-
    country_code <- name <- id <- name_adj <- NULL

  if (!is.null(.table_le)) {
    table_le <- dplyr::bind_rows(RFirmMatch::table_legal_forms, .table_le)
  } else {
    table_le <- RFirmMatch::table_legal_forms
  }

  ichr_lf <- unique(table_le$legal_form)

  pb <- progress::progress_bar$new(total = length(ichr_lf))
  ilst_le <- purrr::map(
    .x = purrr::set_names(paste0(" ", ichr_lf), ichr_lf),
    .f = ~ {
      if (.progress) pb$tick()
      which(endsWith(.table[["name"]], .x))
    }
  ) %>% purrr::compact()


  tab_le <- tibble::tibble(
    table_id = unlist(ilst_le),
    legal_form = rep(names(ilst_le), lengths(ilst_le))
  ) %>%
    dplyr::group_by(table_id) %>%
    dplyr::filter(nchar(legal_form) == max(nchar(legal_form))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(table_le, by = "legal_form") %>%
    dplyr::left_join(
      y = dplyr::distinct(RFirmMatch::table_country, iso3, continent_code),
      by = c("country_code" = "iso3")
    ) %>%
    dplyr::group_by(table_id, legal_form) %>%
    dplyr::summarise(
      dplyr::across(c(legal_form_id, country_code, continent_code), list),
      .groups = "drop"
    )

  tab_le <- tibble::tibble(
    table_id = unlist(ilst_le),
    legal_form = rep(names(ilst_le), lengths(ilst_le))
  ) %>%
    dplyr::group_by(table_id) %>%
    dplyr::filter(nchar(legal_form) == max(nchar(legal_form))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(table_le, by = "legal_form") %>%
    dplyr::left_join(
      y = dplyr::distinct(RFirmMatch::table_country, iso3, continent_code),
      by = c("country_code" = "iso3")
    ) %>%
    dplyr::group_by(table_id, legal_form) %>%
    dplyr::summarise(
      dplyr::across(c(legal_form_id, country_code, continent_code), list),
      .groups = "drop"
    )

  .table %>%
    dplyr::mutate(table_id = dplyr::row_number()) %>%
    dplyr::left_join(
      y = tab_le,
      by = "table_id",
      suffix = c("", "_legal_form")
    ) %>%
    dplyr::mutate(name_adj = stringi::stri_replace_all_regex(
      name, paste0(legal_form, "$"), ""
    )) %>%
    dplyr::mutate(
      name_adj = trimws(name_adj),
      name_adj = gsub("\\s+", " ", name_adj)
      ) %>%
    dplyr::select(table_id, id, name, name_adj, dplyr::everything())
}

#' Full String Firm Names Matching
#'
#' @param .table_firm A Dataframe generated by extract_legal_form()
#' @param .table_match A Dataframe generated by extract_legal_form()
#' @param .type
#' One of c("name", "name_adj")\cr
#' If .type = "name", then names are matched on the full adjusted string\cr
#' If .type = "name_adj", then names are matched on legal form adjusted strings\cr
#'
#' @return
#' A Dataframe with 14 columns:\cr
#' Columns 1-2: 'id_...' \<chr\>: Matched IDs of Firm/Match Tables\cr
#' Column 3: 'match' \<chr\>: Matched String\cr
#' Columns 4-5: 'name_...' \<chr\>: Full standardized name of Firm/Match Table\cr
#' Columns 6-7: 'country_code_...' \<chr\>: The Firm's/Matches' ISO 3166_2 Alpha 3 country code\cr
#' Columns 8-9: 'legal_form_...' \<chr\>: The Firm's/Matches' extracted Legal Form\cr
#' Column 10: 'same_country' \<lgl\>: Logical variable if Firm and Match are from the same country\cr
#' Column 11: 'same_legal_form' \<lgl\>: Logical variable if Firm and Match have the same legal form code\cr
#' Columns 12-13: 'n_id_...' \<int\>: Number of matched Firm/Match IDs\cr
#' Column 14: 'sim' \<num\>: String Similarity (Optimal String Alignment) of the match
#'
#' @export
#'
#' @examples
#' table_firm <- tibble::tibble(
#'  id = 1:2,
#'  country_code = c("DEU", NA),
#'   name1 = c("BASF AG", "BASF SE"),
#'   name2 = c("BASF Germany AG", "BASF Germany SE")
#' )
#' table_match <- tibble::tibble(
#'  id = 1:2,
#'  country_code = c("DEU", "DEU"),
#'   name1 = c("BASF Aktiengesellschaft", "BASF"),
#'   name2 = c("BASF Germany AG", "BASF Germany SE")
#' )
#' table_firm <- prepare_tables(table_firm, "firm")
#' table_match <- prepare_tables(table_match, "match")
#'
#' .table_firm <- table_firm <- extract_legal_form(table_firm)
#' .table_match <- table_match <- extract_legal_form(table_match)
#'
#' match1 <- match_name(table_firm, table_match, "name")
#' match2 <- match_name(table_firm, table_match, "name_adj")
match_name <- function(.table_firm, .table_match, .type = c("name", "name_adj")) {
  id_firm <- id_match <- country_code_firm <- country_code_match <-
    country_code_match <- legal_form_id_match <- name <- legal_form_firm <-
    legal_form_match <- same_country <- same_legal_form <- n_id_firm <-
    n_id_match <- name_firm <- name_match <- name_adj <- legal_form_id_firm <- NULL


  .type <- match.arg(.type)
  itab <- dplyr::inner_join(
    x = .table_firm,
    y = .table_match,
    by = .type,
    suffix = c("_firm", "_match")
  ) %>%
    dplyr::distinct(id_firm, id_match, country_code_firm, .keep_all = TRUE) %>%
    dplyr::mutate(
      same_country = country_code_firm == country_code_match,
      same_legal_form = purrr::map2_lgl(
        .x = legal_form_id_firm,
        .y = legal_form_id_match,
        .f = ~ any(.x %in% .y)
      ),
      n_id_firm = as.integer(stringi::stri_count_fixed(id_firm, "|") + 1L),
      n_id_match = as.integer(stringi::stri_count_fixed(id_match, "|") + 1L),
      dplyr::across(
        .cols = c(id_firm, id_match),
        .fns = ~ purrr::map_chr(., ~ paste(.x, collapse = "|"))
      )
    )

  if (.type == "name") {
    itab %>%
      dplyr::select(
        id_firm, id_match,
        match = name, name_firm = name, name_match = name,
        country_code_firm, country_code_match, legal_form_firm, legal_form_match,
        same_country, same_legal_form, n_id_firm, n_id_match
      ) %>%
      dplyr::mutate(sim = stringdist::stringsim(name_firm, name_match))
  } else {
    itab %>%
      dplyr::select(
        id_firm, id_match,
        match = name_adj, name_firm, name_match,
        country_code_firm, country_code_match, legal_form_firm, legal_form_match,
        same_country, same_legal_form, n_id_firm, n_id_match
      ) %>%
      dplyr::mutate(sim = stringdist::stringsim(name_firm, name_match))
  }
}

#' Sub String Firm Names Matching
#'
#' @param .table_firm A Dataframe generated by extract_legal_form()
#' @param .table_match A Dataframe generated by extract_legal_form()
#' @param .min_char Minum percentage of characters to consider (default = .25)
#' @param .progress Show progress bar?
#'
#' @return
#' A Dataframe with 14 columns:\cr
#' Columns 1-2: 'id_...' \<chr\>: Matched IDs of Firm/Match Tables\cr
#' Column 3: 'match' \<chr\>: Matched String\cr
#' Columns 4-5: 'name_...' \<chr\>: Full standardized name of Firm/Match Table\cr
#' Columns 6-7: 'country_code_...' \<chr\>: The Firm's/Matches' ISO 3166_2 Alpha 3 country code\cr
#' Columns 8-9: 'legal_form_...' \<chr\>: The Firm's/Matches' extracted Legal Form\cr
#' Column 10: 'same_country' \<lgl\>: Logical variable if Firm and Match are from the same country\cr
#' Column 11: 'same_legal_form' \<lgl\>: Logical variable if Firm and Match have the same legal form code\cr
#' Columns 12-13: 'n_id_...' \<int\>: Number of matched Firm/Match IDs\cr
#' Column 14: 'sim' \<num\>: String Similarity (Optimal String Alignment) of the match
#' @export
#'
#' @examples
#' table_firm <- tibble::tibble(
#'  id = 1:2,
#'  country_code = c("DEU", NA),
#'   name1 = c("BASF AG", "BASF SE"),
#'   name2 = c("BASF Germany AG", "BASF Germany SE")
#' )
#' table_match <- tibble::tibble(
#'  id = 1:2,
#'  country_code = c("DEU", "DEU"),
#'   name1 = c("BASF Aktiengesellschaft", "BASF"),
#'   name2 = c("BASF Germany AG", "BASF Germany SE")
#' )
#' table_firm <- prepare_tables(table_firm, "firm")
#' table_match <- prepare_tables(table_match, "match")
#'
#' .table_firm <- table_firm <- extract_legal_form(table_firm)
#' .table_match <- table_match <- extract_legal_form(table_match)
#'
#' match3 <- match_substring(table_firm, table_match)
match_substring <- function(.table_firm, .table_match, .min_char = 0.25, .progress = TRUE) {
  name_adj <- name_firm <- NULL

  int_max = max(nchar(.table_firm$name_adj))
  seq <- seq.int(int_max, 5, by = - 2)

  itab_firm <- .table_firm
  itab_match <- .table_match
  ilst_matches <- list()

  pb <- progress::progress_bar$new(total = length(seq))
  for (i in seq) {
    itab_firm <- dplyr::mutate(itab_firm, name_adj = stringi::stri_sub(name_adj, 1, i))
    itab_match <- dplyr::mutate(itab_match, name_adj = stringi::stri_sub(name_adj, 1, i))

    itab_matches <- match_name(itab_firm, itab_match, "name_adj")

    itab_firm  <- adjust_matches(itab_matches, itab_firm)
    itab_match <- adjust_matches(itab_matches, itab_match)

    ilst_matches[[i]] <- itab_matches %>%
      dplyr::filter(i / nchar(name_firm) >= .min_char)

    if (.progress) pb$tick()
  }

  dplyr::bind_rows(ilst_matches)
}


#' Adjust Tables for already matched names
#'
#' @param .tab_matches A Dataframe generated by match_name() or match_substring()
#' @param .tab A Dataframe generated by extract_legal_form()
#'
#' @return A Dataframe
#' @export
adjust_matches <- function(.tab_matches, .tab) {
  name <- id <- NULL

  ids <- .tab %>%
    dplyr::filter(name %in% .tab_matches$name_match) %>%
    dplyr::pull(id)
  dplyr::filter(.tab, !id %in% ids)
}
