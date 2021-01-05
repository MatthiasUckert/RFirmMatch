library(tidyverse); library(stringi)
prepare_tables <- function(.table, .type = c("firm", "match")) {
  .type <- match.arg(.type)
  itab <- .table %>%
    tidyr::pivot_longer(!dplyr::matches("id|country_code"), names_to = "var", values_to = "name") %>%
    dplyr::mutate(
      name = stringi::stri_trans_general(name, "latin-ascii"),
      name = stringi::stri_replace_all_regex(name, "[[:punct:]]", ""),
      name = trimws(tolower(name))
    ) %>%
    dplyr::arrange(country_code) %>%
    dplyr::distinct(id, name, .keep_all = TRUE) %>%
    dplyr::left_join(
      y = dplyr::distinct(table_country, iso3, continent_code),
      by = c("country_code" = "iso3")
    ) %>%
    dplyr::select(id, name, country_code, continent_code) %>%
    dplyr::filter(!is.na(name))

  if (.type == "match") {
    itab <- dplyr::filter(itab, !is.na(country_code))
  }

  itab %>%
    dplyr::group_by(name, country_code, continent_code) %>%
    dplyr::summarise(id = list(id), .groups = "drop") %>%
    dplyr::select(id, name, country_code, continent_code) %>%
    dplyr::mutate(n_id = lengths(id))

}

extract_legal_form <- function(.table, .table_le = NULL, .progress = FALSE) {

  if (!is.null(.table_le)) {
    table_le <- dplyr::bind_rows(table_gleif_legal_forms, .table_le)
  } else {
    table_le <- table_gleif_legal_forms
  }

  ichr_lf <- unique(table_gleif_legal_forms$legal_form)

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
      y = dplyr::distinct(table_country, iso3, continent_code),
      by = c("country_code" = "iso3")
    ) %>%
    dplyr::group_by(table_id, legal_form) %>%
    dplyr::summarise(
      dplyr::across(c(elf_code, country_code, continent_code), list),
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
    dplyr::select(id, name, name_adj, dplyr::everything())
}


tab_firm0 <- read_rds("C:/Users/MUcke/Downloads/test_firms.rds") %>%
  select(id = company_ids, country_code = country_name, name = company_names)


tab_firm1 <- prepare_tables(tab_firm0, "firm")
tab_firm2 <- extract_legal_form(tab_firm1, .progress = TRUE)

tab_match0 <- read_rds("scripts/ds.rds") %>%
  select(id, country_code, everything())
tab_match1 <- prepare_tables(tab_match0, "match")
tab_match2 <- extract_legal_form(tab_match1, .progress = TRUE)

tab_match_init <- tab_match2
tab_firm_init <- tab_firm2

.table_firm <- tab_firm_init
.table_match <- tab_match_init
.type = "name"
match_name <- function(.table_firm, .table_match, .type = c("name", "name_adj")) {
  .type <- match.arg(.type)
  itab <- dplyr::inner_join(
    x = .table_firm,
    y = .table_match,
    by = .type,
    suffix = c("_firm", "_match")
  ) %>%
    dplyr::distinct(id_firm, id_match, country_code_firm, .keep_all = TRUE) %>%
    dplyr::group_by(id_firm) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup()

  if (.type == "name") {
    itab %>%
      dplyr::select(
        id_firm, id_match,
        name_firm = name, name_match = name,
        country_code_firm, country_code_match,
        legal_form_firm, legal_form_match, elf_code_firm, elf_code_match,
        country_code_legal_form_firm, country_code_legal_form_match, n
      ) %>%
      dplyr::mutate(sim = stringdist::stringsim(name_firm, name_match))
  } else {
    itab %>%
      dplyr::select(
        id_firm, id_match,
        name_firm, name_match,
        country_code_firm, country_code_match,
        legal_form_firm, legal_form_match, elf_code_firm, elf_code_match, n,
        country_code_legal_form_firm, country_code_legal_form_match, n
      ) %>%
      dplyr::mutate(sim = stringdist::stringsim(name_firm, name_match))
  }
}
adjust_matches <- function(.tab_matches, .tab) {
  ids <- .tab %>%
    dplyr::filter(name %in% .tab_matches$name_match) %>%
    dplyr::pull(id)
  dplyr::filter(.tab, !id %in% ids)
}
match_substring <- function(.table_firm, .table_match, .min_char = 0.25, .progress = TRUE) {
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


tab1 <- match_name(tab_firm_init, tab_match_init, "name")
tab2 <- match_name(tab_firm_init, tab_match_init, "name_adj")

tab_firm_init <- adjust_matches(tab1, tab_firm_init)
tab_firm_init <- adjust_matches(tab1, tab_firm_init)

tab_match_init <- adjust_matches(tab1, tab_match_init)
tab_match_init <- adjust_matches(tab2, tab_match_init)

tab3 <- match_substring(tab_firm_init, tab_match_init)
tab31 <- filter(tab3, country_code_firm == country_code_match)



test_firm1 <- tab_firm1 %>%
  group_by(name) %>%
  filter(n_distinct(id) > 1) %>%
  ungroup() %>%
  arrange(name)

test_firm2 <- tab_firm2 %>%
  group_by(name_adj) %>%
  filter(n_distinct(id) > 1) %>%
  ungroup() %>%
  arrange(name_adj)

test_match1 <- tab_match2 %>%
  group_by(name, country_code) %>%
  filter(n_distinct(id) > 1) %>%
  ungroup() %>%
  arrange(name)

test_match2 <- tab_match2 %>%
  group_by(name_adj, country_code) %>%
  filter(n_distinct(id) > 1) %>%
  ungroup() %>%
  arrange(name_adj)
.table_firm <-
match_substring <- function(.table_firm, .table_match) {
  int_max


  if (nrow(.tab_c) > 0) {
    .tab_c1 <- dplyr::filter(.tab_c, ops == "op00000", type == 2)
    .tab_m1 <- dplyr::filter(.tab_m, ops == "op00000", type == 2)
    max.nchar <- min(
      max(nchar(dplyr::pull(.tab_c1, name))),
      max(nchar(dplyr::pull(.tab_m1, name)))
    )
    seq <- seq.int(max.nchar, 5, by = -1)
    lst_idx <- list()
    for (i in seq) {
      .tab_c2 <- dplyr::mutate(.tab_c1, name = stringi::stri_sub(
        name,
        1, i
      ))
      .tab_m2 <- dplyr::mutate(.tab_m1, name = stringi::stri_sub(
        name,
        1, i
      ))
      lst_idx[[i]] <- dplyr::inner_join(
        x = .tab_c2, y = .tab_m2,
        by = "name", suffix = c("_c", "_m")
      ) %>%
        dplyr::mutate(match_id = dplyr::group_indices(
          .,
          co_ident_c, co_ident_m
        )) %>%
        dplyr::arrange(nchar(name)) %>%
        dplyr::distinct(match_id, .keep_all = TRUE) %>%
        dplyr::arrange(match_id) %>%
        dplyr::select(-match_id)
      .tab_c1 <- dplyr::filter(.tab_c1, !co_ident %in%
        lst_idx[[i]]$co_ident_c)
      .tab_m1 <- dplyr::filter(.tab_m1, !co_ident %in%
        lst_idx[[i]]$co_ident_m)
    }
    dplyr::bind_rows(lst_idx) %>%
      dplyr::left_join(y = dplyr::select(.tab_c,
        co_ident_c = co_ident, co_name_type_c = co_name_type,
        name_c = name
      ) %>% dplyr::distinct(co_ident_c, co_name_type_c,
        .keep_all = TRUE
      ), by = c("co_ident_c", "co_name_type_c")) %>%
      dplyr::left_join(y = dplyr::select(.tab_m,
        co_ident_m = co_ident,
        co_name_type_m = co_name_type, name_m = name
      ) %>%
        dplyr::distinct(co_ident_m, co_name_type_m,
          .keep_all = TRUE
        ), by = c("co_ident_m", "co_name_type_m")) %>%
      dplyr::mutate(match_type = 2L, match_id = dplyr::group_indices(
        .,
        co_ident_c, co_ident_m
      ), sim = stringdist::stringsim(
        name_c,
        name_m
      )) %>%
      dplyr::arrange(dplyr::desc(nchar(name))) %>%
      dplyr::distinct(match_id, .keep_all = TRUE) %>%
      dplyr::arrange(match_id) %>%
      dplyr::select(
        -match_id,
        -ops_c, -ops_m
      )
  }
}

RFcmatch2::cmatch_match()

tab1 <-


ids_match <- tab_match_init %>% filter(name %in% tab1$name_match) %>% pull(id)
tab_match_init <- filter(tab_match_init, !id %in% ids_match)
ids_firm <- tab_firm_init %>% filter(name %in% tab1$name_firm) %>% pull(id)
tab_firm_init <- filter(tab_firm_init, !id %in% ids_firm)

tab2 <- inner_join(tab_firm_init, tab_match_init, by = "name_adj", suffix = c("_firm", "_match")) %>%
  filter(country_code_firm == country_code_match) %>%
  filter(map2_lgl(elf_code_firm, elf_code_match, ~ any(.x %in% .y))) %>%
  distinct(id_firm, id_match, .keep_all = TRUE) %>%
  group_by(id_firm) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  dplyr::select(
    id_firm, id_match,
    name_firm, name_match,
    country_code_firm, country_code_match,
    legal_form_firm, legal_form_match, n
  )

ids_match <- tab_match_init %>% filter(name %in% tab2$name_match) %>% pull(id)
tab_match_init <- filter(tab_match_init, !id %in% ids_match)
ids_firm <- tab_firm_init %>% filter(name %in% tab2$name_firm) %>% pull(id)
tab_firm_init <- filter(tab_firm_init, !id %in% ids_firm)

tab3 <- inner_join(tab_firm_init, tab_match_init, by = "name", suffix = c("_firm", "_match")) %>%
  filter(continent_code_firm == continent_code_match) %>%
  distinct(id_firm, id_match, .keep_all = TRUE) %>%
  group_by(id_firm) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  dplyr::select(
    id_firm, id_match,
    name_firm = name, name_match = name,
    country_code_firm, country_code_match,
    legal_form_firm, legal_form_match, n
  ) %>%
  dplyr::mutate(sim = 1)

ids_match <- tab_match_init %>% filter(name %in% tab3$name_match) %>% pull(id)
tab_match_init <- filter(tab_match_init, !id %in% ids_match)
ids_firm <- tab_firm_init %>% filter(name %in% tab3$name_firm) %>% pull(id)
tab_firm_init <- filter(tab_firm_init, !id %in% ids_firm)

tab4 <- inner_join(tab_firm_init, tab_match_init, by = "name_adj", suffix = c("_firm", "_match")) %>%
  filter(continent_code_firm == continent_code_match) %>%
  filter(map2_lgl(elf_code_firm, elf_code_match, ~ any(.x %in% .y))) %>%
  distinct(id_firm, id_match, .keep_all = TRUE) %>%
  group_by(id_firm) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  dplyr::select(
    id_firm, id_match,
    name_firm, name_match,
    country_code_firm, country_code_match,
    legal_form_firm, legal_form_match, n
  )

ids_match <- tab_match_init %>% filter(name %in% tab4$name_match) %>% pull(id)
tab_match_init <- filter(tab_match_init, !id %in% ids_match)
ids_firm <- tab_firm_init %>% filter(name %in% tab4$name_firm) %>% pull(id)
tab_firm_init <- filter(tab_firm_init, !id %in% ids_firm)




tab5 <- fuzzyjoin::stringdist_inner_join(tab_firm_init[1:2, ], tab_match_init, by = "name", max_dist = 2)
