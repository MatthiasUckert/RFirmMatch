library(tidyverse); library(stringi)

.table <- tab_firm <- tibble(
  id = 1:3,
  country_code = c("DEU", "DEU", NA_character_),
  name1 = c("basf se", "basf ag", "basf deutschland aktiengesellschaft")
)

tab_match <- tibble(
  id = 1:5,
  country_code = c("DEU", "ITA", "DEU", "DEU", "DEU"),
  name1 = c()
)


prepare_tables <- function(.table) {
  itab <- .table %>%
    tidyr::pivot_longer(!dplyr::matches("id|country_code"), names_to = "var", values_to = "name") %>%
    dplyr::mutate(country_code = if_else(is.na(country_code), "ANY", country_code)) %>%
    mutate(
      name = stri_trans_general(name, "latin-ascii"),
      name = stri_replace_all_regex(name, "[[:punct:]]", ""),
      name = trimws(tolower(name))
    ) %>%
    arrange(country_code)

  ilst <- split(itab, itab$country_code)

  return(list(tab = itab, lst = ilst))
}

.list <- prepare_tables(tab_firm)

extract_legal_forms <- function(.list) {
  lst_co <- .list[["lst"]]
  lst_co <- lst_co[sort(names(lst_co))]

  lst_lf <- list_gleif_legal_forms
  lst_lf <- lst_lf[sort(names(lst_lf))]
  lst_lf <- lst_lf[names(lst_lf) %in% names(lst_co)]


  itab_join <- dplyr::bind_rows(
    table_gleif_legal_forms %>%
      dplyr::select(elf_code, legal_form, country_code),
    table_gleif_legal_forms %>%
      dplyr::select(elf_code, legal_form, country_code) %>%
      dplyr::mutate(country_code = "ANY") %>%
      dplyr::group_by(legal_form) %>%
      dplyr::summarise(
        dplyr::across(c(elf_code, country_code), ~ paste(., collapse = ", ")),
        .groups = "drop"
      )
  )

  purrr::map2_dfr(
    .x = lst_co,
    .y = lst_lf,
    .f = ~ {
      .x %>%
        mutate(
          legal_form = stringi::stri_extract_last_regex(name, .y),
          name_adj = trimws(stringi::stri_replace_last_regex(name, .y, ""))
        )
    }
  ) %>% left_join(itab_join, by = c("country_code", "legal_form"))
}


test <- extract_legal_forms(.list)





