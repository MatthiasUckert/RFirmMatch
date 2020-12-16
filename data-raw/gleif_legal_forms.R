## code to prepare `gleif_legal_forms` dataset goes here
library(tidyverse); library(janitor); library(stringi); library(ISOcodes)

tab_iso <- as_tibble(ISO_3166_1) %>%
  select(Alpha_2, country_code = Alpha_3, country_name = Name)

tab_gleif0 <- read_delim("data-raw/2020-11-19_elf-code-list-v1.3.csv", ",") %>%
  clean_names()
table_gleif_legal_forms <- tab_gleif0 %>%
  select(elf_code, Alpha_2 = country_code_iso_3166_1,
         local_full = entity_legal_form_name_local_name,
         local_abbr = abbreviations_local_language,
         trans_full = entity_legal_form_name_transliterated_name_per_iso_01_140_10,
         trans_abbr = abbreviations_transliterated
         ) %>%
  pivot_longer(local_full:trans_abbr, names_to = "type", values_to = "legal_form") %>%
  filter(!is.na(legal_form)) %>%
  mutate(legal_form = stri_split_fixed(legal_form, ";")) %>%
  unnest(legal_form) %>%
  mutate(
    legal_form = stri_trans_general(legal_form, "latin-ascii"),
    legal_form = stri_replace_all_regex(legal_form, "[[:punct:]]", ""),
    legal_form = trimws(tolower(legal_form))
    ) %>%
  filter(stri_enc_isascii(legal_form)) %>%
  left_join(tab_iso, by = "Alpha_2") %>%
  select(-Alpha_2) %>%
  distinct(elf_code, country_code, legal_form, .keep_all = TRUE) %>%
  arrange(country_code) %>%
  filter(!is.na(country_code))


list_gleif_legal_forms <- split(paste0(table_gleif_legal_forms$legal_form, "$"), table_gleif_legal_forms$country_code)
list_gleif_legal_forms <- purrr::map(list_gleif_legal_forms, ~ paste(.x, collapse = "|"))
list_gleif_legal_forms[["ANY"]] <- paste(unique(paste0(table_gleif_legal_forms$legal_form, "$")), collapse = "|")


usethis::use_data(table_gleif_legal_forms, overwrite = TRUE)
usethis::use_data(list_gleif_legal_forms, overwrite = TRUE)
