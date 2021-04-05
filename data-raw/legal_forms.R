# ## code to prepare `table_legal_forms` dataset goes here
# library(tidyverse); library(janitor); library(stringi); library(ISOcodes)
# devtools::load_all()
#
# tab_iso <- as_tibble(ISO_3166_1) %>%
#   select(Alpha_2, country_code = Alpha_3, country_name = Name)
#
# tab_gleif0 <- read_delim("data-raw/2020-11-19_elf-code-list-v1.3.csv", ",") %>%
#   clean_names()
# table_gleif_legal_forms <- tab_gleif0 %>%
#   select(legal_form_id = elf_code, Alpha_2 = country_code_iso_3166_1,
#          local_full = entity_legal_form_name_local_name,
#          local_abbr = abbreviations_local_language,
#          trans_full = entity_legal_form_name_transliterated_name_per_iso_01_140_10,
#          trans_abbr = abbreviations_transliterated
#          ) %>%
#   pivot_longer(local_full:trans_abbr, names_to = "type", values_to = "legal_form") %>%
#   filter(!is.na(legal_form)) %>%
#   mutate(legal_form = stri_split_fixed(legal_form, ";")) %>%
#   unnest(legal_form) %>%
#   mutate(
#     legal_form = stri_trans_general(legal_form, "latin-ascii"),
#     legal_form = stri_replace_all_regex(legal_form, "[[:punct:]]", ""),
#     legal_form = trimws(tolower(legal_form))
#     ) %>%
#   filter(stri_enc_isascii(legal_form)) %>%
#   left_join(tab_iso, by = "Alpha_2") %>%
#   select(-Alpha_2) %>%
#   distinct(legal_form_id, country_code, legal_form) %>%
#   arrange(country_code) %>%
#   filter(!is.na(country_code)) %>%
#   mutate(source = "GLEIF")
#
#
#
#
# tab_ecb01 <- openxlsx::read.xlsx("data-raw/List_of_legal_forms.xlsx", 2, startRow = 2) %>%
#   clean_names()
# tab_ecb02 <- openxlsx::read.xlsx("data-raw/List_of_legal_forms.xlsx", 3, startRow = 2) %>%
#   clean_names()
#
# tab_ecb <- bind_rows(tab_ecb01, tab_ecb02) %>%
#   distinct() %>%
#   select(legal_form_id = legal_form, Alpha_2 = country_iso_code,
#          local_abbr = legal_form_acronym_in_the_country_of_origin_if_applicable,
#          local_full = extensive_title_description,
#          trans_full = english_name_description
#          ) %>%
#   pivot_longer(local_abbr:trans_full, names_to = "type", values_to = "legal_form") %>%
#   filter(!is.na(legal_form)) %>%
#   mutate(legal_form = stri_split_fixed(legal_form, "/")) %>%
#   unnest(legal_form) %>%
#   mutate(
#     legal_form = stri_trans_general(legal_form, "latin-ascii"),
#     legal_form = stri_replace_all_regex(legal_form, "[[:punct:]]", ""),
#     legal_form = trimws(tolower(legal_form))
#   ) %>%
#   filter(stri_enc_isascii(legal_form)) %>%
#   left_join(tab_iso, by = "Alpha_2") %>%
#   select(-Alpha_2) %>%
#   distinct(legal_form_id, country_code, legal_form) %>%
#   arrange(country_code) %>%
#   filter(!is.na(country_code)) %>%
#   mutate(source = "ECB")
#
# tab_wiki <- read_csv2("data-raw/wikipedia.csv") %>%
#   mutate(legal_form_id = paste0(source, str_pad(row_number(), 3, pad = 0))) %>%
#   unite(legal_form, lf_acr, lf_ext, sep = "|", na.rm = TRUE) %>%
#   mutate(legal_form = stri_split_fixed(legal_form, "|")) %>%
#   unnest(legal_form) %>%
#   distinct() %>%
#   mutate(
#     legal_form = stri_trans_general(legal_form, "latin-ascii"),
#     legal_form = stri_replace_all_regex(legal_form, "[[:punct:]]", ""),
#     legal_form = trimws(tolower(legal_form))
#   ) %>%
#   filter(stri_enc_isascii(legal_form)) %>%
#   mutate(country_code = Rcountry::standardize_countries(country, missing = "remove"))
# tab_wiki_eu <- filter(tab_wiki, country == "EU") %>%
#   left_join(select(Rcountry::tab_countries, iso3, continent_code), by = c("country" = "continent_code")) %>%
#   mutate(country_code = iso3) %>%
#   select(-iso3)
#
# tab_wiki <- tab_wiki %>%
#   filter(!country == "EU") %>%
#   bind_rows(tab_wiki_eu) %>%
#   distinct(legal_form_id, country_code, legal_form) %>%
#   arrange(country_code) %>%
#   filter(!is.na(country_code)) %>%
#   mutate(source = "WIKI")
#
# table_legal_forms <- bind_rows(table_gleif_legal_forms, tab_ecb, tab_wiki)
# usethis::use_data(table_legal_forms, overwrite = TRUE)


## code to prepare `table_legal_forms` dataset goes here
library(tidyverse); library(janitor); library(stringi); library(ISOcodes)
devtools::load_all()

tab_iso <- as_tibble(ISO_3166_1) %>%
  select(Alpha_2, country_code = Alpha_3, country_name = Name)

tab_gleif0 <- read_delim("data-raw/2020-11-19_elf-code-list-v1.3.csv", ",") %>%
  clean_names()
table_gleif_legal_forms <- tab_gleif0 %>%
  select(legal_form_id = elf_code, Alpha_2 = country_code_iso_3166_1,
         local_full = entity_legal_form_name_local_name,
         local_abbr = abbreviations_local_language,
         trans_full = entity_legal_form_name_transliterated_name_per_iso_01_140_10,
         trans_abbr = abbreviations_transliterated
  ) %>%
  pivot_longer(local_full:trans_abbr, names_to = "type", values_to = "legal_form") %>%
  filter(!is.na(legal_form)) %>%
  mutate(legal_form = stri_split_fixed(legal_form, ";")) %>%
  unnest(legal_form) %>%
  mutate(legal_form = helper_standardize_names(legal_form)) %>%
  filter(stri_enc_isascii(legal_form)) %>%
  left_join(tab_iso, by = "Alpha_2") %>%
  select(-Alpha_2) %>%
  distinct(legal_form_id, country_code, legal_form) %>%
  arrange(country_code) %>%
  filter(!is.na(country_code)) %>%
  mutate(source = "GLEIF")




tab_ecb01 <- openxlsx::read.xlsx("data-raw/List_of_legal_forms.xlsx", 2, startRow = 2) %>%
  clean_names()
tab_ecb02 <- openxlsx::read.xlsx("data-raw/List_of_legal_forms.xlsx", 3, startRow = 2) %>%
  clean_names()

tab_ecb <- bind_rows(tab_ecb01, tab_ecb02) %>%
  distinct() %>%
  select(legal_form_id = legal_form, Alpha_2 = country_iso_code,
         local_abbr = legal_form_acronym_in_the_country_of_origin_if_applicable,
         local_full = extensive_title_description,
         trans_full = english_name_description
  ) %>%
  pivot_longer(local_abbr:trans_full, names_to = "type", values_to = "legal_form") %>%
  filter(!is.na(legal_form)) %>%
  mutate(legal_form = stri_split_fixed(legal_form, "/")) %>%
  unnest(legal_form) %>%
  mutate(legal_form = helper_standardize_names(legal_form)) %>%
  filter(stri_enc_isascii(legal_form)) %>%
  left_join(tab_iso, by = "Alpha_2") %>%
  select(-Alpha_2) %>%
  distinct(legal_form_id, country_code, legal_form) %>%
  arrange(country_code) %>%
  filter(!is.na(country_code)) %>%
  mutate(source = "ECB")

tab_wiki <- read_csv2("data-raw/wikipedia.csv") %>%
  mutate(legal_form_id = paste0(source, str_pad(row_number(), 3, pad = 0))) %>%
  unite(legal_form, lf_acr, lf_ext, sep = "|", na.rm = TRUE) %>%
  mutate(legal_form = stri_split_fixed(legal_form, "|")) %>%
  unnest(legal_form) %>%
  distinct() %>%
  mutate(legal_form = helper_standardize_names(legal_form)) %>%
  filter(stri_enc_isascii(legal_form)) %>%
  mutate(country_code = Rcountry::standardize_countries(country, missing = "remove"))
tab_wiki_eu <- filter(tab_wiki, country == "EU") %>%
  left_join(select(Rcountry::tab_countries, iso3, continent_code), by = c("country" = "continent_code")) %>%
  mutate(country_code = iso3) %>%
  select(-iso3)

tab_wiki <- tab_wiki %>%
  filter(!country == "EU") %>%
  bind_rows(tab_wiki_eu) %>%
  distinct(legal_form_id, country_code, legal_form) %>%
  arrange(country_code) %>%
  filter(!is.na(country_code)) %>%
  mutate(source = "WIKI")

table_legal_forms <- bind_rows(table_gleif_legal_forms, tab_ecb, tab_wiki)
usethis::use_data(table_legal_forms, overwrite = TRUE)
