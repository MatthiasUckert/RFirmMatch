#' MAke Legal Form Table
#'
#' @param .tab_lf An additional table for legal forms, needs to have at least 3 columns:\cr
#' legal_form_id: A non-unique identifier grouping different spellings of legal forms\cr
#' legal_form: A legal form\cr
#' country_code: country code of the legal form in ISO 3166 Alpha 3 format\cr
#'
#' If NULL, internal table will be returned
#'
#' @return A dataframe with legal forms
#' @export
#'
#' @examples
#'
#' make_legal_form_table()
make_legal_form_table <- function(.tab_lf = NULL) {
  if (is.null(.tab_lf)) {
    RFirmMatch::table_legal_forms %>%
      dplyr::select(-source)
  } else {
    RFirmMatch::table_legal_forms %>%
      dplyr::bind_rows(.tab_lf) %>%
      dplyr::select(-source) %>%
      dplyr::distinct()
  }
}


#' Check if Entities have the same legal form
#'
#' @param .tab A dataframe generated by match_names()
#' @param .tab_lf A dataframe generated by make_legal_form_table()
#'
#' @return A dataframe with one additional column: same_legal_form
#' @export
#'
#' @examples
#'
#' .path <- system.file("extdata", "test_tables.xlsx", package = "RFirmMatch")
#' .tab1 <- openxlsx::read.xlsx(.path, 1)
#' .tab1 <- .tab1 %>% prepare_tables() %>% extract_legal_form(make_legal_form_table())
#'
#' .tab2 <- openxlsx::read.xlsx(.path, 2)
#' .tab2 <- .tab2 %>% prepare_tables() %>% extract_legal_form(make_legal_form_table())
#'
#' .tab <- match_name(.tab1, .tab2, name_clean, "full")
#' .tab_lf <- make_legal_form_table()
#'
#' check_legal_form(.tab, .tab_lf)
#'
#' rm(.tab, .tab_lf)
#'
check_legal_form <- function(.tab, .tab_lf) {
  legal_form_id.x <- legal_form_id.y <- id_0 <- id_1 <- same_legal_form <- NULL

  .tab %>%
    dplyr::left_join(.tab_lf, by = c("legal_form_0" = "legal_form", "country_0" = "country_code")) %>%
    dplyr::left_join(.tab_lf, by = c("legal_form_1" = "legal_form", "country_0" = "country_code")) %>%
    dplyr::mutate(same_legal_form = legal_form_id.x == legal_form_id.y) %>%
    dplyr::select(-legal_form_id.x, -legal_form_id.y) %>%
    dplyr::arrange(id_0, id_1, dplyr::desc(same_legal_form)) %>%
    dplyr::distinct(id_0, id_1, .keep_all = TRUE)
}
