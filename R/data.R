#' Legal Form Table
#'
#' A Dataframe containing Legal Forms
#'
#'
#' @format A Dataframe with 9,740 rows and 4 variables:
#' \describe{
#'   \item{legal_form_id}{An ID identifying legal forms}
#'   \item{legal_form}{full or abbreviated legal form}
#'   \item{country_code}{ISO 3166_2 Alpha 3 country code for the legal form}
#'   \item{source}{The source of the legal form}
#' }
"table_legal_forms"

#' Country Code Table
#'
#' A Dataframe containing Country Information
#'
#'
#' @format A Dataframe with 2,305 rows and 8 variables:
#' \describe{
#'   \item{iso3}{...}
#'   \item{iso2}{...}
#'   \item{iso_name}{...}
#'   \item{name}{...}
#'   \item{type}{...}
#'   \item{source}{...}
#'   \item{continent_code}{...}
#'   \item{continent_name}{...}
#' }
"table_country"
