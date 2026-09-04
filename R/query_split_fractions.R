#' Get the proportion of eastern goa biomass in West Yakutat and Southeast Inside for years before the 2025 survey re-design
#'
#' @param akfin_conn 
#' @param sp_group_code 
#'
#' @returns
#' @export
#'
#' @examples
#' query_split_fractions(akfin_conn = akfin,sp_group_code='rex sole') {

query_split_fractions<-function(akfin_conn,sp_group_code) {
  dplyr::tbl(akfin_conn, dplyr::sql('gap_products.akfin_split_fractions')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(management_group == sp_group_code) %>%
    dplyr::select(year = year,
                  east_fraction = east_fraction,
                  west_fraction = west_fraction,
                  species = management_group) %>%
    dplyr::collect() -> .the_data
  return(.the_data)
}