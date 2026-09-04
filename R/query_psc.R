#' Query the detailed catch blend data
#'
#' @param akfin_conn name of the akfin connection already opened with ODBC - usually akfin
#' @param region eg 'GOA' 'EBS' 'AI'
#' @returns
#' @export
#'
#' @examples
#' query_psc(akfin_conn = akfin,region = 'GOA')
query_psc<-function(akfin_conn,region) {
  dplyr::tbl(akfin_conn, dplyr::sql('council.comprehensive_psc')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(fmp_area %in% region) %>%
    dplyr::select(week_end_date = week_end_date,
                  trip_target_name = trip_target_name,
                  species_group_name = species_group_name,
                  pscnq_estimate = pscnq_estimate,
                  year = year,
                  fmp_area = fmp_area,
                  halibut_mortality_tons = halibut_mortality_tons) %>%
    dplyr::collect() -> .the_data
  return(.the_data)
}