#' Query the detailed catch blend data
#'
#' @param akfin_conn name of the akfin connection already opened with ODBC - usually akfin
#' @param species_group_code agency code for the species or stock or complex of interest in the akfin.council.comprehensive_blend data
#' @param region eg 'GOA' 'EBS' 'AI'
#' @returns
#' @export
#'
#' @examples
#' query_catch_blend(akfin_conn=akfin,sp_group_code='REXS',region= c('WG','CG'))
#' query_catch_blend(akfin_conn=akfin,sp_group_code='REXS',region= c('WY','EY','SE'))
#' query_catch_blend(akfin_conn=akfin,sp_group_code='REXS',region= c('WG','CG','WY','EY','SE'))
query_catch_blend<-function(akfin_conn,sp_group_code,region) {
  dplyr::tbl(akfin_conn, dplyr::sql('council.comprehensive_blend_ca')) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(species_group_code == sp_group_code) %>%
  dplyr::filter(fmp_subarea %in% region) %>%
  dplyr::select(week_end_date = week_end_date,
                retained_or_discarded = retained_or_discarded,
                weight_posted = weight_posted,
                year = year,
                fmp_area = fmp_area,
                agency_species_code = agency_species_code,
                species_name,
                species_group_code = species_group_code,
                fmp_subarea = fmp_subarea,
                reporting_area_code =reporting_area_code,
                species_name = species_name,
                species_group_name = species_group_name,
                akfin_species_code = akfin_species_code,
                agency_gear_code = agency_gear_code,
                fmp_gear = fmp_gear,
                reporting_area_code,
                harvest_sector) %>%
  dplyr::collect() -> .the_data
  return(.the_data)
}