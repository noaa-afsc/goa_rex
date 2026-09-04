#' Query the spatial biomass from akfin views table biomass_v
#'
#' @param srv_sp_str the survey species code which is 10200 for rex
#' @param surv_id the survey_definition_id which is 47 for the goa
#' @param conn database connection defined externally
#'
#' @returns
#' @export
#'
#' @examples
#' surv_df<-get_spatial_biomass(srv_sp_str=c('10200'),surv_id = c('47'),conn = akfin)
get_spatial_biomass<-function(srv_sp_str=c('10200'),surv_id = c('47'),conn = akfin) {
  
  # raw specimen data
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass_v')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(species_code %in% srv_sp_str,survey_definition_id %in% surv_id) %>%
    dplyr::select(survey_name = survey_name,
                  survey_definition_id = survey_definition_id,
                  area_id = area_id,
                  species_code = species_code,
                  species_name = species_name,
                  year = year,
                  biomass_mt = biomass_mt,
                  biomass_var = biomass_var,
                  area_type = area_type,
                  area_name = area_name,
                  design_year = design_year,
                  region = region,
                  regulatory_area = regulatory_area,
                  nmfs_statistical_area = nmfs_statistical_area) %>%
    dplyr::collect() -> the_data
  return(the_data)
}