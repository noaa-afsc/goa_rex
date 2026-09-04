# Year, Expected Catch, OFL, ABC, Mean SSB, Mean Relative Spawning Biomass 
#(ratio of female spawning biomass to equilibrium unfished female spawning biomass (B100%)


#' Create the five year projection table from the spm results
#'
#' @param hp_dir directory with harvest projection input files and results
#' @param current_year the current year
#' @param alt the spm alternative to use when making this table where carey did alt 1 so that the ABC follows the FMP
#'
#' @returns
#' @export
#'
#' @examples
#' five_year_hp_table<-function(hp_dir = "C:/assessments/goa_rex/run1/projections",current_year = year(today()), alt = 1)
five_year_hp_table<-function(hp_dir,current_year,alt) {
  df <- readr::read_csv(file.path(hp_dir, "spm_detail.csv"))
  input <- spmR::dat2list(file.path(hp_dir, "spm.dat"))
  scalar<-input$scalars
    short_df<-readr::read_csv(file.path(hp_dir, "spm_summary.csv"))
  
  ref_pts <-short_df |>
   dplyr::filter(is.na(Alt)) |>
    dplyr::select(-c(spp_file,Alt,Year)) |>
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  five_yr<-df |>
          dplyr::filter(Year>current_year,Year<=current_year+5,Alt==alt) |>
          dplyr::select(c(Year,Alt,Sim,Catch,OFL,MaxABC,SSB,B100)) |>
          dplyr::group_by(Year) |>
          dplyr::summarise(mean_catch = mean(Catch*scalar),mean_OFL = mean(OFL*scalar),mean_MaxABC = mean(MaxABC*scalar),
                           mean_ssb = mean(SSB*scalar),mean_rel_ssb = mean(SSB/B100))
  write.csv(five_yr,file.path(hp_dir,"five_year_table.csv"))          
 return(five_yr)
}