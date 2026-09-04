#' Make the scenario projection tables
#'
#' @param spm_detail_df 
#'
#' @returns
#' @export
#'
#' @examples
scenario_tables<-function(proj_dir,spm_detail_df, the_scalar) {
  
  catch_table<-spm_detail_df %>% 
    group_by(Year,Alt) %>%
    mutate(catch = Catch*the_scalar) %>%
    summarize(mean_catch = mean(catch)) %>%
    pivot_wider(id_cols = Year,names_from = Alt, values_from = mean_catch)
  
  write.csv(catch_table, file.path(proj_dir,"catch_table.csv"))
  
  ssb_table<-spm_detail_df %>%
    group_by(Year,Alt) %>%
    mutate(ssb= SSB*the_scalar) %>%
    summarize(mean_ssb = mean(ssb)) %>%
    pivot_wider(id_cols = Year,names_from = Alt, values_from = mean_ssb)
  
  write.csv(ssb_table, file.path(proj_dir,"ssb_table.csv"))
  
  F_table<-spm_detail_df %>%
    group_by(Year,Alt) %>%
    summarize(mean_F = mean(F)) %>%
    pivot_wider(id_cols = Year,names_from = Alt, values_from = mean_F)
  write.csv(F_table,file.path(proj_dir,"F_table.csv"))

  the_tables<-list()
  the_tables$catch_table<-catch_table
  the_tables$ssb_table<-ssb_table
  the_tables$F_table<-F_table
return(the_tables)

}