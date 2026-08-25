#' Estimate catches for use in harvest projections
#'
#' @param catch_data 
#' @param avg_yrs 
#'
#' @returns
#' @export
#'
#' @examples
#' hp_catch_estimation(the_dir, catch_data,avg_yrs = 5)

hp_catch_estimation<-function(the_dir, catch_data,avg_yrs = 5) {
the_month<- month(today())
end_year <- year(today())
  the_data<-catch_data %>%
    rename_with(tolower) %>%
    dplyr::mutate(weight_posted = ifelse(is.na(weight_posted), 0, weight_posted)) %>%
    dplyr::mutate(day = lubridate::day(week_end_date),month = lubridate::month(week_end_date))
  
  #find the latest data (this needs to be generalized to work every year)
  late<-the_data %>% dplyr::filter(year == end_year,month==the_month) %>%
    dplyr::mutate(end_date= max(as.numeric(day)))
  
  end_day<-unique(late$end_date)
  end_month<-unique(late$month)
  
  #a table of the end-of-year catches over time
  endyr_dat<-the_data %>% 
    dplyr::filter( month > end_month | (month == end_month & day > end_day)) %>%
    dplyr::select(c(year,agency_species_code,weight_posted)) %>%
    dplyr::group_by(year,agency_species_code) %>%
    dplyr::summarize(endyr_weight = sum(weight_posted))
  
  # a table of the past full-year catches over time and the proportion caught at end of year
  past_dat<-the_data %>% select(c(year,agency_species_code,weight_posted)) %>%
    group_by(year,agency_species_code) %>%
    summarize(tot_weight = sum(weight_posted)) %>%
    left_join(endyr_dat) %>%
    mutate(prop_endyr = endyr_weight/tot_weight) %>%
    filter(!is.na(prop_endyr),year<end_year)
  
  # average proportion caught at the end of the year (the period after the current year data ends)
  mean_prop<-past_dat %>% group_by(agency_species_code) %>%
    summarize(mean_prop = mean(prop_endyr))
  
  # the estimated current year catch to enter into the spm.dat file
  est_catch<-the_data %>% group_by(year,agency_species_code) %>%
    summarize(tot_weight = sum(weight_posted)) %>%
    filter(year == end_year) %>%
    left_join(mean_prop) %>%
    mutate(est_weight = tot_weight/(1-mean_prop))
  
  # write everything out
  write.csv(past_dat,file.path(the_dir,"past_catch.csv"))
  write.csv(est_catch,file.path(the_dir,"current_estimated_catch.csv"))
  write.csv(the_data,file.path(the_dir,"council_blend_data.csv"))
  
  # calculate average catches
  the_codes<-the_data %>% distinct(agency_species_code,species_name)
  all_dat <-the_data %>% group_by(year,agency_species_code) %>%
    summarize(tot_weight=sum(weight_posted)) %>%
    pivot_wider(names_from = agency_species_code,values_from = tot_weight)
  write.csv(all_dat,file.path(the_dir,"yearly_catch_by_species.csv"))
  write.csv(the_codes,file.path(the_dir,"species_names_and_codes.csv"))
  #Find average complete catches for the last avg_yrs years
  avg_catches<-past_dat %>% filter(year>=end_year-avg_yrs) %>% ungroup() %>%
    group_by(agency_species_code) %>%
    summarize(avg_catches = mean(tot_weight))
  
  write.csv(avg_catches,file = file.path(the_dir,"five_yr_avg_catch.csv"))
catch_est<-list()
catch_est$past_avg_catches = avg_catches
catch_est$est_current_yr_catch = est_catch
return(catch_est)
}