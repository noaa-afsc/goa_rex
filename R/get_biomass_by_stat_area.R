# come back to this from the goa_rex_assessment_instructions.qmd



get_biomass_by_stat_area<-function(conn = akfin,data_dir,sp_group_code = 'rex sole',survey_species_code=c('10200'),surv_id = c('47')) {
  source("C:/GitProjects/goa_rex/R/get_spatial_biomass.R", echo = TRUE)
  source("C:/GitProjects/goa_rex/R/query_split_fractions.R", echo = TRUE)
  #fraction of eastern goa biomass in WYak vs SEI for data before 2025
  
  split_fractions<-query_split_fractions(akfin_conn = conn,sp_group_code=sp_group_code) 
  spatial_biomass_data<-get_spatial_biomass(srv_sp_str=survey_species_code,surv_id = surv_id,conn = conn) 

# area_names<-spatial_biomass_data %>% distinct(area_name)
# nmfs_areas<-spatial_biomass_data %>% distinct(nmfs_statistical_area)
# area_types<-spatial_biomass_data %>% distinct(area_type)

# regulatory_areas<-spatial_biomass_data %>% distinct(regulatory_area)


#New biomass get by area_type = NMFS STATISTICAL AREA but this is blank for entries before 2025
e_biomass_new = spatial_biomass_data %>%
  filter(area_type == 'NMFS STATISTICAL AREA',area_name == 'West Yakutat' | area_name == 'Southeast Outside') %>%
  select(year,area_type,area_name,area_id,biomass_mt, biomass_var) %>% 
  select(c(year,area_name,biomass_mt, biomass_var)) %>%
  rename(strata = area_name,
         biomass = biomass_mt,
         var = biomass_var) %>%
  mutate(strata = case_when(strata == 'West Yakutat' ~ 'wyak',
                            strata == 'Southeast Outside' ~'se',
                            TRUE  ~ 'na')) %>%
  arrange(strata,year)

#Get all western-central data
wc_biomass <-spatial_biomass_data %>%
  filter(area_type == 'REGULATORY AREA',area_name == 'Western GOA' | area_name == 'Central GOA') %>%
  select(c(year,area_name,biomass_mt,biomass_var)) %>%
  rename(strata = area_name,
         biomass = biomass_mt,
         var = biomass_var) %>%
  mutate(strata = case_when(strata == 'Western GOA' ~ 'wgoa',
                            strata == 'Central GOA' ~ 'cgoa',
                            TRUE ~ 'na')) %>%
  arrange(strata,year)

#calculate pre-2025 eastern data using the split fractions table, combine years and areas into one data frame
all_biomass <-spatial_biomass_data %>%
  filter(year < 2025,area_name == 'Eastern GOA') %>%
  select(c(year,biomass_mt,biomass_var)) %>%
  left_join(split_fractions) %>%
  mutate(se_biomass = biomass_mt*east_fraction, 
         wyak_biomass = biomass_mt*west_fraction, 
         se_var = biomass_var*east_fraction, 
         wyak_var = biomass_var*west_fraction)%>%
  select(c(year,wyak_biomass, wyak_var, se_biomass, se_var)) %>%
  tidyr::pivot_longer(cols = -year, names_to = c("strata", ".value"), names_sep = "_") %>%
  bind_rows(e_biomass_new,wc_biomass) %>%
  mutate(cv = sqrt(var)/biomass) %>%
  select(-c(var)) %>%
  arrange(strata,year)
write.csv(all_biomass,file = file.path(data_dir,"biomass_by_nmfs_stat_area.csv"))
return(all_biomass)
}
