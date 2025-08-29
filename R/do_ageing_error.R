#this uses https://github.com/pfmc-assessments/AgeingError

#install.packages("AgeingError", repos = c("https://noaa-fisheries-integrated-toolbox.r-universe.dev", "https://cloud.r-project.org"))
library(AgeingError)
library(readxl)
library(dplyr)
library(tidyverse)

mydir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2025/ageing_error"
adata<-readxl::read_excel(file.path(mydir,"rex_sole_goa_all_ages_survey_fishery_1992_to_2022.xlsx"),sheet = "data")

simple<-adata %>% select(read_age,test_age,reader_index,tester) %>% drop_na() %>% mutate(pair = 1:nrow(simple))

simple_reader<-simple %>% select(pair,read_age,reader_index)
simple_dat<-simple %>% select(pair,test_age,tester) %>% 
  rename(read_age = test_age,reader_index = tester) %>% 
  bind_rows(simple_reader) %>%
  pivot_wider(names_from = reader_index,values_from = read_age, id_cols = pair,names_prefix = "reader") %>%
  select(-c(pair))

# need to do a step to organize by reader-tester id pairs but do long form so that each column is truly by reader adn then I think write_files can deal with it.
write_files(dat = simple_dat,dir = mydir, file_dat = "simple_age_error.dat",file_specs = "simple_age_error.spc",
            minage = 0,refage = 12,plusage = 20)

rex_dat <- AgeingError::load_data(file.path(mydir, "simple_age_error.dat"),
                                     NDataSet = 1,
                                     verbose = TRUE, EchoFile = "simple_age_error.out")

rex_spc <- AgeingError::load_specs(file.path(mydir, "simple_age_error.spc"),
                                                  DataSpecs = rex_dat,
                                                  verbose = TRUE)

rex_mod <- AgeingError::DoApplyAgeError(
  Species = "rex",
  DataSpecs = rex_dat,
  ModelSpecsInp = rex_spc,
  AprobWght = 1e-06,
  SlopeWght = 0.01,
  SaveDir = file.path(mydir,"results"),
  verbose = FALSE
)

rex_out <- AgeingError::ProcessResults(Species = "rex", SaveDir = file.path(mydir,"results"), CalcEff = TRUE, verbose = FALSE)

#this produces an ageing error matrix for each reader - what to do with that?
# needs further examination of results and qaqc - might need some help with this.