# plot the OSA residuals for the GOA rex sole assessment

devtools::install_github("noaa-afsc/afscOSA", dependencies = TRUE)
library(afscOSA)
library(r4ss)

# other afscOSA package dependencies:
library(ggplot2)
library(cowplot)
library(here)
library(dplyr)
library(reshape2)
library(here)

sx = c(1,2) # USER INPUT define sex
fleet = c(1,2,3) # USER INPUT define fleets

model_path<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2025/rex_cie_review/runs/run7_2021_new_data_and_sampler"
mod <- r4ss::SSgetoutput(dirvec = model_path)


# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))

# effective sample sizes for the fleets defined in "fleet" and "sx"
Neffdf <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16,22)])
Neffdf <- Neffdf[Neffdf$Bin == min(Neffdf$Bin),]

# length bins
lens <- sort(unique(comps$Bin))

# fishery females (fleet 1) ----
flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt & comps$Sex==1,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt & Neffdf$Sex==1]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out1_f <- afscOSA::run_osa(obs = obs, exp = exp, N = Neff, fleet = 'Fishery_Fem',index = lens, years = yrs, index_label = 'Length')

# fishery males (fleet 1) ----
flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt & comps$Sex==2,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt & Neffdf$Sex==2]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out1_m <- afscOSA::run_osa(obs = obs, exp = exp, N = Neff, fleet = 'Fishery_Male',index = lens, years = yrs, index_label = 'Length')

#western-central survey females
# fishery females (fleet 1) ----
flt <- 2 # USER INPUT
sx <-1
# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt & comps$Sex==sx,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt & Neffdf$Sex==sx]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out2_f <- afscOSA::run_osa(obs = obs, exp = exp, N = Neff, fleet = 'Western_Central_Fem',index = lens, years = yrs, index_label = 'Length')


#western-central survey males
# fishery females (fleet 1) ----
flt <- 2 # USER INPUT
sx <-2
# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt & comps$Sex==sx,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt & Neffdf$Sex==sx]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out2_m <- afscOSA::run_osa(obs = obs, exp = exp, N = Neff, fleet = 'Western_Central_Male',index = lens, years = yrs, index_label = 'Length')

#eastern survey females
# fishery females (fleet 1) ----
flt <- 3 # USER INPUT
sx <-1
# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt & comps$Sex==sx,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt & Neffdf$Sex==sx]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out3_f <- afscOSA::run_osa(obs = obs, exp = exp, N = Neff, fleet = 'Eastern_Female',index = lens, years = yrs, index_label = 'Length')

#eastern survey males
# fishery females (fleet 1) ----
flt <- 3 # USER INPUT
sx <-2
# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt & comps$Sex==sx,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt & Neffdf$Sex==sx]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out3_m <- afscOSA::run_osa(obs = obs, exp = exp, N = Neff, fleet = 'Eastern_Male',index = lens, years = yrs, index_label = 'Length')

# plot results ----
input_fishery <- list(out1_f, out1_m)
input_surveys<-list(out2_f,out2_m,out3_f,out3_m)
osaplots_fishery <- plot_osa(input_fishery,outpath = file.path(model_path,"osa_fishery_plots"))
osaplots_fishery$bubble
osaplots_fishery$qq
osaplots_fishery$aggcomp

osaplots_surveys<-plot_osa(input_surveys,outpath = file.path(model_path,"osa_survey_plots"))