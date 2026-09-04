#For running outside of the function and debugging:
write_proj<-function(data_file="ForProjections.dat",
                     sara_file = "SARA.dat",
                     data=replist,GrowthMorph = 1,
                     area = 1,Narea = 2,Nmorph = 2,
                     NAGES=30,FY=1982,LY=2026,
                     RecAgeProject = 3,
                     RecAgeSS = 0,
                     species,
                     Region = "GOA",
                     Tier = "3a",
                     AssessType = "full",
                     SSversion = "SS-V3.30"){
#Written by Steve Barbeaux and Carey McGilliard
#2013, 2017
#Example:
#MyOutput = 
#write_proj(data_file="Flathead_Proj_2.dat",data=MyOutput,NAGES=27,FY=1978,LY=2013,RecAge = 3)

RecAge = RecAgeProject #this changes to RecAgeSS for writing SARA file below
if (Nmorph == 2) {
  Morph1_F<-1
  Morph2_F<-2
  Morph1_M<-3
  Morph2_M<-4
}
## writing prjection file
## mean 5 Yr F
Y5<-LY-5
M1<-as.numeric(subset(data$M_at_age,Yr==(LY-1)&Sex==1&Bio_Pattern==GrowthMorph)[,(4+RecAge-1):(NAGES+3)]) #this is weird bc rick has M for plus gp = NA
M2<-as.numeric(subset(data$M_at_age,Yr==(LY-1)&Sex==2&Bio_Pattern==GrowthMorph)[,(4+RecAge-1):(NAGES+3)])
F_5<-mean( data$timeseries$"F:_1"[data$timeseries$Yr>Y5 & data$timeseries$Area==area])
## population weight at age for females
#Steve's trick: fecundity*maturity: WGT_F<-subset(data$endgrowth,data$endgrowth$Sex==1)[2:(NAGES+1),19]
#CRM: separate out Maturity:
MatAge_F<-data$endgrowth$Age_Mat[data$endgrowth$Sex==1&data$endgrowth$Bio_Pattern==GrowthMorph]
MatAge_M<-data$endgrowth$Age_Mat[data$endgrowth$Sex==2&data$endgrowth$Bio_Pattern ==GrowthMorph]

#CRM: separate out beg yr weight-at-age:
Wt_Beg_F<-data$endgrowth$Wt_Beg[data$endgrowth$Sex==1 & data$endgrowth$Bio_Pattern==GrowthMorph]
Wt_Beg_M<-data$endgrowth$Wt_Beg[data$endgrowth$Sex==2 & data$endgrowth$Bio_Pattern==GrowthMorph]

## selectivity at age for fishery
if (GrowthMorph == 1) {
sel_LY_Trawl_F<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph1_F&Sex==1)
sel_LY_Trawl_M<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph1_M&Sex==2)

wt_LY_Trawl_F<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph1_F&Sex==1)
wt_LY_Trawl_M<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph1_M&Sex==2)
}
if (GrowthMorph == 2) {
sel_LY_Trawl_F<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph2_F&Sex==1)
sel_LY_Trawl_M<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph2_M&Sex==2)

wt_LY_Trawl_F<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph2_F&Sex==1)
wt_LY_Trawl_M<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph2_M&Sex==2)
}

#CRMcommentedout: sel_LY_Long<-subset(data$ageselex,data$ageselex$Fleet==2&data$ageselex$Yr==LY&data$ageselex$Factor=="Asel2")
## weight at age for two fisheries
#wt_LY_Trawl<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt")
#CRMcommentedout: wt_LY_Long<-subset(data$ageselex,data$ageselex$Fleet==2&data$ageselex$Yr==LY&data$ageselex$Factor=="bodywt")
## numbers at age
Nage_LY<-subset(data$natage,data$natage$"Beg/Mid"=="B"& Yr==LY & Area==area & Bio_Pattern==GrowthMorph)


#Make this age RecAge recruits FY to LY
##age 1 recruits 1978 - (LY-1)
rec_F<-as.numeric(data$natage[,(13+RecAge)][data$natage$Yr<=LY&data$natage$Yr>=FY&data$natage$Sex==1&data$natage$"Beg/Mid"=="B"&data$natage$Area==area&data$natage$Bio_Pattern==GrowthMorph])
rec_M<-as.numeric(data$natage[,(13+RecAge)][data$natage$Yr<=LY&data$natage$Yr>=FY&data$natage$Sex==2&data$natage$"Beg/Mid"=="B"&data$natage$Area==area&data$natage$Bio_Pattern==GrowthMorph])
Rec_1 <- (rec_F + rec_M)/2
N_rec<-length(Rec_1)

#SSB<-as.numeric(data$sprseries$SPB[data$natage$Yr<=LY&data$sprseries$Yr>=FY])
if (Nmorph==1) {
 SSB<-as.numeric(data$timeseries$"SpawnBio"[data$timeseries$Yr<=LY&data$timeseries$Yr>=FY&data$timeseries])
}
if (GrowthMorph ==1 & Nmorph>1) {
SSB<-as.numeric(data$timeseries$"SpawnBio_GP:1"[data$timeseries$Yr<=LY&data$timeseries$Yr>=FY&data$timeseries$Area==area])
#SSB<-SSB[1:(LY-FY)]
}

if (GrowthMorph ==2 & Nmorph>1) {
 SSB<-as.numeric(data$timeseries$"SpawnBio_GP:2"[data$timeseries$Yr<=LY&data$timeseries$Yr>=FY&data$timeseries$Area==area])
}

#T1<-noquote(paste(data_file))
T1 = noquote(paste(species))
#write(T1,paste(data_file),ncolumns =  1 )
write(T1,paste(data_file),ncolumns = 1)
T1<-noquote(" 0 # SSL Species???")
  write(T1,paste(data_file),ncolumns = 1,append=T)
T1<-noquote(" 0 # Constant Buffer Dorn?")
  write(T1,paste(data_file),append = T)
T1<-noquote(" 1 # Number of fisheries")
  write(T1,paste(data_file),append = T)
T1<-noquote(" 2 # Number of Sexes")
  write(T1,paste(data_file),append = T)
T1<-noquote(paste(F_5,"# Average 5 Yr F"))
  write(T1,paste(data_file),append = T)
T1<-noquote("1 # Author f")
  write(T1,paste(data_file),append = T)
T1<-noquote("0.4 # SPR ABC")
  write(T1,paste(data_file),append = T)
T1<-noquote("0.35 # SPR MSY")
  write(T1,paste(data_file),append = T)
T1<-noquote("1 # Spawning month")
  write(T1,paste(data_file),append = T)

T1<-noquote(paste0(NAGES-RecAge+1," # number of ages"))
  write(T1,paste(data_file),append = T)

T1<-noquote("1 # Fratio")
  write(T1,paste(data_file),append = T)

T1<-noquote("# natural mortality")
  write(T1,paste(data_file),append = T)
 write(M1,paste(data_file),append = T,ncolumns =  45)
 write(M2,paste(data_file),append = T,ncolumns =  45)## not sure what this is.

T1<-noquote("# Maturity females")
  write(T1,paste(data_file),append = T)
 #Steve's trick: write(rep(1,NAGES),paste(data_file),append = T,ncolumns = 45) ## Female maturity??
 #CRM: straight up maturity
 write(round(as.numeric(MatAge_F[(1+RecAge):length(MatAge_F)]),4),paste(data_file),append=T,ncolumns = 45)

T1<-noquote("# Maturity males")
  write(T1,paste(data_file),append = T)
 #Steve's trick: write(rep(1,NAGES),paste(data_file),append = T,ncolumns =  45)## Male maturity??
 #CRM: straight up maturity
 write(round(as.numeric(MatAge_M[(1+RecAge):length(MatAge_M)]),4),paste(data_file),append=T,ncolumns = 45)
 
T1<-noquote("# wt spawn females")
  write(T1,paste(data_file),append = T)
 #Steve's write(round(as.numeric(WGT_F),4),paste(data_file),append = T,ncolumns =  45)
 #Carey's:
 write(round(as.numeric(Wt_Beg_F[(1+RecAge):length(Wt_Beg_F)]),4),paste(data_file),append = T,ncolumns =  45)

T1<-noquote("# WtAge females by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(wt_LY_Trawl_F[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns =  45)
  #CRMcommentedout: write(round(as.numeric(wt_LY_Long[1,9:(NAGES+8)]),4),paste(data_file),append = T,ncolumns =  45)

T1<-noquote("# WtAge males by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(wt_LY_Trawl_M[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns = 45)
  #CRMcommentedout: write(round(as.numeric(wt_LY_Long[2,9:(NAGES+8)]),4),paste(data_file),append = T,ncolumns = 45)
  
T1<-noquote("# Selectivity females by fishery")
 write(T1,paste(data_file),append = T)
 write(round(as.numeric(sel_LY_Trawl_F[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns =  45)
 #CRMcommentedout: write(round(as.numeric(sel_LY_Long[1,9:(NAGES+8)]),4),paste(data_file),append = T,ncolumns =  45)

 T1<-noquote("# Selectivity males by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(sel_LY_Trawl_M[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns =  45)
  #CRMcommentedout: write(round(as.numeric(sel_LY_Long[2,9:NAGES+8]),4),paste(data_file),append = T,ncolumns =  45)

T1<-noquote(paste0("# Numbers at age in ",LY," females males"))
  write(T1,paste(data_file),append = T)
  write(as.numeric(Nage_LY[1,(13+RecAge):(13+NAGES)]),paste(data_file),append = T,ncolumns =  45)
  write(as.numeric(Nage_LY[2,(13+RecAge):(13+NAGES)]),paste(data_file),append = T,ncolumns =  45)

T1<-noquote("# No Recruitments")
  write(T1,paste(data_file),append = T)
  write(N_rec,paste(data_file),append = T,ncolumns =  45)

T1<-noquote("# Recruitment")
  write(T1,paste(data_file),append = T)
  write(round(Rec_1,1),paste(data_file),append = T,ncolumns =  45)

T1<-noquote(paste("# SSB ", FY,"-",LY,sep=""))
  write(T1,paste(data_file),append = T)
  write(SSB,paste(data_file),append = T,ncolumns =  45)

#------------------------------------------------------------------
# end of projection data file, being sara file
#------------------------------------------------------------------

RecAge = RecAgeSS
#Intro lines:
  myline<-noquote(paste0(species,"   # stock"))
  write(myline,paste(sara_file),append = F)

  myline<-noquote(paste0(Region,"   # region"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0(data$endyr,"   # ASSESS_YEAR - year assessment is presented to the SSC"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0(Tier,"     #Tier (1a 1b 2a 2b 3a 3b 4 5 6"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0("none","     #Tier2 if mixed (none 1a 1b 2a 2b 3a 3b 4 5 6"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0(AssessType,"     #Assessment type (new benchmark full partial)"))
  write(myline,paste(sara_file),append = T)
  
  
  #ssb info:
  sline<-grep("SPB_",data$derived_quants$Label)
  allssb<-data$derived_quants$Value[sline[3:length(sline)]] #leave out SPB_Virgin and SPB_Init
  allssb_std<-data$derived_quants$StdDev[sline[3:length(sline)]] #leave out SPB_Virgin and SPB_Init
  #Minimum SSB for endyr:
  lastyr_line<-grep(paste0("SPB_",LY),data$derived_quants$Label)
  ssb_LY<-data$derived_quants$Value[lastyr_line]
  ssb_std_LY<-data$derived_quants$StdDev[lastyr_line]

  lower_ssb = ssb_LY*exp(-1.96*sqrt(log(1+(ssb_std_LY/ssb_LY)^2)))
  upper_ssb = ssb_LY*exp(1.96*sqrt(log(1+(ssb_std_LY/ssb_LY)^2)));
  
  # 7/8*B40
  Bmsy_proxy = "FILL IN MANUALLY: (7/8)*B40"

  myline<-noquote(paste0(round(lower_ssb,2),"     # Minimum B  Lower 95% confidence interval for spawning biomass in assessment year"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0(round(upper_ssb,2),"     # Maximum B  Upper 95% confidence interval for spawning biomass in assessment year"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0(Bmsy_proxy,"    # Bmsy proxy"))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0('"statistical age-structured model using ADMB"',"     # MODEL - Required only if NMFS toolbox software used; optional otherwise "))
  write(myline,paste(sara_file),append = T)

  myline<-noquote(paste0(SSversion,"     # VERSION - Required only if NMFS toolbox software used; optional otherwise "))
  write(myline,paste(sara_file),append = T)  
  
  myline<-noquote(paste0(data$nsexes,"  # number of sexes  if 1 sex=ALL elseif 2 sex=(FEMALE, MALE)"))
  write(myline,paste(sara_file),append = T)  

  myline<-noquote(paste0(data$nfishfleets,"   # number of fisheries "))
  write(myline,paste(sara_file),append = T)  

  myline<-noquote(paste0("1000","  # multiplier for recruitment, N at age, and survey number (1,1000,1000000)"))
  write(myline,paste(sara_file),append = T)  

  myline<-noquote(paste0(RecAge,"  # recruitment age used by model "))
  write(myline,paste(sara_file),append = T)  

  myline<-noquote(paste0(RecAge,"  # Age+ used for biomass estimate"))
  write(myline,paste(sara_file),append = T)  

  if (data$nareas==1) {
  myline<-noquote(paste0("Apical F","  # Fishing mortality type such as Single age or exploitation rate "))
  write(myline,paste(sara_file),append = T)  

  myline<-noquote(paste0("Age model","          # Fishing mortality source such as Model or (total catch (t))/(survey biomass (t))"))
  write(myline,paste(sara_file),append = T)  

  myline<-noquote(paste0("Age at Maximum F","         # Fishing mortality range such as: Age of maximum F"))
  write(myline,paste(sara_file),append = T)  

  } else {
  myline<-noquote(paste0("Exploitation rate","  # Fishing mortality type such as Single age or exploitation rate "))
  write(myline,paste(sara_file),append = T)  
    
  myline<-noquote(paste0("(Total catch (t))/(Total age 3+ model biomass (t))","          # Fishing mortality source such as Model or (total catch (t))/(survey biomass (t))"))
  write(myline,paste(sara_file),append = T)  
 
  myline<-noquote(paste0("All ages","         # Fishing mortality range such as: Age of maximum F"))
  write(myline,paste(sara_file),append = T)  
  }

  myline<-noquote("ALL   #FISHERYDESC -list of fisheries (ALL TWL LGL POT FIX FOR DOM TWLJAN LGLMAY POTAUG ...) ")
  write(myline,paste(sara_file),append = T)  

 myline<-noquote("#FISHERYYEAR -list years used in model")
 write(myline,paste(sara_file),append = T)   

 catch_yrs<-as.vector(data$startyr:data$endyr)
 myline<-noquote(catch_yrs)
 write(myline,paste(sara_file),append = T,ncolumns = length(catch_yrs))   

 myline<-noquote("#AGE -list ages used in model ")
 write(myline,paste(sara_file),append = T) 
 
 #Ages
 agevec<-RecAge:data$accuage
 myline<-noquote(as.vector(agevec))
 write(myline,paste(sara_file),append = T,ncolumns = length(agevec)) 
 
 #Recruitment
 Recruits<-data$recruit$pred_recr[data$recruit$era=="Main" | data$recruit$era=="Late"]
 T1<-noquote("#RECRUITMENT -Number of recruits by year (see multiplier above) ")
 write(T1,paste(sara_file),append = T)
 write(round(Recruits,1),paste(sara_file),append = T,ncolumns =  length(Recruits))
 
 #Spawning biomass
 T1<-noquote("#SPAWNBIOMASS -Spawning biomass by year in metric tons ")
 write(T1,paste(sara_file),append = T)
 
}




   #Steve's examples (Carey commented out)
   #  write_proj()
   #  write_proj(data_file="Model2_Proj.dat",data=Models[[2]],NAGES=30,FY=1977,LY=2013)
   #  write_proj(data_file="Model3_Proj.dat",data=Models[[3]],NAGES=30,FY=1977,LY=2013)R