#------------------------------------------------------------------------
#Two Time series figures: here, need to define RexF35, RexB35, RexF40, RexB40
#-----------------------------------------------------------------------
#Plot of estimated F/F35% vs SSB/B35%, including 2 future years LastYr+1 and LastYr+2.
#grep("Recr_Initial",c$derived$Label)
phase_plot<-function(proj_results,ss3_list)
if (species == "Rex")
{
  
  F35precise = RexF35
  B35precise = RexB35
  
  F40precise = RexF40
  B40precise = RexB40
}
nyears = length(FirstYr:(LastYr))
Fline = grep(paste0("F_",FirstYr),c$derived$Label) #be sure your F reporting 
Fseries = c$derived[Fline:(Fline+nyears-1),]

#Add projection years LastYr+2 and LastYr+2 to RelF and RelSSB:
ssb_yrplus1 = Alt1.sb.df$Mean_SSB[Alt1.sb.df$Year==LastYr+1]*1000
ssb_yrplus2 = Alt1.sb.df$Mean_SSB[Alt1.sb.df$Year==LastYr+2]*1000
ssb_w_future = c(SPB_derived$Value,ssb_yrplus1,ssb_yrplus2)

F_yrplus1 = Alt1.F.df$Mean_F[Alt1.F.df$Year==LastYr+1]
F_yrplus2 = Alt1.F.df$Mean_F[Alt1.F.df$Year==LastYr+2]
F_w_future = c(Fseries$Value,F_yrplus1,F_yrplus2)


RelF = F_w_future/F35precise
RelSSB = ssb_w_future/B35precise

RelB40 = B40precise/B35precise
RelF40 = F40precise/F35precise



par(mai =c(1.02,1,0.82,0.42))
plot(RelSSB,RelF,type = "l",lwd = 2,xlim = c(0,(max(RelSSB)+1)),ylim = c(0,1.5),xlab ="(Spawning Biomass)/B35%" ,ylab = "F/F35%",cex.axis = 2,cex.lab =2)
abline(h = 1,col = "grey")
abline(v = 1,col = "grey")

#Plot OFL control rule
Bofl0 = 0.05*B40precise/B35precise
Bflat = B40precise/B35precise
segments(x0 = RelB40,y0 = 1,x1=(max(RelSSB)+1),y1 = 1,col = "firebrick3",lwd = 2,lty = 3)
segments(x0 = Bofl0 ,y0 = 0,x1=RelB40,y1 = 1,col = "firebrick3",lwd = 2, lty = 3)
segments(x0 = 0,y0=0, x1 = 0.05/B35,y1= 0,col = "firebrick3",lwd = 2, lty = 3) 

#Plot maxABC control rule
#----------------------------------------------------------------------------------
segments(x0 = RelB40,y0 = RelF40,x1=(max(RelSSB)+1),y1 = RelF40,col = "firebrick3",lwd = 2)
segments(x0 = Bofl0 ,y0 = 0,x1=RelB40,y1 = RelF40,col = "firebrick3",lwd = 2)
segments(x0 = 0,y0=0, x1 = 0.05/B35,y1= 0,col = "firebrick3",lwd = 2) 
#----------------------------------------------------------
dev.copy(png,file.path(SS3PredDir,"PhasePlot.png"))
dev.off() 
#--------------------------------------------------------------------------------
# #Plot spawning stock biomass with uncertainty - not beautiful, but working:
# par(mfrow = c(2,2), col = "black",cex.lab = 1.25, cex.axis = 1.25,lwd = 2)
# SS_plots(replist = MyOutput,plot = 3,uncertainty=Uncertainty,pdf = F,par=parlist) #Come back to make the specific plots you need
# 
# #Plot SSB vs. F with projection model stuff on there.
#  
# 


