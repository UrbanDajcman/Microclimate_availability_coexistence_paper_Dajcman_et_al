




using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}


using("NicheMapR", "microclima", "raster", "ggplot2", "tidyverse", "matlab")




rm(list = ls())

setwd("C:/1_Urban/1_Doktorat/MODELIRANJE/NICHEMAPR/NichemapR_GITHUB_PAPER_1/IHOR_PMUR_NichemapR/Analysis/Ectotherm/")



micro_nceps <-list.files(path = "../../Results/microNCEP/", pattern = "micro_ncep", full.names = TRUE)
micro_nceps


Species_params <- list.files(path = "../../Data/DEB_Ectotherm_params_spring/",pattern = "pars", full.names = TRUE)
Species_params




loc <- c()
type <- c()



for(i in 1:length(micro_nceps)){
  loc_i <- strsplit(micro_nceps, split = "_")[[i]][3]
  temp <- strsplit(micro_nceps, split = "_")[[i]]
  temp2 <-  temp [length(temp)]
  type_i <- substr(temp2, 1, nchar(temp2) - 6)
  loc <- c(loc, loc_i)
  type <- c(type, type_i)
}
  

loc
type



Species =rep(c("Ihor", "Pmur"),each = 15 )
Species

#yearout
DEVTIME
BIRTHDAY 
MONMATURE 
MONREPRO 
FECUNDITY 
CLUTCHES 
ANNUALACT 
LifeSpan 

#yearsout
Forage
Bsk
Year



results_all <- data.frame(Locality = rep(loc,2),  Sintopy = rep(type,2), Species =rep(c("Ihor", "Pmur"),each = 15 ), devtime = NA, birthday = NA, MonMature = NA,
                          MonRepro=NA, Fecundity=NA, Clutches=NA, AnnualAct=NA, LifeSpan=NA, Forage=NA, Bask =NA, Aspect =NA, Slope=NA, Elevation=NA, MaxTB=NA, MinTB=NA, MeanTB=NA, 
                          Year=NA, Years_repro=NA, NO_Snow=NA, mean_MAX_SOLR=NA, MaxWgt=NA, MaxLen=NA, egg_dev=NA, MeanTALOC =NA, MeanVLOC= NA, MeanRHLOC=NA )

results_all

#micro$DEP
#[1]   0.0   2.5   5.0  10.0  15.0  20.0  30.0  50.0 100.0 200.0


for (s in 1:length(micro_nceps)){
  load(micro_nceps[s])
  
  results_all[results_all$Locality == loc[s], "Aspect" ] <- micro$ASPECT
  results_all[results_all$Locality == loc[s], "Slope" ] <- micro$SLOPE
  results_all[results_all$Locality == loc[s], "Elevation" ] <- micro$elev
  
  
  
  
  for(j in unique(results_all$Species)){
    ifelse(j == "Ihor", load(Species_params[1]), load(Species_params[2]))
    
    
    maxdepth 
    
    ecto<-ecto<-ectotherm(DEB=1,
                          startday = startday,
                          viviparous=viviparous,
                          #clutchsize = clutchsize,
                          clutch_ab = clutch_ab,
                          minclutch = minclutch,
                          maxclutch = maxclutch,
                          batch = batch,
                          z.mult=z.mult,
                          shape=shape,
                          alpha_max=alpha_max,
                          alpha_min=alpha_min,
                          T_F_min=T_F_min,
                          T_F_max=T_F_max,
                          T_B_min=T_B_min,
                          T_RB_min=T_RB_min,
                          T_pref=T_pref,
                          CT_max=CT_max,
                          CT_min=CT_min,
                          diurn=diurn,
                          nocturn=nocturn,
                          crepus=crepus,
                          shade_seek=shade_seek,
                          burrow=burrow,
                          climb=climb,
                          mindepth=mindepth,
                          maxdepth=maxdepth,
                          pct_wet=pct_wet,
                          z=z*z.mult,
                          del_M=del.M,
                          p_Xm=p.Xm,
                          kap_X=kap.X,
                          v=v/24,
                          kap=kap,
                          p_M=p.M/24,
                          E_G=E.G,
                          kap_R=kap.R,
                          k_J=k.J/24,
                          E_Hb=E.Hb*z.mult^3,
                          E_Hj=E.Hj*z.mult^3,
                          E_Hp=E.Hp*z.mult^3,
                          E_He=E.He*z.mult^3,
                          h_a=h.a/(24^2),
                          s_G=s.G,
                          T_REF=T.ref,
                          T_A=T.A,
                          T_AL=T.AL,
                          T_AH=T.AH,
                          T_L=T.L,
                          T_H=T.H,
                          E_0=E.0*z.mult^4,
                          f=f,
                          d_V=d.V,
                          d_E=d.E,
                          d_Egg=d.E,
                          mu_X=mu.X,
                          mu_E=mu.E,
                          mu_V=mu.V,
                          mu_P=mu.P,
                          kap_X_P=kap.P,
                          n_X=c(n.CX,n.HX,n.OX,n.ON),
                          n_E=c(n.CE,n.HE,n.OE,n.OE),
                          n_V=c(n.CV,n.HV,n.OV,n.OV),
                          n_P=c(n.CP,n.HP,n.OP,n.OP),
                          n_M_nitro=c(n.NC,n.NH,n.NO,n.NN),
                          L_b=L.b,
                          V_init=V_init,
                          E_init=E_init,
                          E_H_init=E_H_init,
                          stage=stage,
                          photostart = photostart,
                          photofinish = photofinish,
                          daylengthstart = daylengthstart,
                          daylengthfinish = daylengthfinish,
                          photodirs=photodirs,
                          photodirf=photodirf,
                          soilnode = 3)
    
    
    
    yearout<-as.data.frame(ecto$yearout)
    yearsout<-as.data.frame(ecto$yearsout)
    environ<-as.data.frame(ecto$environ)
    debout<-as.data.frame(ecto$debout)
    metout<-as.data.frame(ecto$metout)
    
    #taking only snow days, omitting egg stage
    metout_snow <- metout[debout$STAGE != 0,]
    
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "devtime" ] <- yearout$DEVTIME
    results_all[results_all$Species == j & results_all$Locality == loc[s], "birthday" ] <- yearout$BIRTHDAY
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MonMature" ] <- yearout$MONMATURE
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MonRepro" ] <- yearout$MONREPRO
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Fecundity" ] <- max(yearsout$Fec)
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Clutches" ] <- max(yearsout$Clutch)
    results_all[results_all$Species == j & results_all$Locality == loc[s], "AnnualAct" ] <- yearout$ANNUALACT
    results_all[results_all$Species == j & results_all$Locality == loc[s], "LifeSpan" ] <- max(yearsout$YEAR)
    
    #add maxwgt and maxlen
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MaxWgt" ] <- max(yearsout$MaxWgt)
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MaxLen" ] <- max(yearsout$MaxLen)
    
    #add egg_dev time
    results_all[results_all$Species == j & results_all$Locality == loc[s], "egg_dev" ] <- max(yearsout$tStg1) - max(yearsout$tEgg)
    
    
    #number of snow days
    results_all[results_all$Species == j & results_all$Locality == loc[s], "NO_Snow" ] <- sum(metout_snow$SNOWDEP==0)/24
    
    #solar radiation - mean of daily maximum radiation during periods without snow
    results_all[results_all$Species == j & results_all$Locality == loc[s], "mean_MAX_SOLR" ] <- mean(with(metout_snow, tapply(SOLR, DOY, max)))
    
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Rel_hum" ] <- mean(environ$RELHUM)
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Solar" ] <- mean(environ$SOLAR)
    
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MinTB" ] <- min((environ$TC))  
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MaxTB" ] <- max((environ$TC)) 
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MeanTB" ] <- mean(environ$TC)
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Forage" ] <- yearsout$Forage[length(which(yearsout$Forage != 0))]
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Bask" ] <- yearsout$Bsk[length(which(yearsout$Bsk != 0))]
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Year" ] <- yearsout$YEAR[length(which(yearsout$YEAR != 0))]
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "Years_repro" ] <- length(which(yearsout$Clutch != 0))
    
    
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MeanTALOC" ] <- mean(metout$TALOC)
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MeanVLOC" ] <- mean(metout$VLOC)
    results_all[results_all$Species == j & results_all$Locality == loc[s], "MeanRHLOC" ] <- mean(metout$RHLOC)
    
    
    #yearsout$YEAR[length(which(yearsout$Clutch != 0))]  add to get number of years after having first clutch
    
  
    
    save(ecto, file=paste0("../../Results/ecto_files/ecto_spring", j, "_", loc[s], ".Rda"))
    
    
    
    
    
    
    
    
  }
  
}





results_all

yearsout
environ





#save(results_all, file="../../Results/ECTOTHERM_results/results_all_ecto_springTP.Rda")



library(xlsx)

#write.xlsx(results_all, file="../../Results/ECTOTHERM_results/results_all_ecto_springTP.xlsx", sheetName = "Ecto_rez")

