

remove.packages("NicheMapR")

devtools::install_github("mrke/NicheMapR")

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}



using("NicheMapR", "microclima", "raster", "ggplot2", "tidyverse", "matlab", "reshape2")




rm(list = ls())



load(file="../../Results/ecto_files/ecto_Pmur_Ribnica.Rda")


metout <- as.data.frame(ecto$metout)
metout

shadmet <- as.data.frame(ecto$shadmet)
shadmet



mean_snow <- metout %>%
  group_by(DOY) %>%
  summarise_at(vars(SNOWDEP), list(Snow_cover = mean)) 

mean_snow <- filter(mean_snow, Snow_cover > 0)


plot(mean_snow$DOY, mean_snow$Snow_cover)

#shade snow 

mean_snow_shd <- shadmet %>%
  group_by(DOY) %>%
  summarise_at(vars(SNOWDEP), list(Snow_cover = mean)) 

mean_snow_shd <- filter(mean_snow_shd, Snow_cover > 0)


plot(mean_snow_shd$DOY, mean_snow_shd$Snow_cover)


ecto$metout
ecto$debout
ecto$yearsout
ecto$yearout

#get n. days without snow

results_all[results_all$Species == j & results_all$Locality == loc[s], "Forage" ] <- metout_snow$SNOWDEP

metout_snow <- metout[debout$STAGE != 0,]
metout_snow


#days without snow

sum(metout_snow$SNOWDEP==0)/24

plot(metout_snow$SNOWDEP)


#mean daily maximum radiation values

mean(with(metout, tapply(SOLR, DOY, max)))

#max daily solar
max(metout$SOLR)

#egg development time
egg_dev <- max(yearsout$tStg1) - max(yearsout$tEgg)

egg_dev

#alternative version

yearsout

((208-150) + 2*365 + (238-208))/30

((208-150) + (365-208) +2*365 +238)/30.5

((208-150) + (365-208) +365 +222)/30.5

(2*365 + 222)/30.5

(2*365 + 222)/30.5 +(365*5)/30.5

yearout


debout <- as.data.frame(ecto$debout)

debout



plot(debout$DAY, debout$STAGE)

plot(debout$DAY, debout$WETMASS)

plot(debout$DAY, debout$WETGONAD)

plot(debout$DAY, debout$H_S)

plot(debout$DAY, debout$p_A, type="l")
abline(v=150)


plot(debout$DAY, debout$STAGE, type="l")
abline(v=208)


yearsout

plot(debout$STAGE, type="l")
abline(v=(150+58)*24)
abline(v=(3243)*24)
abline(v=(3243-58)*24)
abline(v=(3243-208)*24)



yearout

plot(debout$DAY, debout$P_SURV)


yearout <- as.data.frame(ecto$yearout)

yearout


yearsout <- as.data.frame(ecto$yearsout)

yearsout

debout

yearsout

plot(debout$STAGE)
abline(v=3243)

masbal <- as.data.frame(ecto$masbal)

masbal

plot(masbal$H2OBal_g, type="l")

O2_cons <- masbal %>%
  group_by(DOY) %>%
  summarise_at(vars(O2_ml), list(O2_cons = mean)) 

CO2_prod <- masbal %>%
  group_by(DOY) %>%
  summarise_at(vars(CO2_ml ), list(CO2_prod = mean))

H2OResp_g   <- masbal %>%
  group_by(DOY) %>%
  summarise_at(vars(H2OResp_g   ), list(H2OResp_g   = mean))


masbal$H2OResp_g




plot(O2_cons$DOY, O2_cons$O2_cons)
plot(CO2_prod$DOY, CO2_prod$CO2_prod)


ggplot()+
  geom_line(data=O2_cons, aes(DOY, O2_cons ), color="blue")+
  geom_line(data=CO2_prod, aes(DOY, CO2_prod), color="red")+
  geom_line(data=H2OResp_g , aes(DOY, H2OResp_g ), color="green")+
  theme_minimal()

enbal <- as.data.frame(ecto$enbal)

plot(enbal$DAY, enbal$QSOL)

environ <- as.data.frame(ecto$environ)

environ$DEP

plot(environ$DAY, environ$TC)

dates <- micro$dates

masbal <- cbind(dates, masbal)
enbal <- cbind(dates, enbal)

masbal

plot(masbal$dates, masbal$H2OResp_g)













#ecto test
rm(list = ls())

getwd()

load("../../Data/DEB_Ectotherm_params/PMuralis_pars.RData")

load("../../Results/microNCEP/micro_ncep_Stefan_aloPmur.RData")


startday = 150
maxdepth  <- 8
aestivate=0


ecto<-ectotherm(DEB=1,
                startday = startday,
                viviparous=viviparous,
                clutchsize = clutchsize,
                minclutch = minclutch,
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
                soilnode = 4)




yearout<-as.data.frame(ecto$yearout)
yearsout<-as.data.frame(ecto$yearsout)
environ<-as.data.frame(ecto$environ)
debout<-as.data.frame(ecto$debout)
metout<-as.data.frame(ecto$metout)

masbal<-as.data.frame(ecto$masbal)
enbal<-as.data.frame(ecto$enbal)




yearout
yearsout
debout
environ
metout



plot(metout$DOY, metout$SNOWDEP, type="l")

plot(debout$STAGE, type="l")
abline(v=150*24)


plot(debout$P_SURV, type="l")
abline(v=150*24)

plot(environ$TC, type="l")
abline(v=150*24)
abline(v=(150+365)*24)
abline(h=5.1)

plot(environ$DEP, type="l")
abline(v=150*24)

plot(metout$TALOC, type="l")
plot(metout$TAREF, type="l")

dstart <- "01/01/1995"
dfinish <- "31/12/2010"

tzone<-paste("Etc/GMT+",0,sep="")
dates<-seq(as.POSIXct(dstart, format="%d/%m/%Y",tz=tzone)-3600*12, as.POSIXct(dfinish, format="%d/%m/%Y",tz=tzone)+3600*11, by="hours")

soil <- as.data.frame(ecto$soil)
soilmoist <- as.data.frame(ecto$soilmoist)



metout <- cbind(dates,metout)
soil <- cbind(dates,soil)
soilmoist <- cbind(dates, soilmoist)

environ <- cbind(dates, environ)
masbal <- cbind(dates, masbal)
enbal <- cbind(dates, enbal)


#DEB plot
ndays<- max(masbal$DAY)

par(mfrow = c(1,1))
plot(seq(1, ndays * 24) / 24, debout$WETMASS, type = 'l', xlab = 'date', 
     ylab = paste0('wet mass (g)'), col = 'pink', lwd = 2, 
     ylim = c(0, max(debout$WETMASS)))
points(seq(1, ndays * 24) / 24, debout$V, type = 'l', xlab = 'date', 
       ylab = paste0('wet mass (g)'), col = 'dark green', lwd = 2)
points(seq(1, ndays * 24) / 24, debout$WETMASS-debout$WETGONAD, type = 'l', 
       lwd = 2, col = 'brown')
points(seq(1, ndays * 24) / 24, debout$WETMASS-debout$WETGONAD-debout$WETGUT,
       type = 'l', lwd = 2, col = 'grey')
abline(v = (seq(1, ndays * 24) / 24)[which(debout$E_H>E.Hb)[1]], lty = 2, col = 'grey')
abline(v = (seq(1, ndays * 24) / 24)[which(debout$E_H>E.Hp)[1]], lty = 2, col = 'grey')
legend(4000, max(debout$WETMASS) * 0.3, 
       c('repro. buffer', 'food in gut', 'reserve', 'structure'), lty = rep(1, 4), 
       col = c("pink", "brown", "grey", "dark green"), bty = 'n')
text(0, max(debout$WETMASS) * 1, labels = "embryo", cex = 0.85)
text((which(debout$E_H > E.Hp)[1] - which(debout$E_H > E.Hp)[1] * .5) / 24 ,
     max(debout$WETMASS) * 1, labels = "immature", cex = 0.85)
text(which(debout$E_H > E.Hp)[1] * 1.2 / 24, max(debout$WETMASS) * 1,
     labels = "adult", cex = 0.85)

