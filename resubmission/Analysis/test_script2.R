

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}


using("NicheMapR", "microclima", "raster", "ggplot2", "tidyverse", "R.matlab", "matlab")



setwd("C:/1_Urban/1_Doktorat/MODELIRANJE/NICHEMAPR/NichemapR_GITHUB_PAPER_1/IHOR_PMUR_NichemapR/Analysis/Ectotherm/")


load("../../Results/ECTOTHERM_results/results_all_ecto2.Rda")





results_all

ecto$yearsout


#Bilpa micro


load("../../Results/microNCEP/micro_ncep_Bilpa_sint.RData")
bilpa_micro <- micro


#Fridrihstajn

load("../../Results/microNCEP/micro_ncep_Fridrihstajn_aloPmur.RData")
fridrihstajn_micro <- micro




#Bilpa ecto

load("../../Results/ecto_files/ecto_Ihor_Bilpa.Rda")
bilpa_ecto <- ecto


#Fridrihstajn ecto

load("../../Results/ecto_files/ecto_Ihor_Fridrihstajn.Rda")
fridrihstajn_ecto <- ecto



plot(as.data.frame(bilpa_micro$metout)$TALOC, type="l")

plot(as.data.frame(fridrihstajn_micro$metout)$TALOC, type="l")


plot(as.data.frame(bilpa_ecto$environ)$TC, type="l")

plot(as.data.frame(fridrihstajn_ecto$environ)$TC, type="l")
abline(v=180*24)

#BILPA

bilpa_ecto <- ecto

bilpa_environ <- as.data.frame(bilpa_ecto$environ)
bilpa_environ

bilpa_debout <- as.data.frame(bilpa_ecto$debout)
bilpa_debout




plot(bilpa_debout$STAGE, type="l")
abline(v=180*24)
abline(v=h_die_bilpa)
abline(v=205*24)


plot(bilpa_debout$P_SURV, type="l")
abline(v=h_die_bilpa)


min(bilpa_debout$P_SURV)

which(bilpa_debout$P_SURV==min(bilpa_debout$P_SURV))

h_die_bilpa <- which(bilpa_debout$P_SURV==min(bilpa_debout$P_SURV))
h_start_ihor <- 180*24



bilpa_ecto$yearsout

bilpa_environ_2 <- bilpa_environ[h_start_ihor:h_die_bilpa,]
bilpa_environ_2


min(bilpa_environ_2$TC)
plot(bilpa_environ_2$TC, type="l")


bilpa_debout_2 <- bilpa_debout[h_start_ihor:h_die_bilpa,]
bilpa_debout_2



#Fridrihstajn

fridrihstajn_ecto <- ecto


plot(as.data.frame(fridrihstajn_ecto$metout)$TALOC[1:h_die_fridrihstajn], type="l")


fridrihstajn_environ <- as.data.frame(fridrihstajn_ecto$environ)
fridrihstajn_environ

fridrihstajn_debout <- as.data.frame(fridrihstajn_ecto$debout)
fridrihstajn_debout




plot(fridrihstajn_debout$STAGE, type="l")
abline(v=180*24)
abline(v=h_die_fridrihstajn)
abline(v=205*24)


plot(fridrihstajn_debout$P_SURV, type="l")
abline(v=h_die_fridrihstajn)


min(fridrihstajn_debout$P_SURV)

which(fridrihstajn_debout$P_SURV==min(fridrihstajn_debout$P_SURV))

h_die_fridrihstajn <- which(fridrihstajn_debout$P_SURV==min(fridrihstajn_debout$P_SURV))
h_start_ihor <- 180*24






fridrihstajn_ecto$yearsout

fridrihstajn_environ_2 <- fridrihstajn_environ[h_start_ihor:h_die_fridrihstajn,]
fridrihstajn_environ_2


min(fridrihstajn_environ_2$TC)
plot(fridrihstajn_environ_2$TC, type="l")
abline(v=(223-180)*24)


fridrihstajn_debout_2 <- fridrihstajn_debout[h_start_ihor:h_die_fridrihstajn,]
fridrihstajn_debout_2


#DEB plot fridrihstajn

immature_x <-(which(fridrihstajn_debout_2$E_H > E.Hp)[1] - which(fridrihstajn_debout_2$E_H > E.Hp)[1] * .5) / 24
adult_x <- which(fridrihstajn_debout_2$E_H > E.Hp)[1] * 1.2 / 24


deb_ecto_plot <- ggplot(fridrihstajn_debout_2) +
  geom_line(aes(x=seq(1,length(fridrihstajn_debout_2$WETMASS)), y=fridrihstajn_debout_2$WETMASS), color="pink", size=1)+
  geom_line(aes(x=seq(1,length(fridrihstajn_debout_2$WETMASS)), y=fridrihstajn_debout_2$V), color="green3", size=1)+
  geom_line(aes(x=seq(1,length(fridrihstajn_debout_2$WETMASS)), y=fridrihstajn_debout_2$WETMASS-fridrihstajn_debout_2$WETGONAD), color="brown", size=1)+
  geom_line(aes(x=seq(1,length(fridrihstajn_debout_2$WETMASS)), y=fridrihstajn_debout_2$WETMASS-fridrihstajn_debout_2$WETGONAD-fridrihstajn_debout_2$WETGUT), color="grey", size=1)+
  
  

  
  
  
 # scale_x_continuous("Year", breaks=seq(0,length(fridrihstajn_debout_2$WETMASS),8765), limits = c(0, 87650), label = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))+
 # scale_y_continuous("Wet mass (g)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

deb_ecto_plot





#DEB plot bilpa

immature_x <-(which(bilpa_debout$E_H > E.Hp)[1] - which(bilpa_debout$E_H > E.Hp)[1] * .5) / 24
adult_x <- which(bilpa_debout$E_H > E.Hp)[1] * 1.2 / 24


deb_ecto_plot <- ggplot(bilpa_debout) +
  geom_line(aes(x=seq(1,length(bilpa_debout$WETMASS)), y=bilpa_debout$WETMASS), color="pink", size=1)+
  geom_line(aes(x=seq(1,length(bilpa_debout$WETMASS)), y=bilpa_debout$V), color="green3", size=1)+
  geom_line(aes(x=seq(1,length(bilpa_debout$WETMASS)), y=bilpa_debout$WETMASS-bilpa_debout$WETGONAD), color="brown", size=1)+
  geom_line(aes(x=seq(1,length(bilpa_debout$WETMASS)), y=bilpa_debout$WETMASS-bilpa_debout$WETGONAD-bilpa_debout$WETGUT), color="grey", size=1)+
  
  
  
  
  
  
  
  # scale_x_continuous("Year", breaks=seq(0,length(bilpa_debout$WETMASS),8765), limits = c(0, 87650), label = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))+
  # scale_y_continuous("Wet mass (g)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

deb_ecto_plot


rm(list = ls())



# MICRCO GLOBAL TEST

longlat <- c(146.77, -19.29) # Townsville, northern Australia
nyear <- 5
ndays <- 365*5

micro <- micro_global(loc = longlat, timeinterval = 365, nyear = nyear)

# ECTOTHERM TEST


longlat <- c(146.77, -19.29) # Townsville, northern Australia
nyear <- 5
ndays <- 365*5

micro <- micro_global(loc = longlat, timeinterval = 365, nyear = nyear)

ecto<- ectotherm(DEB=1)

ecto_deb <- as.data.frame(ecto$debout)
ecto_deb

#DEB plot test

immature_x <-(which(ecto_deb$E_H > E.Hp)[1] - which(ecto_deb$E_H > E.Hp)[1] * .5) / 24
adult_x <- which(ecto_deb$E_H > E.Hp)[1] * 1.2 / 24


deb_ecto_plot <- ggplot(ecto_deb) +
  geom_line(aes(x=seq(1,length(ecto_deb$WETMASS)), y=ecto_deb$WETMASS), color="pink", size=1)+
  geom_line(aes(x=seq(1,length(ecto_deb$WETMASS)), y=ecto_deb$V), color="green3", size=1)+
  geom_line(aes(x=seq(1,length(ecto_deb$WETMASS)), y=ecto_deb$WETMASS-ecto_deb$WETGONAD), color="brown", size=1)+
  geom_line(aes(x=seq(1,length(ecto_deb$WETMASS)), y=ecto_deb$WETMASS-ecto_deb$WETGONAD-ecto_deb$WETGUT), color="grey", size=1)+
  
  
  # scale_x_continuous("Year", breaks=seq(0,length(ecto_deb$WETMASS),8765), limits = c(0, 87650), label = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))+
  # scale_y_continuous("Wet mass (g)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

deb_ecto_plot














