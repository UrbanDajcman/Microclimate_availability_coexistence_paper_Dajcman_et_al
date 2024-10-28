

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



ecto_files <-list.files(path = '../../Results/ecto_files/', pattern = "ecto", full.names = TRUE)
ecto_files


ecto_names <-list.files(path = '../../Results/ecto_files/', pattern = "ecto", full.names = FALSE)
ecto_names <- tools::file_path_sans_ext(ecto_names) 
ecto_names

Species_params <- list.files(path = "../../Data/DEB_Ectotherm_params/",pattern = "pars", full.names = TRUE)
Species_params

Species <- c("Ihor", "Pmur")
Species


ecto_files[1]





for (s in 1:length(ecto_files)){
  
  
  load(ecto_files[s])
  ifelse(grepl("Ihor", ecto_files[1], fixed = TRUE), load(Species_params[1]),  load(Species_params[2]))
  
  
  
  
  yearout <- as.data.frame(ecto$yearout)
  yearsout <- as.data.frame(ecto$yearsout)
  environ <- as.data.frame(ecto$environ)
  debout <- as.data.frame(ecto$debout)
  
  
  
  
  immature_x <-(which(debout$E_H > E.Hp)[1] - which(debout$E_H > E.Hp)[1] * .5) / 24
  adult_x <- which(debout$E_H > E.Hp)[1] * 1.2 / 24
  
  
  deb_ecto_plot <- ggplot(debout) +
    geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$WETMASS), color="pink", size=1)+
    geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$V), color="green3", size=1)+
    geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$WETMASS-debout$WETGONAD), color="brown", size=1)+
    geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$WETMASS-debout$WETGONAD-debout$WETGUT), color="grey", size=1)+
    
    geom_vline(xintercept = seq(1,length(debout$WETMASS))[which(debout$E_H>E.Hb)[1]], linetype="dashed", color="black", size=1)+
    geom_vline(xintercept = seq(1,length(debout$WETMASS))[which(debout$E_H>E.Hp)[1]], linetype="dashed", color="black", size=1)+
    
    geom_text(x=0,y= max(debout$WETMASS) * 0.5, label= "embryo")+
    geom_text(x=immature_x*24,y= max(debout$WETMASS) * 0.7, label= "immature")+
    geom_text(x=adult_x*24,y= max(debout$WETMASS) * 1, label= "adult")+
    
    geom_text(x=8765*9,y= max(debout$WETMASS) * 0.8, label= "repro buffer", color = "pink")+
    geom_text(x=8765*9,y= max(debout$WETMASS) * 0.5, label= "food in gut", color ="brown")+
    geom_text(x=8765*9,y= max(debout$WETMASS) * 0.45, label= "reserve", color = "grey")+
    geom_text(x=8765*9,y= max(debout$WETMASS) * 0.2, label= "structure", color = "green3")+
    geom_text(x=8765*9,y= max(debout$WETMASS) * 1, label= paste0(ecto_names[s]), color = "black", size=5)+
    
    
    
    
    
    scale_x_continuous("Year", breaks=seq(0,length(debout$WETMASS),8765), limits = c(0, 87650), label = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))+
    scale_y_continuous("Wet mass (g)")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
  
  deb_ecto_plot
  
  ggsave(file=paste0("../../Figures/DEB_ecto_plots/", "DEB_plot_", ecto_names[s], ".png"), plot=deb_ecto_plot, units="cm", width = 30, height = 10)
    
    
    
    
    
  
  
}






load(file='../../Results/ecto_files/ecto_ihor_Iski.Rda')

#Load params for species
load(file="../../Data/Deb_Ectotherm_params/IHorvathi_pars.RData")



yearout <- as.data.frame(ecto$yearout)
yearsout <- as.data.frame(ecto$yearsout)
environ <- as.data.frame(ecto$environ)
debout <- as.data.frame(ecto$debout)

yearout
yearsout
environ
debout



forage <- subset(environ, ACT == 2) # get foraging hours
bask <- subset(environ, ACT == 1) # get basking hours
night <- subset(environ, SOLAR == 0) # get night hours

with(night, plot(TIME ~ DOY, ylab = "Hour of Day", xlab = "Day of Year", pch = 15, cex = 2, 
                 col = 'dark blue')) # nighttime hours
with(bask, points(TIME ~ DOY, pch = 15, cex = 2, col = 'light blue')) # basking Tbs
with(forage, points(TIME ~ DOY, pch = 15, cex = 2, col = 'orange')) # foraging Tbs



immature_x <-(which(debout$E_H > E.Hp)[1] - which(debout$E_H > E.Hp)[1] * .5) / 24
adult_x <- which(debout$E_H > E.Hp)[1] * 1.2 / 24




length(debout$WETMASS)

deb_ecto_plot <- ggplot(debout) +
  geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$WETMASS), color="pink", size=1)+
  geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$V), color="green3", size=1)+
  geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$WETMASS-debout$WETGONAD), color="brown", size=1)+
  geom_line(aes(x=seq(1,length(debout$WETMASS)), y=debout$WETMASS-debout$WETGONAD-debout$WETGUT), color="grey", size=1)+
  
  geom_vline(xintercept = seq(1,length(debout$WETMASS))[which(debout$E_H>E.Hb)[1]], linetype="dashed", color="black", size=1)+
  geom_vline(xintercept = seq(1,length(debout$WETMASS))[which(debout$E_H>E.Hp)[1]], linetype="dashed", color="black", size=1)+
  
  geom_text(x=0,y= max(debout$WETMASS) * 0.5, label= "embryo")+
  geom_text(x=immature_x*24,y= max(debout$WETMASS) * 0.7, label= "immature")+
  geom_text(x=adult_x*24,y= max(debout$WETMASS) * 1, label= "adult")+
  
  geom_text(x=8765*9,y= max(debout$WETMASS) * 0.8, label= "repro buffer", color = "pink")+
  geom_text(x=8765*9,y= max(debout$WETMASS) * 0.5, label= "food in gut", color ="brown")+
  geom_text(x=8765*9,y= max(debout$WETMASS) * 0.45, label= "reserve", color = "grey")+
  geom_text(x=8765*9,y= max(debout$WETMASS) * 0.2, label= "structure", color = "green3")+
  
  
  
  scale_x_continuous("Year", breaks=seq(0,length(debout$WETMASS),8765), limits = c(0, 87650), label = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))+
  scale_y_continuous("Wet mass (g)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

deb_ecto_plot


ggsave("deb_ecto_plot.pdf", plot=deb_ecto_plot, path = "../../Figures", units="cm", width=30, height=15, dpi=300 )
