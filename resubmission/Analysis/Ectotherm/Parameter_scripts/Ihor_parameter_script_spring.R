

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



## ECTOTHERM

rm(list = ls())

# morph, behav and water loss

#Ww_g <- 4
startday=140      #Changed to match PMUR
pct_wet <- 0.2    # % of surface area acting as a free-water exchanger
alpha_max <- 0.85 # maximum solar absorptivity
alpha_min <- 0.85 # minimum solar absorptivity
shape <- 3        # animal shape - 3 = lizard
T_RB_min <- 15.6  # min Tb at which they will attempt to leave retreat #Anamarija provided data
T_B_min <- 15.6   # min Tb at which leaves retreat to bask #Anamarija provided data
T_F_min <- 19.1     # minimum Tb at which activity occurs; from Osojnik et al (IHOR spring min_males)
T_F_max <- 31.6     # maximum Tb at which activity occurs; from Osojnik et al (IHOR spring max_males)
T_pref <- 27.8      # preferred Tb (will try and regulate to this)  from Osojnik et al (IHOR spring average_males)
CT_max <- 42.2      # critical thermal maximum (affects choice of retreat) Taken from Herrando-Perez Heat tolerance more variable
CT_min <- 5.7       # critical thermal minimum (affects choice of retreat)

mindepth <- 1     # min depth (node, 1-10) allowed  #micro$DEP #[1]   0.0   2.5   5.0  10.0  15.0  20.0  30.0  50.0 100.0 200.0
maxdepth <- 10    # max depth (node, 1-10) allowed

shade_seek <- 1   # shade seeking?
burrow <- 1       # can it burrow?
climb <- 0        # can it climb to thermoregulate?
nocturn <- 0      # nocturnal activity
crepus <- 0       # crepuscular activity
diurn <- 1        # diurnal activity




# with DEB on

getwd()


Ih.data <- readMat('../../Data/DEB_files/Iberolacerta_horvathi/Iberolacerta_horvathi/results_Iberolacerta_horvathi.mat') # load the results matlab file
Ih.data$par

par.names <- unlist(labels(Ih.data$par))

# clear possible missing parameters
if(exists("E.Hj")==TRUE){rm(E.Hj)}
if(exists("E.He")==TRUE){rm(E.He)}
if(exists("L.j")==TRUE){rm(L.j)}
if(exists("T.L")==TRUE){rm(T.L)}
if(exists("T.H")==TRUE){rm(T.H)}
if(exists("T.AL")==TRUE){rm(T.AL)}
if(exists("T.AH")==TRUE){rm(T.AH)}

for(i in 1:length(par.names)){
  assign(par.names[i], unlist(Ih.data$par[i]))
}

p.Am <- z * p.M / kap
F.m <- p.Am / kap.X # redefining F.m to max possible value
z.mult <- 1         # DEB body size scaling parameter
p.Xm <- 336.24 # had to take it manually from html
L.b <- 0.653031 # had to take it manually from html

# assign possible missing parameters
if(exists("E.Hj")==FALSE){E.Hj <- E.Hb}
if(exists("E.He")==FALSE){E.He <- E.Hb}
if(exists("L.j")==FALSE){L.j <- L.b}

# assign missing 5-par thermal response curve parameters if necessary
if(exists("T.L")==FALSE){T.L <- CT_min + 5 + 273.15} ## important!! Observations, model performs well.
if(exists("T.H")==FALSE){T.H <- CT_max + 273.15}
if(exists("T.AL")==FALSE){T.AL <- 5E04}
if(exists("T.AH")==FALSE){T.AH <- 9E04}

# overwrite nitrogenous waste indices with those of uric acid (currently ammonia by default)
n.NC <- 1
n.NH <- 4/5
n.NO <- 3/5
n.NN <- 4/5

# DEB initial state

# egg
V_init <- 3e-9
E.0 <- 8712.41 # I had to take it manually from the html
E_init <- E.0 / V_init
E_H_init <- 0
stage <- 0



# reproduction parameters
viviparous <- 0 
clutchsize <- 5 # This should be max R (Ri)?

clutch_ab = c(0.13, -4.17) #female reproductive characteristics of the Horvath's rock lizard (Iberolacerta horvathi) from Slovenia
minclutch <- 1
maxclutch <- 5
batch <- 0

photostart <- 5 # vernal equinox (March 20) is the start of the reproduction cycle; 4 changed to 5
photofinish <- 5 # summer solstice is the end of the reproduction cycle; 2 changed to 5
daylengthstart = 12.6 #check with mike
daylengthfinish = 14

photodirs=1
photodirf=0


save.image(file='../../Data/DEB_Ectotherm_params_spring/IHorvathi_pars_spring.RData')




