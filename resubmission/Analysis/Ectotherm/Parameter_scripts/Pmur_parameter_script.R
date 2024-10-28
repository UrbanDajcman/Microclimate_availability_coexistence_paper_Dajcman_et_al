




using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}


using("NicheMapR", "microclima", "raster", "ggplot2", "tidyverse", "matlab", "R.matlab")



rm(list = ls())



## ECTOTHERM PARAMETERS MURALIS


# morph, behav and water loss
startday=140
pct_wet <- 0.2    # % of surface area acting as a free-water exchanger
alpha_max <- 0.85 # maximum solar absorptivity
alpha_min <- 0.85 # minimum solar absorptivity
shape <- 3        # animal shape - 3 = lizard
T_RB_min <- 17.8  # min Tb at which they will attempt to leave retreat #Anamarija provided data (changed to 17.8 from 19.8 based on table by Anamarija)
T_B_min <- 17.8   # min Tb at which leaves retreat to bask #Anamarija provided data
T_F_min <- 26.3     # minimum Tb at which activity occurs; from Osojnik et al (PMUR summer min_males)
T_F_max <- 35.5     # maximum Tb at which activity occurs; from Osojnik et al (PMUR summer max_males)
T_pref <- 31.2      # preferred Tb (will try and regulate to this); from Osojnik et al. (PMUR summer average_males)
CT_max <- 43      # critical thermal maximum (affects choice of retreat)
CT_min <- 5.1     # critical thermal minimum (affects choice of retreat)

mindepth <- 1     # min depth (node, 1-10) allowed
maxdepth <- 10    # max depth (node, 1-10) allowed

shade_seek <- 1   # shade seeking?
burrow <- 1       # can it burrow?
climb <- 0        # can it climb to thermoregulate?
nocturn <- 0      # nocturnal activity
crepus <- 0       # crepuscular activity
diurn <- 1        # diurnal activity



getwd()

Pm.data <- readMat('../../Data/DEB_files/Podarcis_muralis/Podarcis_muralis_Castanet/results_Podarcis_muralis_Castanet.mat') # load the allstat file

Pm.data$par

par.names <- unlist(labels(Pm.data$par))

# clear possible missing parameters
if(exists("E.Hj")==TRUE){rm(E.Hj)}
if(exists("E.He")==TRUE){rm(E.He)}
if(exists("L.j")==TRUE){rm(L.j)}
if(exists("T.L")==TRUE){rm(T.L)}
if(exists("T.H")==TRUE){rm(T.H)}
if(exists("T.AL")==TRUE){rm(T.AL)}
if(exists("T.AH")==TRUE){rm(T.AH)}

for(i in 1:length(par.names)){
  assign(par.names[i], unlist(Pm.data$par[i]))
}

p.Am <- z * p.M / kap
F.m <- p.Am / kap.X # redefining F.m to max possible value
z.mult <- 1         # DEB body size scaling parameter
p.Xm <- 594.863 # had to take it manually from html
L.b <- 0.527789 # had to take it manually from html

# assign possible missing parameters
if(exists("E.Hj")==FALSE){E.Hj <- E.Hb}
if(exists("E.He")==FALSE){E.He <- E.Hb}
if(exists("L.j")==FALSE){L.j <- L.b}

# assign missing 5-par thermal response curve parameters if necessary
if(exists("T.L")==FALSE){T.L <- CT_min + 5 + 273.15}
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
E.0 <- 	6467.47 # I had to take it manually from the html
E_init <- E.0 / V_init
E_H_init <- 0
stage <- 0



# reproduction parameters

viviparous <- 0 
clutchsize <- 5 # This should be max R (Ri)? AVERAGE
clutch_ab = c(0.15, -5.50) #ref: Mechanisms of regulation and maintenance of color polymorphism in the common wall lizard (Podarcis muralis)
minclutch <- 2 #changed
maxclutch <- 8 #changed
batch <- 0 #changed from 1

photostart <- 5 # vernal equinox (March 20) is the start of the reproduction cycle; 4 changed to 5
photofinish <- 5 # summer solstice is the end of the reproduction cycle; 2 changed to 5
daylengthstart = 12.6 #check with mike
daylengthfinish = 14

photodirs=1
photodirf=0


getwd()
save.image(file='../../Data/DEB_Ectotherm_params/PMuralis_pars.RData')



