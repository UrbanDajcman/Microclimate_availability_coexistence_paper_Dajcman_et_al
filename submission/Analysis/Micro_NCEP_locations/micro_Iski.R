


#install.packages("C:/Urban/Doktorat/MODELIRANJE/NICHEMAPR/microclima-master.zip", repos = NULL, 
#type = "win.binary")

#devtools::install_github("ilyamaclean/microclima")

#install.packages("RNetCDF")

library(NicheMapR)



rm(list = ls())


## MICROCLIMATE Iski vintgar 45.9069 14.494



#################################
# micro_ncep 

library(microclima)
library(raster)


dstart <- "01/01/1995"
dfinish <- "31/12/2010"
minshade <- 0
maxshade <- 90
Thcond <- 2.5
SpecHeat <- 870
Density <- 2.56
BulkDensity <- 1.3
windfac <- 1
REFL <- 0.2
cap <- 1
SLE <- 0.95
warm <- 0
Usrhyt <- 0.01
clearsky <- FALSE

lon <- 14.494
lat <- 45.9069




dem <- microclima::get_dem(r = NA, lat = lat, lon = lon, resolution = 30, zmin = -20, xdims = 100, ydims = 100)

elev <- raster::extract(dem, cbind(lon, lat))[1]
xy <- data.frame(x = lon, y = lat)
sp::coordinates(xy) = ~x + y
sp::proj4string(xy) = "+init=epsg:4326"
xy <- as.data.frame(sp::spTransform(xy, raster::crs(dem)))
slope <- raster::terrain(dem, unit = "degrees")
slope <- raster::extract(slope, xy)
aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees")
aspect <- raster::extract(aspect, xy)
ha36 <- 0
for (i in 0:35) {
  har <- microclima::horizonangle(dem, i * 10, raster::res(dem)[1])
  ha36[i + 1] <- atan(raster::extract(har, xy)) * (180/pi)
}
hori <- spline(x = ha36, n = 24, method =  'periodic')$y
hori[hori < 0] <- 0
hori[hori > 90] <- 90
soilgrids <- 1
spatial = NA

#spatial <- 'C:/Urban/Doktorat/MODELIRANJE/NICHEMAPR/NCEP/ncep_time'
#spatial <- 'C:/Urban/Doktorat/MODELIRANJE/NICHEMAPR/NCEP/'
ERR <- 3.5
DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200)

micro <- micro_ncep(SLE = SLE, warm = warm, soilgrids = soilgrids, dstart = dstart, dfinish = dfinish,
                    Usrhyt = Usrhyt, slope = slope, aspect = aspect, DEP = DEP, REFL = REFL,
                    hori = hori, minshade = minshade, maxshade = maxshade,
                    loc = c(lon, lat), runshade = 1, run.gads = 1, snowmodel = 1,
                    BulkDensity =  BulkDensity, cap = cap,
                    Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                    windfac = windfac, spatial = spatial, ERR = ERR, dem = dem)


metout <- as.data.frame(micro$metout)
shadmet <- as.data.frame(micro$shadmet)
str(metout)

dates <- micro$dates

plot(dates, metout$TALOC, type='l')
plot(dates, metout$TAREF, type='l')

plot(dates, metout$RHLOC, type='l')
plot(dates, metout$RH, type='l')

plot(dates, metout$VLOC, type='l')
plot(dates, metout$VREF, type='l')


plot(dates, metout$FROST, type='l')
plot(dates, metout$SNOWDEP, type='l')


save(micro, file='../Results/microNCEP/micro_ncep_Iski_sint.RData')
load('micro_ncep_Iski_sint.RData')









## ECTOTHERM

# only biophys

# morph, behav and water loss
Ww_g <- 4
pct_wet <- 0.2    # % of surface area acting as a free-water exchanger
alpha_max <- 0.85 # maximum solar absorptivity
alpha_min <- 0.85 # minimum solar absorptivity
shape <- 3        # animal shape - 3 = lizard
T_RB_min <- 15  # min Tb at which they will attempt to leave retreat
T_B_min <- 15   # min Tb at which leaves retreat to bask
T_F_min <- 23.7     # minimum Tb at which activity occurs
T_F_max <- 33     # maximum Tb at which activity occurs
T_pref <- 28      # preferred Tb (will try and regulate to this)
CT_max <- 43      # critical thermal maximum (affects choice of retreat)
CT_min <- 7       # critical thermal minimum (affects choice of retreat)
mindepth <- 2     # min depth (node, 1-10) allowed
maxdepth <- 10    # max depth (node, 1-10) allowed
shade_seek <- 1   # shade seeking?
burrow <- 1       # can it burrow?
climb <- 0        # can it climb to thermoregulate?
nocturn <- 0      # nocturnal activity
crepus <- 0       # crepuscular activity
diurn <- 1        # diurnal activity


ecto <- ectotherm(Ww_g = Ww_g,
                  pct_wet = pct_wet,
                  alpha_max = alpha_max,
                  alpha_min = alpha_min,
                  shape = shape,
                  T_RB_min = T_RB_min,
                  T_B_min = T_B_min,
                  T_F_min = T_F_min,
                  T_F_max = T_F_max,
                  T_pref = T_pref,
                  CT_max = CT_max,
                  CT_min = CT_min, 
                  mindepth = mindepth,
                  maxdepth = maxdepth, 
                  shade_seek = shade_seek,
                  burrow = burrow,
                  climb = climb,
                  nocturn = nocturn,
                  crepus = crepus,
                  diurn = diurn) 


# retrieve output
metout <- as.data.frame(micro$metout)
environ<-as.data.frame(ecto$environ) # behaviour, Tb and environment
environ$Solar <- metout$SOLR # add solar radiation for activity window plots
#colnames(environ)[ncol(environ)] <- "Solar"
enbal<-as.data.frame(ecto$enbal) # heat balance outputs
masbal<-as.data.frame(ecto$masbal) # mass balance outputs
debout<-as.data.frame(ecto$debout) # DEB model outputs
yearout <- as.data.frame(ecto$yearout) # whole life cycle summary
yearsout <- as.data.frame(ecto$yearsout) # annual summaries

dates <- micro$dates

# quick plots

plot(dates, environ$TC, type='l')



# activity plots

environ1 <- subset(environ, YEAR == 3)
forage<-subset(environ1, ACT==2)
bask<-subset(environ1, ACT==1)
night<-subset(environ1, Solar==0)
day<-subset(environ1, Solar==0)
with(night,plot(TIME ~ DOY,ylab="Hour of Day",xlab="Day of Year",pch=15,cex=2,col=
                  'dark blue'))
# nighttime hours
with(bask,points(TIME~DOY,pch=15,cex=2,col='light blue')) # basking Tbs
with(forage,points(TIME~DOY,pch=15,cex=2,col='orange')) # foraging Tbs





# with DEB on
#C:\1_Urban\1_Doktorat\MODELIRANJE\DEB\DEB_models\debs_urtzi_check\DEBs_UD_Updated\Podarcis_muralis_UD

library(R.matlab)
Ih.data <- readMat('C:/1_Urban/1_Doktorat/MODELIRANJE/DEB/DEB_models/debs_urtzi_check/DEBs_UD_Updated/Iberolacerta_horvathi_UD/results_Iberolacerta_horvathi.mat') # load the allstat file
Ih.data
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
  assign(par.names[i], unlist(Pm.data$par[i]))
}

p.Am <- z * p.M / kap
F.m <- p.Am / kap.X # redefining F.m to max possible value
z.mult <- 1         # DEB body size scaling parameter
p.Xm <- 290.864 # had to take it manually from html
L.b <- 2.494 # had to take it manually from html

# assign possible missing parameters
if(exists("E.Hj")==FALSE){E.Hj <- E.Hb}
if(exists("E.He")==FALSE){E.He <- E.Hb}
if(exists("L.j")==FALSE){L.j <- L.b}

# assign missing 5-par thermal response curve parameters if necessary
if(exists("T.L")==FALSE){T.L <- CT_min + 273.15}
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
E.0 <- 2948.05 # I had to take it manually from the html
E_init <- E.0 / V_init
E_H_init <- 0
stage <- 0



# reproduction parameters
viviparous <- 0 
clutchsize <- 3 # This should be max R (Ri)?
# clutch_ab = c(3.482, -24.518)
minclutch <- 2
batch <- 1
photostart <- 3 # vernal equinox (March 20) is the start of the reproduction cycle
photofinish <- 1 # summer solstice is the end of the reproduction cycle

# food and water
#raindrink <- 2.5
#X <- 1000
# gutfill <- 100


ecto<-ectotherm(DEB=1,
                startday = 100,
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
                soilnode = 2)





# retrieve output
metout <- as.data.frame(micro$metout)
environ<-as.data.frame(ecto$environ) # behaviour, Tb and environment
environ$Solar <- metout$SOLR # add solar radiation for activity window plots
#colnames(environ)[ncol(environ)] <- "Solar"
enbal<-as.data.frame(ecto$enbal) # heat balance outputs
masbal<-as.data.frame(ecto$masbal) # mass balance outputs
debout<-as.data.frame(ecto$debout) # DEB model outputs
yearout <- as.data.frame(ecto$yearout) # whole life cycle summary
yearsout <- as.data.frame(ecto$yearsout) # annual summaries

dates <- micro$dates



# quick plots

plot(dates, environ$TC, type='l')



# activity plots

environ1 <- subset(environ, YEAR == 3)
forage<-subset(environ1, ACT==2)
bask<-subset(environ1, ACT==1)
night<-subset(environ1, Solar==0)
day<-subset(environ1, Solar==0)
with(night,plot(TIME ~ DOY,ylab="Hour of Day",xlab="Day of Year",pch=15,cex=2,col=
                  'dark blue'))
# nighttime hours
with(bask,points(TIME~DOY,pch=15,cex=2,col='light blue')) # basking Tbs
with(forage,points(TIME~DOY,pch=15,cex=2,col='orange')) # foraging Tbs




# tSVL <- read.csv('~/Desktop/DEB/1_green_lizards/Lacerta schreiberi/Data/6_time_SVL.csv', sep=';', dec=',')
# tSVLf <- read.csv('~/Desktop/DEB/1_green_lizards/Lacerta schreiberi/Data/7_time_SVL_females.csv', sep=';', dec=',')
# 
# plot(seq(1, length(micro$dates)) / 24, debout$L_W, type = 'l', xlab='date', ylab='body length (SVL, mm)')
# points(tSVL[,1], tSVL[,2], pch=19, cex=.5)
# points(tSVLf[,2], tSVLf[,3], pch=19, cex=.5, col='blue')


plot(seq(1, length(micro$dates)) / 24, debout$WETMASS, type = 'l', xlab = 'date', 
     ylab = paste0('wet mass (g)'), col = 'pink', lwd = 2, 
     ylim = c(0, max(debout$WETMASS)))
points(seq(1, length(micro$dates)) / 24, debout$V, type = 'l', xlab = 'date', 
       ylab = paste0('wet mass (g)'), col = 'dark green', lwd = 2)
points(seq(1, length(micro$dates)) / 24, debout$WETMASS-debout$WETGONAD, type = 'l', 
       lwd = 2, col = 'brown')
points(seq(1, length(micro$dates)) / 24, debout$WETMASS-debout$WETGONAD-debout$WETGUT,
       type = 'l', lwd = 2, col = 'grey')
abline(v = (seq(1, length(micro$dates)) / 24)[which(debout$E_H>E.Hb)[1]], lty = 2, col = 'grey')
abline(v = (seq(1, length(micro$dates)) / 24)[which(debout$E_H>E.Hp)[1]], lty = 2, col = 'grey')
legend(1200, max(debout$WETMASS) * 0.3, 
       c('repro. buffer', 'food in gut', 'reserve', 'structure'), lty = rep(1, 4), 
       col = c("pink", "brown", "grey", "dark green"), bty = 'n')
text(0, max(debout$WETMASS) * 1, labels = "embryo", cex = 0.85)
text((which(debout$E_H > E.Hp)[1] - which(debout$E_H > E.Hp)[1] * .5) / 24 ,
     max(debout$WETMASS) * 1, labels = "immature", cex = 0.85)
text(which(debout$E_H > E.Hp)[1] * 1.2 / 24, max(debout$WETMASS) * 1,
     labels = "adult", cex = 0.85)





# append dates
environ1 <- subset(environ, YEAR == 3)

days <- rep(seq(1,12),24)
days <- days[order(days)]
dates <- days+metout$TIME/60/24-1 # dates for hourly output
dates2 <- seq(1,12,1) # dates for daily output
metout <- cbind(dates,metout)
environ <- cbind(dates,environ)
masbal <- cbind(dates,masbal)
enbal <- cbind(dates,enbal)




with(environ1, plot(TC ~ dates, ylab = "", xlab="month of year", col = 'black', xlim = c(-0.25, 12), ylim = c(-20, 40), type = "l", yaxt = 'n'))
with(environ1, points(ACT * 2 + 7 ~ dates, type = "p", pch = 16, col = "orange"))
with(environ1, points(SHADE / 10 - 6 ~ dates, type = "l", col = "dark green"))
with(environ1, points(DEP - 10 ~ dates, type = "l", col = "brown"))
abline(ecto$T_F_min, 0, lty = 2, col = 'blue')
abline(ecto$T_F_max, 0, lty = 2, col = 'red')
ytick<-seq(15, 40, by=5)
axis(side=2, at=ytick, labels = TRUE)
mtext(text = c('A', 'B', 'I'), side = 2, line = 1, at = c(11, 9, 7))
ytick<-seq(-6, 4, by=2)
axis(side=2, at=ytick, labels = FALSE)
mtext(text = seq(0, 100, 20), side = 2, line = 1, at = seq(-6, 4, 2), las = 2)
ytick<-seq(-20, -10, by=2)
axis(side=2, at=ytick, labels = FALSE)
mtext(text = rev(seq(0, 100, 20)), side = 2, line = 1, at = seq(-20, -10, 2), las = 2)
abline(h = -10, lty = 2, col = 'grey')
mtext(text = c('body temperature (°C)', 'activity', 'shade (%)', 'depth (cm)'), side = 2, line = 2.5, at = c(30, 9, 0, -15))
text(0.1, c(ecto$T_F_max + 1, ecto$T_F_min + 1), c('T_F_max', 'T_F_min'), col = c('red', 'blue'), cex = 0.75)









yearsout




