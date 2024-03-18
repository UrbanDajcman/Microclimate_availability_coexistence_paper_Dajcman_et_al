


#install.packages("C:/Urban/Doktorat/MODELIRANJE/NICHEMAPR/microclima-master.zip", repos = NULL, 
#type = "win.binary")

#devtools::install_github("ilyamaclean/microclima")

#install.packages("RNetCDF")

library(NicheMapR)



rm(list = ls())


## MICROCLIMATE Snežnik - Svščaki 45.59946 14.396794


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

lon <- 14.396794
lat <- 45.59946




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
DEP <- c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200)

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
micro$longlat #LATLONG

dates <- micro$dates

plot(dates, metout$TALOC, type='l')
plot(dates, metout$TAREF, type='l')

plot(dates, metout$RHLOC, type='l')
plot(dates, metout$RH, type='l')

plot(dates, metout$VLOC, type='l')
plot(dates, metout$VREF, type='l')


plot(dates, metout$FROST, type='l')
plot(dates, metout$SNOWDEP, type='l')


save(micro, file='../Results/microNCEP/micro_ncep_Sneznik_aloIhor.RData')
load('micro_ncep_Sneznik_aloIhor.RData')







