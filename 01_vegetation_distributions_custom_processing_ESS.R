############## VEG TYPE DISTRIBUTION RASTER PROCESSING FOR CEMML PHASE II ########################
###### SCRIPT BY EMILY SEDDON, OCTOBER 2022 ##############

library(terra)
library(raster)
require(rgdal)
library(dplyr)
library(here)
library(doParallel)
here()

#load("S:/Projects/CEMML_HCCVI/CEMML_v2_Rproject/veg_dist_history.RData")
outdir <- here("group_distributions/group_rasters_raw")
#setwd("S:/Projects/CEMML_HCCVI/Pro_Projects/CEMML_HCCVI_Veg_distributions_PhaseII_102022/Veg_distribution_base_raster")

my.list<- list.files("S:/Projects/CEMML_HCCVI/Pro_Projects/CEMML_HCCVI_Veg_distributions_PhaseII_102022/Veg_distribution_base_raster/tif_files", pattern=".tif", full.names=T)
short.list<- list.files("S:/Projects/CEMML_HCCVI/Pro_Projects/CEMML_HCCVI_Veg_distributions_PhaseII_102022/Veg_distribution_base_raster/tif_files", pattern=".tif", full.names=F)
vegtypes<-gsub(".tif", "",my.list)
vegtypes.short<-gsub(".tif", "",short.list)

veggies <- raster::stack(my.list, quick=TRUE)
veggies <- terra::rast(my.list)
veg.1 <- rast(my.list[1])
veg.2 <- rast(my.list[2])
veg.3 <- rast(my.list[3])
veg.4 <- rast(my.list[4])
veg.5 <- rast(my.list[5])
veg.6 <- rast(my.list[6])
veg.7 <- rast(my.list[7])
veg.8 <- rast(my.list[8])
veg.9 <- rast(my.list[9])
veg.10 <- rast(my.list[10])
veg.11 <- rast(my.list[11])
veg.12 <- rast(my.list[12])
veg.13 <- rast(my.list[13])
veg.14 <- rast(my.list[14])
veg.15 <- rast(my.list[15])
veg.16 <- rast(my.list[16])
veg.17 <- rast(my.list[17])
veg.18 <- rast(my.list[18])
veg.19 <- rast(my.list[19])
veg.20 <- rast(my.list[20])
veg.21 <- rast(my.list[21])
veg.22 <- rast(my.list[22])
veg.23 <- rast(my.list[23])
veg.24 <- rast(my.list[24])
veg.25 <- rast(my.list[25])
veg.26 <- rast(my.list[26])
veg.27 <- rast(my.list[27])
veg.28 <- rast(my.list[28])

plot(veg.24)
a<-rbind(c(10000, 1), c(20000, 1))
b<-rbind(c(10000, 2), c(20000, 2))
c<-rbind(c(7076, 3), c(10000, 3))
d<-rbind(c(10000, 4), c(20000, 4))
e<-rbind(c(6143, 5), c(20000, 5))
f<-rbind(c(0, NA), c(10000, 6), c(20000, 6))
g<-rbind(c(10000, 7), c(20000, 7))
h<-rbind(c(941, 8), c(6141, 8), c(7092, 8))
ii<-rbind(c(6058, 9), c(10000, 9))
j<-rbind(c(10000, 10))
k<-rbind(c(10000, 11))
l<-rbind(c(7149, 12), c(20000, 12))
m<-rbind(c(6291, 13), c(10000, 13))
n<-rbind(c(10000, 14))
#o<-rbind(c(7082, 15))
p<-rbind(c(20000, 16))
q<-rbind(c(6319, 17), c(20000, 17))
#r<-rbind(c(9145, 18))
s<-rbind(c(6054, 19), c(20000, 19))
t<-rbind(c(10000, 20), c(20000, 20))
u<-rbind(c(6012, 21))
v<-rbind(c(6014, 22), c(10000, 22))
w<-rbind(c(6008, 23))
#x<-rbind(c(7109, 24))###
y<-rbind(c(10000, 25))
z<-rbind(c(10000, 26))
aa<-rbind(c(10000, 27), c(20000, 27))
bb<-rbind(c(10000, 28))

rclss.1<-classify(veg.1, a)
rclss.2<-classify(veg.2, b)
rclss.3<-classify(veg.3, c)
rclss.4<-classify(veg.4, d)
rclss.5<-classify(veg.5, e)
rclss.6<-classify(veg.6, f)
rclss.7<-classify(veg.7, g)
rclss.8<-classify(veg.8, h)
rclss.9<-classify(veg.9, ii)
rclss.10<-classify(veg.10, j)
rclss.11<-classify(veg.11, k)
rclss.12<-classify(veg.12, l)
rclss.13<-classify(veg.13, m)
rclss.14<-classify(veg.14, n)
#rclss.15<-classify(veg.15, o)
rclss.16<-classify(veg.16, p)
rclss.17<-classify(veg.17, q)
#rclss.18<-classify(veg.18, r)
rclss.19<-classify(veg.19, s)
rclss.20<-classify(veg.20, t)
rclss.21<-classify(veg.21, u)
rclss.22<-classify(veg.22, v)
rclss.23<-classify(veg.23, w)
#rclss.24<-classify(veg.24, x)
rclss.25<-classify(veg.25, y)
rclss.26<-classify(veg.26, z)
rclss.27<-classify(veg.27, aa)
rclss.28<-classify(veg.28, bb)

veg.list<- list(rclss.1, rclss.10, rclss.11, rclss.12, rclss.13, rclss.14, veg.15, rclss.16, rclss.17, veg.18, rclss.19, rclss.2, rclss.20, rclss.21, rclss.22, rclss.23, veg.24, rclss.25, rclss.26, rclss.27, rclss.28, rclss.3, rclss.4, rclss.5, rclss.6, rclss.7, rclss.8, rclss.9)


lapply(veg.list, function (x) terra::writeRaster(x, filename=paste0(outdir,"_reclass_", names(x), "_.tif")))


veg.list.rs<- list(rclss.10, rclss.11, rclss.13, rclss.14, rclss.21, rclss.22, rclss.23, rclss.25, rclss.28, rclss.6, rclss.9)


CEMML_systems_BPS<-rast("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/NorthAmerica_IVC_BPS846_wgs84.tif")
crs(CEMML_systems_BPS, proj = T)
reproject<-lapply(veg.list, function(x) {terra::project(x, y='epsg:4326')})
resamp <- lapply(veg.list.rs, function(x) {terra::resample(x, rclss.1, method="near")})

##### crop rasters by a bounding box around raster #####
cr<-crop(veg.16, pr)
crr<-raster(cr)
plot(cr, col = "red")
e<-extent(veg.16)
plot(e)

pe <- as.polygons(ext(veg.16))
pr <- as.polygons(veg.16 > -Inf, dissolve=TRUE)
#pp <- rasterToPolygons(r, dissolve=TRUE)

plot(veg.16)
plot(pe, lwd=5, border='red', add=TRUE)
plot(pr.dis, lwd=3, border='blue', add=TRUE)

#### Check extents ####
ext.rast2 <- extent(template) #TEMPLATE
ext.rast1 <- extent(-125, -67, 24.375, 53) #LOCA POLYGON
ext.rast3 <- extent(-129.2679, -39.26788, -37.84473, 52.15527) #VEG RASTER
#ext.rast1 <- extent(-2445015, 2344005, 241425, 3232155)

#plot(ext.rast1, xlim = c(-2445015, 2344005), ylim= c(20, 3232155), col="blue")
plot(ext.rast1, xlim = c(-130, -35), ylim= c(-38, 60), col="blue")
plot(ext.rast2, add=T, col="red")
plot(ext.rast3, add=T, col="green")
#extent(veg.3)<-c(-126, -66, 23.375, 54)

#my.crs <- "+proj=longlat +datum=WGS84 +no_defs"
#my.crs <- CEMML_systems_BPS
#crs(rclss.1) <- my.crs
#extent(veggies)<-c(-2741615, 4364875, -1556123, 4814797)
extent(veggies)<-c(-126, -66, 23.375, 54)

n<-rbind(c(7132, 20000), c(7147, 20000))
#n<-rbind(c(386, 20000), c(1272, 20000), c(1371, 20000), c(1397, 20000), c(1417, 20000), c(1428, 20000), c(1497, 20000), c(1684, 20000), c(1720, 20000), c(2056, 20000))
#m<-rbind(c(7304, 10000), c(7305, 10000), c(7306, 10000), c(7312, 10000), c(7364, 10000), c(7381, 10000), c(7509, 10000), c(7510, 10000))
rclss<-classify(veggies.90m, n)
#m<-rbind(0, NA)
#rclss.2<-classify(rclss, m)

writeRaster(rclss, filename=paste0("S:/Projects/CEMML_HCCVI/Pro_Projects/CEMML_HCCVI_Veg_distributions_PhaseII_102022/Veg_distribution_base_raster/", "SCIOFW_BpS_dist_90m_reclass.tif"), overwrite=T, datatype='INT2U')


#rclssG288<-classify(G288, n)
#m<-rbind(c(7074, 20000), c(7100, 20000))
#rclssBpSG288<-classify(BpS_G288, m)

ext(veggies.30m)<-c(-2534975, 4130335, -1527053, 4561087)

rs <- resample(veggies.30m, rclss, method="near")

rclss<-classify(rs, n)

merge.rast <- merge(rs, rclss)

ext.rast1 <- extent(-2445015, 2343975, 241455, 3232155)
ext.rast2 <- extent(-2534975, 4130335, -1527053, 4561087)

#plot(ext.rast1, xlim = c(-2534975,4130335), ylim= c(-1527053,4561087), col="blue")
#plot(ext.rast2, add=T, col="red")


writeRaster(merge.rast, filename=paste0("S:/Projects/CEMML_HCCVI/Pro_Projects/CEMML_HCCVI_Veg_distributions_PhaseII_102022/Veg_distribution_base_raster/", "CGPMGP_WGPFPG_BpS_30m_30m_merge_10272022.tif"), overwrite=T, datatype='INT2U')


CPPJSh_90m<-rast("S:/Projects/BLM/BLM_HCCVI_Pinyon_Juniper/SSS_PJ_zones/Pro_Projects/BLM_SSS_PJ_adaptation_zones/Analysis/Co_Plateau_PJSh/BpS_CO_Plateau_PJ_Shrubland_90m.tif")
CPPJSh_90m_rs<-rast("S:/Projects/BLM/BLM_HCCVI_Pinyon_Juniper/SSS_PJ_zones/Pro_Projects/BLM_SSS_PJ_adaptation_zones/Analysis/Co_Plateau_PJSh/BpS_CO_Plateau_PJ_Shrubland_90m_resample.tif")
CPPJSh_30m<-rast("S:/Projects/BLM/BLM_HCCVI_Pinyon_Juniper/SSS_PJ_zones/Pro_Projects/BLM_SSS_PJ_adaptation_zones/Analysis/Co_Plateau_PJSh/BpS_CPPJSh_reclass_extract.tif")
rs <- resample(CPPJSh_30m, CPPJSh_90m, method="near")
merge.rast.2 <- merge(rs, CPPJSh_90m)
values(merge.rast)
writeRaster(merge.rast.2, filename=paste0("S:/Projects/BLM/BLM_HCCVI_Pinyon_Juniper/SSS_PJ_zones/Pro_Projects/BLM_SSS_PJ_adaptation_zones/Analysis/Co_Plateau_PJSh/", "CPPJSh_merge_10262022.tif"), overwrite=T, datatype='INT2U')
