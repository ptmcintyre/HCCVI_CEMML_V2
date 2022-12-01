

# This script upscales 90m veg distribution rasters to match the resolution of the climate raster being used
# saves them as .tif files in presence/absence based on 15cells (or user specified) or continuous counts 
# careful with projections (things should be WGS84 EPSG:4326, but climate files are sometimes odd)

chooseCRANmirror()
library(dplyr)
library(raster)
library(rgdal)
library(here)
library(rasterDT)
library(exactextractr)
library(sf)
library(terra)
library(doParallel)
here()
outdir <- here("group_distributions/LOCA_rasters")

#load(here("group_distributions/LOCA_rasters/CTFW_clim_poly_112022.RData"))

vegtypes.30m <- list.files(here("group_distributions/group_rasters_raw/30m"), pattern=".tif", full.names=T)
#vegtypes.90m <- list.files(here("group_distributions/group_rasters_raw/90m"), pattern=".tif", full.names=T)
#vegtypes.90m <- list.files("S:/Projects/_Workspaces/Emily_Seddon/90m", pattern=".tif", full.names=T)
vegtypes.90m <- list.files("D:/90m", pattern=".tif", full.names=T)


##testr<-raster(vegtypes[1]) #check that values are 0 or 1
#vegtypes.30m<-gsub(".tif", "",vegtypes.30m)
vegtypes.short.30m <- list.files(here("group_distributions/group_rasters_raw/30m"), pattern=".tif")
vegtypes.short.30m<-gsub(".tif", "",vegtypes.short.30m)

#vegtypes.90m<-gsub(".tif", "",vegtypes.90m)
vegtypes.short.90m <- list.files("D:/90m", pattern=".tif")
vegtypes.short.90m<-gsub(".tif", "",vegtypes.short.90m)
#vegtypes.short<-vegtypes.short[c(2,14)]

#Read in climate file to create template raster for creating summed values of ecosystems

#historic.biovars<-list.files(here("process_initial_climate_data/biovars_yearWGS84"), pattern=".tif")
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
#template<-raster("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids/LOCA Conus polygon/LOCA_WGS84_raster_template.tif")
#template<-raster(here("process_initial_climate_data/biovars_yearWGS84", historic.biovars[1]))
template <- raster(here("biovars/historic", historic.biovars[1])) #raster package
#template <- rast(here("biovars/historic", historic.biovars[1])) #terra package

#my.crs<-crs(template)

#?why do I have this bit about reclassifying
#template[template<30]<-0
plot(template)

vegtypes_run <- vegtypes.30m
vegtypes_run <- vegtypes.90m

#polygon LOCA template for summarizing bps raster values in and converting to upscaled raster 

clim_poly<-st_read("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids/LOCA_grid_SCCASC_WGS84.shp") #updated with CRS- simple features no longer recognizing some older CRS references
## realized clim_poly in GCS_wgs84 (no negatives, goes 0-360)
#clim_poly<-st_transform(clim_poly, "EPSG:5070") 
st_crs(clim_poly)


detectCores()
cpus <- 4
cl <- makeCluster(cpus)
registerDoParallel(cl)
i=1
veg=vegtypes.30m[i]

foreach(i=1:length(vegtypes_run)) %dopar% {
      veg=vegtypes.30m[i]
      name <- vegtypes.short.30m[match(veg, vegtypes.30m)]
      outfile <- paste0(outdir, "/", name)
      if(file.exists(outfile)) next()
      print(veg)
      
      # open raster
      #veg <- raster(vegtypes.30m[i]) #raster package
      veg_1 <- terra::rast(vegtypes.30m[i]) #terra package
      pr <- terra::as.polygons(veg_1 > -Inf)
      cr <- terra::crop(veg_1, pr)
      veg_2<-raster::raster(cr)
  
      # reclassify to either NA or 1, based on extraction of number of pixels of the group within MACA climate raster cells
      my.zone<-exactextractr::exact_extract(veg_2, clim_poly, 'sum')
      clim_poly$veg<-my.zone
      #st_transform(clim_poly, "EPSG:4326")
      my.rast<-rasterDT::fasterizeDT(clim_poly, template, fun='sum',field="veg")
      my.rast<-my.rast/veg_2@data@max ##divide by max cell value (e.g., 22)
      #terra::project(my.rast, y='epsg:4326')
      #max(clim_poly$veg)
      
      upscaled <- raster::reclassify(my.rast, rcl=c(-1,130.5,NA, 130.5,Inf,1)) #change for 30m reclassifies values from -1 to 14.5 as NA, and from 14.5 to 1
      #upscaled <- raster::reclassify(my.rast, rcl=c(-1,14.5,NA, 14.5,Inf,1)) #change for 90m reclassifies values from -1 to 14.5 as NA, and from 14.5 to 1
      raster::writeRaster(upscaled, filename=paste(outfile, ".tif", sep=""), overwrite=T)
      raster::writeRaster(my.rast, filename=paste(outfile, "_continuous.tif", sep=""), overwrite=T)
}

stopCluster()

#save(my.zone, file="S:/Projects/CEMML_HCCVI/CEMML_v2_Rproject/HCCVI_CEMML_V2/group_distributions/LOCA_rasters/CGPMGP_WGPFPG_clim_poly_11082022.RData")

rm()
gc()


