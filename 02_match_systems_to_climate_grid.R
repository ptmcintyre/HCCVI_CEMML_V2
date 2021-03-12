

# This script upscales 90m veg distribution rasters to 800m
# so that they match our PRISM and TopoWx climate data, and
# saves them as .grd files. It is assumed that the source files
# have already been projected to WGS84 (Steph did this in Arc).
   

library(dplyr)
library(raster)
library(here)
library(rasterDT)
library(exactextractr)
library(sf)

outdir <- here("system_distributions/MACA_rasters")

vegtypes <- list.files(here("system_distributions/system_rasters_raw"), pattern=".grd", full.names=T)
vegtypes<-gsub(".grd", "",vegtypes)
vegtypes.short <- list.files(here("system_distributions/system_rasters_raw"), pattern=".grd")
vegtypes.short<-gsub(".grd", "",vegtypes.short)



#Read in climate file to creat template raster for creating summed values of ecosystems
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
template<-raster(here("biovars/historic", historic.biovars[1]))
template[template<30]<-0
plot(template)
template <- reclassify(template, c(-Inf, Inf, NA))

# define the subset of types to run
vegtypes_run <- vegtypes

#polygon maca template for summarizing bps raster values in and converting to upscaped raster 
maca_poly<-st_read("F:/Projects/CEMML/ClimateGrids/MACA_CCSM4_Monthly_CONUS_Standard_Poly.shp")



for(veg in vegtypes_run){
      name <- vegtypes.short[match(veg, vegtypes)]
      outfile <- paste0(outdir, "/", name)
      if(file.exists(outfile)) next()
      print(veg)
      
      # open raster
      veg <- raster(veg) 
      #veg<-raster(vegtypes[1])
      plot(veg)
      
      # reclassify to either NA or 1, and convert to points
      #veg <- reclassify(veg, rcl=c(-1,1,NA,  1,Inf,1), right=F)
      #veg_points <- as.data.frame(rasterToPoints(veg))
      #coordinates(veg_points) <- c("x", "y")
      
      my.zone<-exact_extract(veg, maca_poly, 'sum')
      maca_poly$veg<-my.zone
      my.rast<-fasterizeDT(maca_poly, template, fun='sum',field="veg" )
      
      # rasterize points at 800m. Criterion: 15 or more 90m presences
      #upscaled <- rasterize(veg_points, template, field="layer", fun=sum)
      #upscaled <- reclassify(upscaled, rcl=c(-1,14.5,NA,  14.5,Inf,1))
      upscaled <- reclassify(my.rast, rcl=c(-1,14.5,NA,  14.5,Inf,1))
      writeRaster(upscaled, filename=paste(outfile, ".tif", sep=""), overwrite=T)
      writeRaster(my.rast, filename=paste(outfile, "_continuous.tif", sep=""), overwrite=T)
}