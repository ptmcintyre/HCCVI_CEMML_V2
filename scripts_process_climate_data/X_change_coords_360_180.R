###this script changed coordinates from original system in netcdf files to map correctly
##to convert from 0 -360 to 180-180 should be -360 for values greater than 180. Here just -360 since all values greater than 180 for longitude
library(terra)
library(raster)


clim.files<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", full.names = T)
new.file<-gsub("biovars_year", "biovars_yearWGS84",clim.files)
i=1
for (i in 1:length(clim.files)){
  oldR<-rast(clim.files[i])
  xmin(oldR)=xmin(oldR)-360
  xmax(oldR)=xmax(oldR)-360
  newR<-oldR
  out.file<-new.file[i]
  writeRaster(newR, out.file, overwrite=TRUE)
}



clim.files<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/historic", full.names = T)
new.file<-gsub("monthly_vals_year", "monthly_vals_yearWGS84",clim.files)
i=1
for (i in 1:length(clim.files)){
  oldR<-rast(clim.files[i])
  xmin(oldR)=xmin(oldR)-360
  xmax(oldR)=xmax(oldR)-360
  newR<-oldR
  out.file<-new.file[i]
  writeRaster(newR, out.file, overwrite=TRUE)
}


clim.files<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/rcp85", full.names = T)
new.file<-gsub("monthly_vals_year", "monthly_vals_yearWGS84",clim.files)
i=1
for (i in 1:length(clim.files)){
  oldR<-rast(clim.files[i])
  xmin(oldR)=xmin(oldR)-360
  xmax(oldR)=xmax(oldR)-360
  newR<-oldR
  out.file<-new.file[i]
  writeRaster(newR, out.file, overwrite=TRUE)
}






templateR<-raster("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids/LOCA Conus polygon/LOCA_WGS84_raster_template.tif")


r <- raster(oldR)
r2<-raster(oldR, template=templateR)

coords(r)
plot(r)

plot(templateR)

plot(oldR)
