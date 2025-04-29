library(sp)
library(sf)
library(raster)
library(here)
library(terra)
library(tidyverse)
library(lubridate)
library(caret)
library(dplyr)
library(stringr)
library(listr)
library(ggplot2) 
#test

#shapefiles
tdir <- here::here("Data")
shp_dir <- file.path(tdir, "shp")
shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)


shp_list <- list()
for (shp_file in shp_files) {
  var_name <- tools::file_path_sans_ext(basename(shp_file))
  shp_obj <- st_read(shp_file)
  shp_list[[var_name]] <- shp_obj
}
#############################################################################
#rasters
tdir <- here::here("Data")
rst_dir <- file.path(tdir, "Rasters")
rst_files <- list.files(rst_dir, pattern = "\\.tif$", full.names = TRUE)

raster_list <- list()
for (raster_file in rst_files) {
  var_name <- tools::file_path_sans_ext(basename(raster_file))
  raster_obj <- rast(raster_file)  # Using terra::rast
  raster_list[[var_name]] <- raster_obj
}
#############################################################################

#############################################################################
#clipping
cropped <- list()
masked <- list()

for(i in seq_along(raster_list)){
  raster_name <- names(raster_list)[i]
  shp_name <- names(shp_list)[i]
  cropped[[raster_name]] <- crop(raster_list[[raster_name]], shp_list[[shp_name]])
  masked[[raster_name]] <- terra::mask(cropped[[raster_name]], shp_list[[shp_name]])
  output_path <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Clipped_rasters/", raster_name, ".tif")
  writeRaster(masked[[raster_name]], filename = output_path, overwrite = TRUE)
}




