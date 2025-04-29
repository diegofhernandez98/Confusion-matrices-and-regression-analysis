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


sdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working"
tdir <- here::here("Data")

target_crs <- "EPSG:7850"
shp <- st_read(paste0(sdir, "\\SummaryData\\shpLink\\DBCA_FireSeverityCatelogue_1987to2023_2024-11-18.shp")) %>%
  st_make_valid()%>%
  st_transform(crs = target_crs)

# Load the raster file
bf <- rast(paste0(sdir, "\\Projects\\2024-11-06_BF2023-FRK007\\2024-05-01_BF2023-FRK007\\v2024-11-01\\ozcbi_geoTifs\\OzCBI_FRK007_2024-11-01.tif"))
# Project the raster to the CRS of shp using a valid method
bf <- project(bf, target_crs, method = "near")
bf_20 <- bf
res(bf_20) <- 20 #20 m
bf <- terra::resample(bf, bf_20, method = "near")

#getting raster boundary https://gis.stackexchange.com/questions/187798/create-polygons-of-the-boundary-of-a-raster-in-r
pe <- as.polygons(ext(bf))
pr <- as.polygons(bf> -Inf)
shp_pr <- st_as_sf(pr)
plot(pr)

# #intersection 
selection <- st_intersects(shp,shp_pr)
selected_polygons <- shp[apply(selection, 1, any), ]
selected_polygons[1]
BurnID <- as.character(selected_polygons[[1]])
print(BurnID)

#changing path values
#only run ONCE !!!
selected_polygons$filePath <- str_replace_all(selected_polygons$filePath ,"Landsat","Historical")
selected_polygons$filePath <- str_replace_all(selected_polygons$filePath ,"Sentinel", "sevSentinel")
selected_polygons$filePath <- str_replace_all(selected_polygons$filePath ,"severity_geoTifs", "ozcbi_geoTifs")
selected_polygons$filePath <- str_replace_all(selected_polygons$filePath ,"BurnSeverity", "OzCBI")

i <- 1
rst_list <- list()
for (i in 1:nrow(selected_polygons)){
  rst <- rast(paste0(sdir, "\\", selected_polygons$filePath[i]))
  plot(rst)
  rst_listp <- project(rst, target_crs, method = "near")
  rst_listp20 <-rst_listp 
  res(rst_listp20) <- 20 
  rst_lists <- resample(rst_listp,rst_listp20,method = "near")
  rst_list[[i]] <-rst_lists
}
###############################################################################################################
#updating the paths
selection <- st_intersects(shp,shp_pr)
FClips <- shp[apply(selection, 1, any), ]

FClips$filePath <- str_replace_all(FClips$filePath ,"Landsat","Historical")
FClips$filePath <- str_replace_all(FClips$filePath ,"Sentinel", "sevSentinel")
FClips$filePath <- str_replace_all(FClips$filePath ,"severity_geoTifs","treatment_area")


new_path <- sapply(FClips$filePath, function(path) {
  sub("treatment_area/.*", "treatment_area/", path)
})

# Construct the full paths
sdir <- "Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working"
treat_dir <- file.path(sdir, new_path)

dir_unique <- unique(treat_dir)
#testing if files exists
for (i in 1:length(dir_unique)) {
  if (file.exists(dir_unique[i])) {
    cat("The path", dir_unique[i], "exists.\n")
  } else {
    cat("The path", dir_unique[i], "does not exist.\n")
  }
}

#getting shapefiles
treat_list <- list()
shp_list <- list()
for (i in 1:length(dir_unique)){
  if(file.exists(dir_unique[i])){
    treat_files <- list.files(dir_unique[[i]], pattern = "\\.shp$", full.names = TRUE)
  }else{
    print(paste("No .shp files found in", dir_unique[i]))
  }
  treat_list[[i]] <- treat_files
}

for (i in 1:length(treat_list)) {
  for (j in 1:length(treat_list[[i]])) {
    shp_obj <- st_read(treat_list[[i]][j])
    shp_list[[length(shp_list) + 1]] <- shp_obj
  }
}

target_crs <- "EPSG:7850"
shp_listp <- list()

for (i in 1:length(shp_list)){
  shp_p <- st_transform(shp_list[[i]],target_crs)
  shp_listp[[i]] <- shp_p
}
###############################################################################################################
#exporting individuals polygons to single shp
single_shp <- list()
for (i in seq_along(shp_listp)) {
  polygons <- shp_listp[[i]]
  for (j in 1:nrow(polygons)) {
    single_polygons <- polygons[j, ]
    single_shp <- append(single_shp, list(single_polygons))
  }
}
###############################################################################################################
#changing raster list names 
rst_new <- list()
for (i in seq_along(rst_list)){
  names(rst_list[[i]]) <- str_remove_all(names(rst_list[[i]]), "OzCBI_")
  names(rst_list[[i]]) <- substr(names(rst_list[[i]]), 1,17)
  rst_new[[i]] <- rst_list[[i]]
}


#checking for rasters within the shp list
# Initialize total matches counter
total_matches <- 0

# Loop through each raster in rst_new
for (i in seq_along(rst_new)) {
  raster_name <- names(rst_new[[i]])
  match_found <- FALSE
  
  # Loop through each shapefile in single_shp
  for (j in seq_along(single_shp)) {
    burn_id <- single_shp[[j]]$BURNID
    
    # Check if raster name matches BURNID
    if (raster_name == burn_id) {
      cat("raster", raster_name, "is equal to:", burn_id, "\n")
      match_found <- TRUE
      total_matches <- total_matches + 1
      
      # Export the raster
      #output_path <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Rasters/", raster_name, ".tif")
      #writeRaster(rst_new[[i]], filename = output_path, overwrite = TRUE)
      #output_path2 <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/shp/", burn_id, ".shp")
      #st_write(single_shp[[j]],output_path2)
      
      break
    }
  }
  
  if (!match_found) {
    cat("raster", raster_name, "does not match any BURNID\n")
  }
}

cat("Total matches:", total_matches, "\n")
###############################################################################################################

#CHANGE FOR BUSHFIRE
# Construct the full paths
sdir <- "Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/2024-05-01_BF2023-FRK007/v2024-11-01/treatment_area"
treat_dir <- file.path(sdir)

dir_unique <- unique(treat_dir)

#getting shapefiles
treat_list <- list()
shp_list <- list()
for (i in 1:length(dir_unique)){
  if(file.exists(dir_unique[i])){
    treat_files <- list.files(dir_unique[[i]], pattern = "\\.shp$", full.names = TRUE)
  }else{
    print(paste("No .shp files found in", dir_unique[i]))
  }
  treat_list[[i]] <- treat_files
}

for (i in 1:length(treat_list)) {
  for (j in 1:length(treat_list[[i]])) {
    shp_obj <- st_read(treat_list[[i]][j])
    shp_list[[length(shp_list) + 1]] <- shp_obj
  }
}

target_crs <- "EPSG:7850"
shp_listp <- list()

for (i in 1:length(shp_list)){
  shp_p <- st_transform(shp_list[[i]],target_crs)
  shp_listp[[i]] <- shp_p
}

single_shp <- list()
for (i in seq_along(shp_listp)) {
  polygons <- shp_listp[[i]]
  for (j in 1:nrow(polygons)) {
    single_polygons <- polygons[j, ]
    single_shp <- append(single_shp, list(single_polygons))
  }
}

#single export
raster_name <- names(bf)
output_path <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Rasters/", raster_name, ".tif")
writeRaster(bf, filename = output_path, overwrite = TRUE)
burn_id <- single_shp[[1]]$BURNID
output_path2 <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/shp/", burn_id, ".shp")
st_write(single_shp[[1]],output_path2)





###############################################################################################################

#bush fire export for several bush firees
total_matches <- 0
for (i in seq_along(bf)){
  raster_name <- names(bf[[i]])
  match_found <- FALSE
  # Loop through each shapefile in single_shp
  for (j in seq_along(single_shp)) {
    burn_id <- single_shp[[j]]$BURNID
    
    # Check if raster name matches BURNID
    if (raster_name == burn_id) {
      cat("raster", raster_name, "is equal to:", burn_id, "\n")
      match_found <- TRUE
      total_matches <- total_matches + 1
      
      # Export the raster
      output_path <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Rasters/", raster_name, ".tif")
      writeRaster(rst_new[[i]], filename = output_path, overwrite = TRUE)
      output_path2 <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/shp/", burn_id, ".shp")
      st_write(single_shp[[j]],output_path2)
      
      break
    }
  }
  if (!match_found) {
    cat("raster", raster_name, "does not match any BURNID\n")
  }
}

cat("Total matches:", total_matches, "\n")

###############################################################################################################
#knowing the overlap percentage 









###############################################################################################################

#CLIPPING RASTERS USING SHP
masked_raster2 <- list()
for (i in seq_along(rst_new)) {
  raster_name <- names(rst_new)[i]
  shp_name <- names(single_shp)[i]
  cropped[[raster_name]] <- crop(rst_new[[raster_name]], single_shp[[shp_name]])
  masked[[raster_name]] <- terra::mask(cropped[[raster_name]],single_shp[[shp_name]])
  
  # Define the output file path
  #output_file <- file.path(Clip_results2, paste0(raster_name, ".tif"))
  
  # Export the cropped raster
  #writeRaster(masked_raster2[[raster_name]], output_file, overwrite = TRUE)
}

