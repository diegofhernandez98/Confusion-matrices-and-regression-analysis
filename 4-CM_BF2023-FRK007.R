library(sp)
library(sf)
library(raster)
library(here)
library(terra)
library(tidyverse)
library(lubridate)
install.packages("caret")
library(caret)
library(dplyr)
library(stringr)
library(listr)


sdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working"
tdir <- here::here("Data")

target_crs <- "EPSG:7850"
shp <- st_read(paste0(sdir, "\\SummaryData\\shpLink\\DBCA_FireSeverityCatelogue_1987to2023_2024-11-18.shp")) %>%
  st_make_valid()%>%
  st_transform(crs = target_crs)

# Load the raster file
bf <- rast(paste0(sdir, "\\Projects\\2024-11-06_BF2023-FRK007\\2024-05-01_BF2023-FRK007\\v2024-11-01\\severity_geoTifs\\BurnSeverity_FRK007_2024-11-01.tif"))
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
selected_polygons$filePath <- str_replace_all(selected_polygons$filePath ,"Landsat","Historical")
selected_polygons$filePath <- str_replace_all(selected_polygons$filePath ,"Sentinel", "sevSentinel")

#Getting rasters from each path
i <- 1
rst_list <- list()
df_rasters <-list()
for (i in 1:nrow(selected_polygons)){
  rst <- rast(paste0(sdir, "\\", selected_polygons$filePath[i]))
  plot(rst)
  rst_listp <- project(rst, target_crs, method = "near")
  rst_listp20 <-rst_listp 
  res(rst_listp20) <- 20 
  rst_lists <- resample(rst_listp,rst_listp20,method = "near")
  rst_list[[i]] <-rst_lists
  #df_rasters[[i]] <-as.data.frame(rst_list[[i]],xy = TRUE, na.rm = TRUE)\
  
  #saving temp file !!
  raster_name <- names(rst_list[[i]])[1]
  file_name <- paste0("raster_", raster_name, ".rds")
  #saveRDS(rst_list[[i]],here::here("tmp",file_name))
  
  output_path <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Clipped_rasters/Severity/", file_name, ".tif")
  #writeRaster(rst_list[[i]], filename = output_path, overwrite = TRUE)
}



###############################################################################################################
#stacking
df_list <- list() 

for (i in 1:length(rst_list)) {
  pb <- resample(rst_list[[i]], bf, method = "near")
  st <- c(bf, pb)
  df <- as.data.frame(values(st)) %>%
    na.omit()
  if (nrow(df) != 0) {
    colnames(df) <- c("wf", "pb")
    df$PBname <- names(rst_list[[i]])
    df$PBname <- str_remove_all(df$PBname, "BurnSeverity_")
    df$PBname <- substr(df$PBname, 1, 17)
  } else {
    cat(names(rst_list[[i]]), "\n")
  }
  if (nrow(df) >= 500) {
    df_list[[length(df_list) + 1]] <- df  # Add non-NULL data frame to df_list
  }
}
length(df_list)

name_list <- character(length(df_list)) 
#changing list name of each item

for (i in 1:length(df_list)) {
  df_lname <- df_list[[i]][[3]]
  conf_name <- unique(df_lname)
  name_list[i] <- conf_name 
}


###############################################################################################################
#knowing list of less 500
not_included <- list()
for (i in 1:length(rst_list)){
  pb <- resample(rst_list[[i]], bf, method = "near")
  st <- c(bf, pb)
  df <- as.data.frame(values(st)) %>%
    na.omit()
  if (nrow(df) != 0) {
    colnames(df) <- c("wf", "pb")
    df$PBname <- names(rst_list[[i]])
    df$PBname <- str_remove_all(df$PBname, "BurnSeverity_")
    df$PBname <- substr(df$PBname, 1, 17)
  } else {
    cat(names(rst_list[[i]]), "\n")
  }
  
  if (nrow(df) <= 500) {
    not_included [[length(not_included ) + 1]] <- df  # Add non-NULL data frame to df_list
  }
}
length(not_included)


name_list2 <- character(length(not_included))

for (i in 1:length(not_included)) {
  if (ncol(not_included[[i]]) >= 3) {  # Check if the data frame has at least 3 columns
    df_lname <- not_included[[i]][[3]]
    if (length(df_lname) > 0) {  # Check if the third column has values
      conf_name2 <- unique(df_lname)
      name_list2[i] <- paste(conf_name2, collapse = ", ")  # Combine unique values into a single string
    } else {
      name_list2[i] <- NA  # Assign NA if the third column is empty
    }
  } else {
    name_list2[i] <- NA  # Assign NA if there are less than 3 columns
  }
}
print(name_list2)
###############################################################################################################
# #Creating the confusion matrix by using previous data frame
conf_matrix_list <- list()
for (i in 1:length(df_list)) {
  first_column <- df_list[[i]][[1]]
  second_column <- df_list[[i]][[2]]
  predicted <- factor(first_column) # bf Predicted values
  expected <- factor(second_column) # pb Expected values
  
  # Ensure both factors have the same levels
  all_levels <- union(levels(predicted), levels(expected))
  predicted <- factor(predicted, levels = all_levels)
  expected <- factor(expected, levels = all_levels)
  
  # Check if there are any common levels
  if (length(intersect(levels(predicted), levels(expected))) > 0) {
    # Create the confusion matrix
    conf_matrix <- confusionMatrix(predicted, expected)
    
    # Store the confusion matrix in the list
    conf_matrix_list[[i]] <- conf_matrix
  } else {
    warning(paste("No overlapping levels for dataframe", i))
  }
}
names(conf_matrix_list) <- name_list


for (i in 1:length(conf_matrix_list)) {
  overall <- conf_matrix_list[[i]]$overall
  matrix <- names(conf_matrix_list)[i]
  cat("================================================","\n")
  cat("Confusion matrix:",matrix,"\n")
  print(overall)
}
print(name_list)
print(name_list2)


print(name_list[[1]])

conf_matrix_list[[1]]$table
conf_matrix_list[[1]]$overall



###############################################################################################################
#getting the fire types
df_list_names <- names(conf_matrix_list)

filtered <- filter(selected_polygons, BURNID %in% df_list_names)
firetype <-as.character(filtered[[6]])

for (i in 1:length(firetype)) {
  if (is.na(firetype[i])) {
    next
  } else if (firetype[i] == "WF") {
    firetype[i] <- "Wildfire"
  } else if (firetype[i] == "PB") {
    firetype[i] <- "Prescribed Burn"
  }
}

##another way 
# 
# firetype <- ifelse(firetype == "WF", "Wildfire", 
#                    ifelse(firetype == "PB", "Prescribed Burn", firetype))
# 


