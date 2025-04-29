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
library(gridExtra)
library(grid)
library(viridis)
library(reshape2)


# Define the custom color palette
custom_colors <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")

# Function to convert raster to data frame
raster_to_df <- function(raster) {
  df <- as.data.frame(raster, xy = TRUE)
  colnames(df) <- c("Longitude", "Latitude", "Pixel Value")
  return(df)
}

# Assuming rlst is a list of raster objects
for(i in seq_along(rlst)){
  raster <- rlst[[i]]
  raster_df <- raster_to_df(raster)
  raster_name <- names(rlst)[i]
  
  plots <- ggplot(raster_df, aes(x = Longitude, y = Latitude, fill = `Pixel Value`)) + 
    geom_tile() +
    scale_fill_gradientn(colors = custom_colors) +
    labs(title = raster_name, x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
  plot_saved <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Maps/", raster_name, ".png")
  ggsave(plot_saved, plots = plot, height = 4, width = 6.5)
}

##################################################################################


wf_df <- raster_to_df(wf)

wf_plot <- ggplot(wf_df,aes(x = Longitude, y = Latitude, fill = `Pixel Value`)) + 
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors) +
  labs(title = raster_name, x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

print(wf_plot)

##################################################################################
#both combined :
# Define the custom color palette
custom_colors <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")

# Function to convert raster to data frame
raster_to_df <- function(raster) {
  df <- as.data.frame(raster, xy = TRUE)
  colnames(df) <- c("Longitude", "Latitude", "Pixel Value")
  return(df)
}

# Create the base plot for wf_df
wf_df <- raster_to_df(wf)
wf_plot <- ggplot(wf_df, aes(x = Longitude, y = Latitude, fill = `Pixel Value`)) + 
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors) +
  labs(title = "WF Data", x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

# Assuming rlst is a list of raster objects
for(i in seq_along(rlst)){
  raster <- rlst[[i]]
  raster_df <- raster_to_df(raster)
  raster_name <- names(rlst)[i]
  
  # Create the plot for the current raster
  plot <- ggplot(raster_df, aes(x = Longitude, y = Latitude, fill = `Pixel Value`)) + 
    geom_tile() +
    scale_fill_gradientn(colors = custom_colors) +
    labs(title = raster_name, x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add the wf_df plot as a layer
  combined_plot <- plot + 
    geom_tile(data = wf_df, aes(x = Longitude, y = Latitude, fill = `Pixel Value`), alpha = 0.5) +
    scale_fill_gradientn(colors = custom_colors)
  
  print(combined_plot)
  plot_saved <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Maps/", raster_name, ".png")
  ggsave(plot_saved, plot = combined_plot, height = 4, width = 6.5)
}







