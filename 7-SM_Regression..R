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

tdir <- here::here("tmp")
rst_dir <- file.path(tdir)
rst_files <- list.files(rst_dir, pattern = "\\.rds$", full.names = TRUE)
target_crs <- "EPSG:7850"

rst_list <- list()

for (i in 1:length(rst_files)) {
  var_name <- tools::file_path_sans_ext(basename(rst_files[i]))
  loaded_raster <- readRDS(rst_files[i])
  raster_obj <- project(loaded_raster, target_crs, method = "near")
  rst_list[[var_name]] <- raster_obj
}
length(rst_list)



sdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working"
bf <- rast(paste0(sdir, "\\Projects\\2024-11-06_BF2023-FRK007\\2024-05-01_BF2023-FRK007\\v2024-11-01\\severity_geoTifs\\BurnSeverity_FRK007_2024-11-01.tif"))

plot(bf)
bf <- project(bf, target_crs, method = "near")
bf_20 <- bf
res(bf_20) <- 20 #20 m
bf <- terra::resample(bf, bf_20, method = "near")
wf <- bf

###############################################################################################################
#percentage coverage between wildfire and prescribed burns
#reading shp of wildfire
tdir <- here::here("Data")
shp_dir <- file.path(tdir, "shp")
shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
shp_file <- st_read(shp_files[grep("FRK007.shp", shp_files)])

areas_overlapping <- list()
df_list <- list()
rlst <- list()
for (i in seq_along(rst_list)) {
  rasters <- rst_list[[i]]
  
  # Prescribed burns to polygons
  pr <- as.polygons(ext(rasters))
  pr <- as.polygons(rasters > -Inf)
  shp_pr1 <- st_as_sf(pr)
  
  # Wildfire to polygons
  pr2 <- as.polygons(ext(wf))
  pr2 <- as.polygons(wf > -Inf)
  shp_pr2 <- st_as_sf(pr2)
  
  if (any(st_intersects(shp_pr1, shp_pr2, sparse = FALSE))) {
    cropped <- crop(rasters, shp_file)
    overlap <- terra::mask(cropped, shp_file)
    overlap_area <- cellSize(overlap, unit = "m")
    total_overlap_area <- sum(values(overlap_area), na.rm = TRUE)
    
    wf_area <- cellSize(wf, unit = "m")
    wf_area_number <- sum(values(wf_area), na.rm = TRUE)
    percentage_area <- (total_overlap_area * 100) / wf_area_number
    print(paste("Area overlapped was:", percentage_area))
    areas_overlapping[[i]] <- percentage_area
    
    # Transforming rasters into a dataframe !
    pb <- resample(rasters, bf, method = "near")
    st <- c(bf, pb)
    df <- as.data.frame(values(st)) %>%
      na.omit()
    
    if (nrow(df) != 0) {
      colnames(df) <- c("wf", "pb")
      df$PBname <- names(rst_list[[i]])
      df$PBname <- str_remove_all(df$PBname, "BurnSeverity_")
      df$PBname <- substr(df$PBname, 1, 17)
      df$OverlapArea <- percentage_area  # Add overlap area to data frame
    } else {
      cat(names(rst_list[[i]]), "\n")
    }
    
    if (nrow(df) >= 500) {
      df_list[[length(df_list) + 1]] <- df  # Add non-NULL data frame to df_list
      rlst[[length(rlst) + 1]] <- rasters
      
      name_list <- character(length(df_list))
      #changing list name of each item
      
      for (i in 1:length(df_list)) {
        df_lname <- df_list[[i]][[3]]
        conf_name <- unique(df_lname)
        name_list[i] <- conf_name
      }
      #using name_list to rename
      names(rlst) <- name_list

    }
  } else {
    next
  }
}


###############################################################################################################
#Regression plots for each prescribed burn
library(ggplot2)
library(grid)
library(cowplot)

# Function to remove rows with any negative values
remove_negatives <- function(df) {
  df[rowSums(df < 0) == 0, ]
}

resampled_df_list <- lapply(df_list, function(df) {
  df <- remove_negatives(df)
  df[sample(nrow(df), size = 0.1 * nrow(df)), ]
})


###############################################################################################################

# Second loop for summary plot 
conf_names <- c()
estimates <- c()
for (i in seq_along(resampled_df_list)) {
  df <- resampled_df_list[[i]]
  Prescribed_Burn <- df[[2]]
  Bush_Fire <- df[[1]]
  lmModel <- lm(Bush_Fire ~ Prescribed_Burn, data = df)
  estimate <- summary(lmModel)$coefficients["Prescribed_Burn", "Estimate"]
  names_df <- df[[3]]
  conf_name <- unique(names_df)
  
  conf_names <- c(conf_names, conf_name)
  estimates <- c(estimates, rep(estimate, length(conf_name)))
  
  
}

names(resampled_df_list) <- names(rlst)
plot_df <- data.frame(conf_name = conf_names, estimate = estimates)
plot_df$Year <- as.numeric(str_extract(plot_df$conf_name, "\\d{4}"))

#creating new columns
# Create intervals from the Year column
bins <- c(1987,1991,1996,2001,2006,2011,2016,2021,2026)
labels<- c('1987-1991',
           '1991-1996',
           '1996-2001',
           '2001-2006',
           '2006-2011',
           '2011-2016',
           '2016-2021',
           '2021-2026')
plot_df$Year_Interval <- cut(plot_df$Year, breaks = bins, labels = labels, right = FALSE)
plot_df$firetype <- firetype

head(plot_df)

p <- ggplot(plot_df, aes(x = conf_name, y = estimate, fill = firetype)) + 
  geom_bar(stat = "identity") +
  labs(title = "Summary Graph", x = "Fire type", y = "Coefficient Value", fill = "Fire type") +
  scale_fill_manual(values= c("gold",
                              "brown1",
                              "dimgray")) + 
  geom_text(aes(label = round(estimate, 2)), vjust = -1, size = 2) +
  theme(axis.text.x = element_blank())  

print(p)


plot_saved2 <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/plots/summary_severityclasses.png")
ggsave(plot_saved2, plot = p, height = 4, width = 6.5)