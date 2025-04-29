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


tdir <- here::here("Data")
rst_dir <- file.path(tdir, "Clipped_rasters")
rst_files <- list.files(rst_dir, pattern = "\\.tif$", full.names = TRUE)
target_crs <- "EPSG:7850"

rst_list <- list()
for (raster_file in rst_files) {
  var_name <- tools::file_path_sans_ext(basename(raster_file))
  raster_obj <- rast(raster_file)  # Using terra::rast
  raster_obj <- project(raster_obj, target_crs, method = "near")
  rst_list[[var_name]] <- raster_obj
}
crs(rst_list[[1]])
wf <- rst_list$`OzCBI_FRK007_2024-11-01`
wf
#raster_list[c(54)] = NULL

# Load the raster file bushfire
sdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working"
bf <- rast(paste0(sdir, "\\Projects\\2024-11-06_BF2023-FRK007\\Data\\Clipped_rasters\\OzCBI_FRK007_2024-11-01.tif"))
# Project the raster to the CRS of shp using a valid method
plot(bf)
bf <- project(bf, target_crs, method = "near")
bf_20 <- bf
res(bf_20) <- 20 #20 m
bf <- terra::resample(bf, bf_20, method = "near")
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
      
      # #here to develop coverage
      # new_list <- list()
      # 
      # for(i in 1:length(df_list)){
      #   coverage <- unique(df_list[[i]][[4]])
      #   if(coverage >= 30){
      #     new_list[[length(new_list) + 1]] <- df_list[[i]]
      #   }
      # }
      
    }
  } else {
    next
  }
}


###############################################################################################################
###############################################################################################################
# # Creating regression model unique
#Dependent Variable (Response Variable): Bushfire occurrence
#Independent Variable (Predictor Variable): Prescribed burns 

summary(df_list[[1]])

predictor_variable = df_list[[1]][[2]]
response_variable= df_list[[1]][[1]]

ggplot(df_list[[1]], aes(x = predictor_variable , y = response_variable)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue")

lmModel = lm(response_variable ~ predictor_variable, data = df_list[[1]])

#summary(lmModel)$coefficients["predictor_variable","Estimate"]

#summary(lmModel)
#plot(lmModel)

plot(rst_list[[1]])
plot(bf, add = TRUE)
###

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

# First loop for plotting individual data frames
for (i in seq_along(resampled_df_list)) {
  df <- resampled_df_list[[i]]
  Prescribed_Burn <- df[[2]]
  Bush_Fire <- df[[1]]
  overlapped_n <- unique(df[[4]])
  
  lmModel <- lm(Bush_Fire ~ Prescribed_Burn, data = df)
  estimate <- summary(lmModel)$coefficients["Prescribed_Burn", "Estimate"]
  
  text <- paste("Correlation coefficient:", round(estimate, 2))
  text2 <- paste("Overlapping:",round(overlapped_n, 2),"%")
  
  p <- ggplot(df, aes(x = Prescribed_Burn, y = Bush_Fire)) +
    geom_point(aes(color = "Data Points"), size = 0.3) +
    geom_smooth(method = "lm", aes(color = "Regression Line", linetype = "Regression Line"), se = FALSE) +
    ggtitle(paste("DataFrame: ", name_list[i])) +
    labs(x = "Prescribed Burn", y = "Bush Fire") + 
    scale_color_manual(name = "Legend", values = c("Data Points" = "black", "Regression Line" = "blue")) +
    scale_linetype_manual(name = "Legend", values = c("Data Points" = "solid", "Regression Line" = "solid")) +
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid"))),
           linetype = "none") +
    theme(legend.position = "right",
          legend.spacing.x = unit(0.2, 'cm'),  
          legend.spacing.y = unit(0.2, 'cm'),
          legend.background = element_rect(fill = "gray")) 
  
  text_grob <- textGrob(text, gp = gpar(fontface = "bold",fontsize = 8))
  text_grob2 <- textGrob(text2, gp = gpar(fontface = "bold",fontsize = 8))
  
  combined_plot <- plot_grid(p, ggdraw() + draw_grob(text_grob, x = -1.9, y = 0.4) + 
                                           draw_grob(text_grob2, x = -2.2, y = 0.35),ncol = 2, rel_widths = c(10, 1))
  
  print(combined_plot)
  plot_saved <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/plots/", name_list[i], ".png")
  ggsave(plot_saved, plot = combined_plot, height = 4, width = 6.5)
}



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
plot_df <- data.frame(conf_name = conf_names, estimate = estimates)
plot_df$Year <- as.numeric(str_extract(plot_df$conf_name, "\\d{4}"))

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


p <- ggplot(plot_df, aes(x = conf_name, y = estimate, fill = Year_Interval)) +
  geom_bar(stat = "identity") +
  labs(title = "Summary Graph", x = "Prescribed Burns", y = "Coefficient Value",fill = "Years") +
  scale_fill_viridis_d(option = "viridis") +
  geom_text(aes(label = round(estimate, 2)), vjust = -1, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels

print(p)

plot_saved2 <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/plots/summary.png")
ggsave(plot_saved2, plot = p, height = 4, width = 6.5)
##################################################################################

##################################################################################
##stacking previous working 
# df_list <- list() 
# 
# for (i in 1:length(rst_list)) {
#   pb <- resample(rst_list[[i]], bf, method = "near")
#   st <- c(bf, pb)
#   df <- as.data.frame(values(st)) %>%
#     na.omit()
#   if (nrow(df) != 0) {
#     colnames(df) <- c("wf", "pb")
#     df$PBname <- names(rst_list[[i]])
#     df$PBname <- str_remove_all(df$PBname, "BurnSeverity_")
#     df$PBname <- substr(df$PBname, 1, 17)
#   } else {
#     cat(names(rst_list[[i]]), "\n")
#   }
#   if (nrow(df) >= 500) {
#     df_list[[length(df_list) + 1]] <- df  # Add non-NULL data frame to df_list
#   }
# }
# length(df_list)
# 

#################################################################################################
#DONT RUN !

# regression <- list()
# for (i in seq_along(df_list)) {
#   df <- df_list[[i]]
#   
#   Prescribed_Burn <- df[[2]]  #predictor_variable
#   Bush_Fire <- df[[1]]   #response_variable
#   
#   # Create the plot
#   p <- ggplot(df, aes(x = Prescribed_Burn, y = Bush_Fire)) +
#     geom_point() +
#     geom_smooth(method = "lm", col = "blue") +
#     ggtitle(paste("DataFrame", name_list[i]))
#   regression[i] <- p
#   # Print the plot
#   print(p)
#   
#   plot_saved<- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/plots/", name_list[i], ".png")
#   ggsave(plot_saved)
#   
# }
# library(ggplot2)
# library(grid)
# library(cowplot)
# 
# 
# resampled_df_list <- lapply(df_list, function(df) df[sample(nrow(df), size = 0.1 * nrow(df)), ])
# 
# for (i in seq_along(resampled_df_list)) {
#   df <- resampled_df_list[[i]]
#   Prescribed_Burn <- df[[2]]
#   Bush_Fire <- df[[1]]
#   lmModel <- lm(Bush_Fire ~ Prescribed_Burn, data = df)
#   estimate <- summary(lmModel)$coefficients["Prescribed_Burn", "Estimate"]
#   text <- paste("Coefficient:", round(estimate, 2))
#   
#   p <- ggplot(df, aes(x = Prescribed_Burn, y = Bush_Fire)) +
#     geom_point(aes(color = "Data Points"), size =0.3)  +
#     geom_smooth(method = "lm", aes(color = "Regression Line", linetype = "Regression Line"), se = FALSE) +
#     ggtitle(paste("DataFrame: ", name_list[i])) +
#     labs(x = "Prescribed Burn", y = "Bush Fire") + 
#     scale_color_manual(name = "Legend", values = c("Data Points" = "black", "Regression Line" = "blue")) +
#     scale_linetype_manual(name = "Legend", values = c("Data Points" = "solid", "Regression Line" = "solid")) +
#     guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid"))),
#            linetype = "none") +
#     theme(legend.position = "right",
#           legend.spacing.x = unit(0.2, 'cm'),  
#           legend.spacing.y = unit(0.2, 'cm'),
#           legend.background = element_rect(fill = "gray")) 
#   
#   text_grob <- textGrob(text, gp = gpar(fontface = "bold"))
#   
#   combined_plot <- plot_grid(p, ggdraw() + draw_grob(text_grob, x = -1.8, y = 0.4), ncol = 2, rel_widths = c(10, 1))
#   
#   print(combined_plot)
#   plot_saved <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/plots/", name_list[i], ".png")
#   ggsave(plot_saved, plot = combined_plot, height = 4, width = 6.5)
# }




#Creating summary graph with a legend color on it 
# conf_names <- c()
# estimates <- c()
# for (i in seq_along(resampled_df_list)) {
#   df <- resampled_df_list[[i]]
#   Prescribed_Burn <- df[[2]]
#   Bush_Fire <- df[[1]]
#   lmModel <- lm(Bush_Fire ~ Prescribed_Burn, data = df)
#   estimate <- summary(lmModel)$coefficients["Prescribed_Burn", "Estimate"]
#   names_df <- df[[3]]
#   conf_name <- unique(names_df)
#   
#   conf_names <- c(conf_names, conf_name)
#   estimates <- c(estimates, rep(estimate, length(conf_name)))
# }
# plot_df <- data.frame(conf_name = conf_names, estimate = estimates)
# 
# 
# p <- ggplot(plot_df, aes(x = conf_name, y = estimate, fill = conf_name)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Summary Graph", x = "Prescribed Burns", y = "Coefficient Value",fill = "Prescribed Burns names") +
#   scale_fill_discrete(name = "Legend") +
#   geom_text(aes(label = round(estimate, 2)),vjust = -1, size = 2) +
#   scale_fill_viridis_d(option="mako") +
#   theme(axis.text.x = element_blank(),
#         legend.text = element_text(size = 3))
# print(p)
# 
# 
# plot_saved2 <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/plots/summary.png")
# ggsave(plot_saved2, plot = p, height = 4, width = 6.5)
#####
#another way to do it 
# bars <-ggplot(plot_df, aes(x = conf_name, y = estimate, fill = conf_name)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Summary Graph", x = "Prescribed Burns", y = "Coefficient Value") +
#   geom_text(aes(label = round(estimate, 2)), vjust = -0.5)
# bars + scale_fill_brewer(palette = "Set3")
# 



