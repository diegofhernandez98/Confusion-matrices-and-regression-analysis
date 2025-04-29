library(ggplot2)
library(caret) # Assuming you are using the caret package for confusionMatrix
library(gridExtra) # For arranging plots and text
library(grid) # For textGrob

conf_matrix_list <- list()

for (i in 1:length(df_list)) {
  first_column <- df_list[[i]][[1]]
  second_column <- df_list[[i]][[2]]
  dfnames <-unique(df_list[[i]][[3]])
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
  matrix <- names(conf_matrix_list)
  plt <- as.data.frame(conf_matrix_list[[i]]$table)
  p <- ggplot(plt, aes(Prediction, Reference, fill = Freq)) +
    geom_tile() + 
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "#009194") +
    labs(title = paste0(firetype[i],"-", matrix[i]), x = "Bushfire", y = "Prescribed Burn")
  print(p)
  plot_saved <- paste0("Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Projects/2024-11-06_BF2023-FRK007/Data/Cm/", matrix[i], ".png")
  ggsave(plot_saved, plot = p, height = 4, width = 6.5)
}
