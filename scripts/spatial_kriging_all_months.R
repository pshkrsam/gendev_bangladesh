library(terra)
library(fields)
library(ggplot2)
library(tidyr)
library(dplyr)
library(fs)

# Set working directory
setwd("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/exposure_spei/analysis")

# Define directories
focus_dir <- "focus_months"
ref_dir <- "alt_sources"
output_dir <- "output"
plot_dir <- "plots"

# Create output directories if they don't exist
dir.create(output_dir, showWarnings = FALSE)
dir.create(plot_dir, showWarnings = FALSE)

# Get list of focus files
focus_files <- dir_ls(focus_dir, glob="*.tif")

# Function to create reference file path
get_ref_path <- function(focus_path, ref_dir) {
  base_name <- basename(focus_path)
  ref_name <- paste0("alt_", base_name)
  file.path(ref_dir, ref_name)
}

# Kriging function with mask
kriging_fill <- function(focus_path, ref_path, mask_shp) {
  # Read rasters
  focus_rast <- rast(focus_path)
  reference_rast <- rast(ref_path)
  
  # Create mask
  mask_rast <- rasterize(mask_shp, focus_rast)
  
  # Prepare kriging data
  coords <- xyFromCell(focus_rast, 1:ncell(focus_rast))
  values_focus <- values(focus_rast)[,1]
  mask_values <- values(mask_rast)[,1]
  
  # Create dataframes for known and prediction points
  known_points <- data.frame(
    x = coords[!is.na(values_focus) & !is.na(mask_values), 1],
    y = coords[!is.na(values_focus) & !is.na(mask_values), 2],
    value = values_focus[!is.na(values_focus) & !is.na(mask_values)]
  )
  
  pred_points <- data.frame(
    x = coords[is.na(values_focus) & !is.na(mask_values), 1],
    y = coords[is.na(values_focus) & !is.na(mask_values), 2]
  )
  
  # Perform kriging
  fit <- try(fields::Krig(
    x = as.matrix(known_points[, c("x", "y")]),
    Y = known_points$value,
    theta = 50000
  ))
  
  predictions <- predict(fit, xnew = as.matrix(pred_points[, c("x", "y")]))
  
  # Fill values
  result_rast <- focus_rast
  na_cells <- which(is.na(values(result_rast)) & !is.na(values(mask_rast)))
  values(result_rast)[na_cells] <- predictions
  
  # Mask result
  result_rast <- mask(result_rast, mask_shp)
  
  return(result_rast)
}

# Function to create and save plot
create_comparison_plot <- function(focus_rast, reference_rast, result_rast, month_name, plot_dir) {
  focus_df <- as.data.frame(focus_rast, xy=TRUE)
  names(focus_df)[3] <- "value"
  focus_df$type <- "Focus"
  
  ref_df <- as.data.frame(reference_rast, xy=TRUE)
  names(ref_df)[3] <- "value"
  ref_df$type <- "Reference"
  
  result_df <- as.data.frame(result_rast, xy=TRUE)
  names(result_df)[3] <- "value"
  result_df$type <- "Filled"
  
  all_data <- bind_rows(focus_df, ref_df, result_df)
  
  p <- ggplot(all_data) +
    geom_raster(aes(x=x, y=y, fill=value)) +
    facet_wrap(~type, nrow=1) +
    scale_fill_distiller(palette = "RdYlBu", 
                         limits = c(-0.8, 2.5)) +
    coord_equal() +
    theme_minimal() +
    labs(title = paste(month_name, "SPEI-48 Comparison"),
         fill = "SPEI-48")
  
  ggsave(file.path(plot_dir, paste0("kriging_comparison_", month_name, ".png")), 
         p, width=15, height=5, dpi=300)
}

# Read shapefile
mask_shp <- vect("Bangladesh_shp_fixed.shp")

# Process all files
for(focus_path in focus_files) {
  month_name <- tools::file_path_sans_ext(basename(focus_path))
  cat("\nProcessing", month_name, "...\n")
  
  # Get reference path
  ref_path <- get_ref_path(focus_path, ref_dir)
  
  if(file.exists(ref_path)) {
    # Start timing
    start_time <- Sys.time()
    
    # Perform kriging
    result_rast <- kriging_fill(focus_path, ref_path, mask_shp)
    
    # Save result
    writeRaster(result_rast, 
                file.path(output_dir, basename(focus_path)), 
                overwrite=TRUE)
    
    # Create and save plot
    create_comparison_plot(rast(focus_path), 
                           rast(ref_path), 
                           result_rast, 
                           month_name, 
                           plot_dir)
    
    # Calculate elapsed time
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    cat("Completed in:", round(elapsed, 2), "minutes\n")
    
  } else {
    cat("Warning: Missing reference file for", basename(focus_path), "\n")
  }
}

cat("\nAll processing completed!\n")