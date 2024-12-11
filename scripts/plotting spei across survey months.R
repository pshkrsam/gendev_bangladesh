# Load required libraries
library(terra)
library(RColorBrewer)
library(stringr)

# Set working directory to where your files are
setwd("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/exposure_spei/analysis/")

# Function to create custom color palette for SPEI values
create_spei_colors <- function() {
  colors <- c(
    "#FF0000",     # Extreme drought (SPEI < -2)
    "#FF8C00",     # Moderate drought (-1.99 < SPEI < -1.5)
    "#FFA500",     # Mild drought (-1.49 < SPEI < -1)
    "#FFFF00",     # Near normal (-0.99 < SPEI < 0.99)
    "#98FB98",     # Slightly wet (1 < SPEI < 1.49)
    "#4682B4",     # Moderately wet (1.5 < SPEI < 1.99)
    "#0000FF"      # Extremely wet (SPEI > 2)
  )
  return(colors)
}

# Function to plot SPEI rasters
plot_spei_rasters <- function() {
  
  # List all tif files in the output directory
  raster_files <- list.files("output", pattern = "\\.tif$", full.names = TRUE)
  
  if(length(raster_files) == 0) {
    stop("No .tif files found in the output directory")
  }
  
  # Create a data frame with filenames and proper month ordering
  file_info <- data.frame(
    filepath = raster_files,
    filename = basename(raster_files),
    stringsAsFactors = FALSE
  )
  
  # Extract month and year
  file_info$month <- substr(file_info$filename, 1, 3)
  file_info$year <- substr(file_info$filename, 4, 7)
  
  # Convert month abbreviations to numbers for sorting
  month_lookup <- setNames(1:12, tolower(substr(month.abb, 1, 3)))
  file_info$month_num <- month_lookup[tolower(file_info$month)]
  
  # Sort by year and month
  file_info <- file_info[order(file_info$year, file_info$month_num), ]
  
  # Take only the first 12 months
  file_info <- file_info[1:12, ]
  
  # Create color breaks and palette
  breaks <- c(-Inf, -1.83, -1.43, -1, -0.99, 0.99, 1.43, 1.83, Inf)
  colors <- create_spei_colors()
  
  # Set up the plotting layout
  layout(matrix(c(1:12, rep(13, 4)), nrow = 4, ncol = 4, byrow = TRUE),
         heights = c(rep(1, 3), 0.5))
  
  # Plot each raster
  for (i in 1:12) {
    # Read raster
    r <- rast(file_info$filepath[i])
    
    # Set margins for each plot
    par(mar = c(2, 2, 2, 2))
    
    # Plot with custom colors and breaks
    plot(r, 
         main = paste0(toupper(file_info$month[i]), " ", file_info$year[i]),
         breaks = breaks,
         col = colors,
         legend = FALSE,
         axes = FALSE)
  }
  
  # Add legend at the bottom
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", 
         legend = c("Extremely wet (>1.83)",
                    "Moderately wet (1.43-1.82)",
                    "Slightly wet (1-1.42)",
                    "Near normal (-0.99-0.99)",
                    "Mildly dry (-1.42--1)",
                    "Moderately dry (-1.43-1.82)",
                    "Extremely dry (<-1.83)"),
         fill = rev(colors),
         title = "SPEI Categories",
         bty = "n",
         horiz = TRUE,
         cex = 0.7)
}

# Run the plotting function
plot_spei_rasters()