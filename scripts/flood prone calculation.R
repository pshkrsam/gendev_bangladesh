# Load required libraries
library(terra)  # for raster processing
library(tidyverse)  # for data manipulation
library(sf)  # for vector data

# Function to process flood data
process_flood_data <- function(input_tif, output_tif) {
  # Read the GeoTIFF with all bands
  flood_rast <- rast(input_tif)
  
  # Extract individual bands
  # Assuming bands are in the same order as GEE:
  # 1: flooded
  # 2: duration
  # 3: clear_views
  # 4: clear_perc
  # 5: jrc_perm_water
  
  flooded <- flood_rast[[1]]  # flood extent
  perm_water <- flood_rast[[5]]  # permanent water
  
  # Subtract permanent water from flood extent
  flood_prone <- flooded - perm_water
  
  # Set negative values to 0 (if any occur from the subtraction)
  flood_prone[flood_prone < 0] <- 0
  
  # Write the result
  writeRaster(flood_prone, 
              output_tif, 
              overwrite=TRUE,
              datatype="INT1U")  # 8-bit unsigned integer
  
  return(flood_prone)
}

# Function to create a simple map
plot_floods <- function(flood_prone) {
  # Create color palette
  cols <- colorRampPalette(c("white", "red"))(100)
  
  # Plot
  plot(flood_prone, 
       col = cols,
       main = "Flood-Prone Areas",
       legend = TRUE,
       plg = list(title = "Flood Prone\n(1 = yes, 0 = no)"))
}

# Example usage
setwd("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/exposure_spei/analysis")

if(TRUE) {  # Set to TRUE when ready to run
  # Replace with your input/output paths
  input_file <- "bangladesh_floods_utm.tif"
  output_file <- "bangladesh_flood_prone.tif"
  
  # Process the data
  flood_prone <- process_flood_data(input_file, output_file)
  
  # Create plot
  plot_floods(flood_prone)
  
  # Print summary statistics
  print("Summary of flood-prone areas:")
  print(freq(flood_prone))
}

# Optional: Calculate area of flood-prone regions
calculate_flood_area <- function(flood_prone) {
  # Get cell size in square kilometers
  cell_area <- prod(res(flood_prone)) / 1000000  # Convert m² to km²
  
  # Calculate frequencies
  freqs <- freq(flood_prone)
  
  # Calculate areas
  areas <- data.frame(
    value = freqs[,1],
    count = freqs[,2],
    area_km2 = freqs[,2] * cell_area
  )
  
  return(areas)
}