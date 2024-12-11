library(terra)
library(fs)

# Specify your paths

setwd("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/exposure_spei/analysis")

focus_dir <- "focus_months"
ref_dir <- "alt_sources"
preceding_dir <-"preceding_months"
output_dir <- "output"


# Get list of focus files
focus_files <- dir_ls(focus_dir, glob="*.tif")

# Function to create reference file path from focus file
get_ref_path <- function(focus_path, ref_dir) {
  base_name <- basename(focus_path)
  ref_name <- paste0("alt_", base_name)
  file.path(ref_dir, ref_name)
}

# Gap filling function
gap_fill <- function(focus_path, ref_path, output_path) {
  focus <- rast(focus_path)
  reference <- rast(ref_path)
  result <- cover(focus, reference)
  writeRaster(result, output_path, overwrite=TRUE)
  cat("Processed:", basename(focus_path), "\n")
}

# Process all files
for(focus_path in focus_files) {
  ref_path <- get_ref_path(focus_path, ref_dir)
  output_path <- file.path(output_dir, basename(focus_path))
  
  if(file.exists(ref_path)) {
    gap_fill(focus_path, ref_path, output_path)
  } else {
    cat("Warning: Missing reference file for", basename(focus_path), "\n")
  }
}
###############################################################################

# Step 1: Load required packages
library(terra)
library(fields)
library(ggplot2)
library(tidyr)
library(dplyr)

# Step 2: Read and check rasters
# Replace with your actual file paths
focus_rast <- rast("focus_months/oct2017.tif")
reference_rast <- rast("alt_sources/alt_oct2017.tif")

# Check raster properties
print("Focus raster:")
print(focus_rast)
print("Number of NA values in focus raster:")
print(sum(is.na(values(focus_rast))))

print("\nReference raster:")
print(reference_rast)

###############################################################################
# Step 3: Prepare data for kriging
# Get coordinates and values
total_cells <- ncell(focus_rast)
coords <- xyFromCell(focus_rast, 1:total_cells)
values_focus <- values(focus_rast)[,1]

# Create dataframes for known and prediction points
known_points <- data.frame(
  x = coords[!is.na(values_focus), 1],
  y = coords[!is.na(values_focus), 2],
  value = values_focus[!is.na(values_focus)]
)

pred_points <- data.frame(
  x = coords[is.na(values_focus), 1],
  y = coords[is.na(values_focus), 2]
)

# Print summary information
cat("Total cells:", total_cells, "\n")
cat("Known points:", nrow(known_points), "\n")
cat("Points to predict:", nrow(pred_points), "\n")

# Optional: Look at the structure of our data
str(known_points)
str(pred_points)


# Step 4 (revised): Perform kriging with proper NA filling
start_time <- Sys.time()

# Fit kriging model
cat("Fitting kriging model...\n")
fit <- try(fields::Krig(
  x = as.matrix(known_points[, c("x", "y")]),
  Y = known_points$value,
  theta = 50000
))

# Make predictions
cat("Making predictions...\n")
predictions <- predict(fit, xnew = as.matrix(pred_points[, c("x", "y")]))

# Create result raster and fill values
result_rast <- focus_rast  # create copy
na_cells <- which(is.na(values(result_rast)))  # Get indices of NA cells
values(result_rast)[na_cells] <- predictions   # Fill only NA cells

# Calculate elapsed time
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")
cat("\nKriging completed in:", round(elapsed, 2), "minutes\n")

# Verify results
print(result_rast)
print("Number of NA values in result:")
print(sum(is.na(values(result_rast))))

# Optional: Quick check of value ranges
cat("\nValue ranges in result raster:\n")
print(range(values(result_rast), na.rm=TRUE))



# Step 5: Create visualization
library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data frames for plotting
focus_df <- as.data.frame(focus_rast, xy=TRUE)
names(focus_df)[3] <- "value"
focus_df$type <- "Focus"

ref_df <- as.data.frame(reference_rast, xy=TRUE)
names(ref_df)[3] <- "value"
ref_df$type <- "Reference"

result_df <- as.data.frame(result_rast, xy=TRUE)
names(result_df)[3] <- "value"
result_df$type <- "Filled"

# Combine all data
all_data <- bind_rows(focus_df, ref_df, result_df)

# Create plot
p <- ggplot(all_data) +
  geom_raster(aes(x=x, y=y, fill=value)) +
  facet_wrap(~type, nrow=1) +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(-0.8, 2.5)) +
  coord_equal() +
  theme_minimal() +
  labs(title = "October 2017 SPEI-48 Comparison",
       fill = "SPEI-48")

# Display plot
print(p)

# Save outputs
ggsave("kriging_comparison.png", p, width=15, height=5, dpi=300)
writeRaster(result_rast, "filled_oct2017.tif", overwrite=TRUE)

################################################################################

library(terra)
library(fields)
library(ggplot2)
library(tidyr)
library(dplyr)

# Step 1: Read all data
focus_rast <- rast("focus_months/oct2017.tif")
reference_rast <- rast("alt_sources/alt_oct2017.tif")
# Add your shapefile path here
mask_shp <- vect("Bangladesh_shp_fixed.shp")

# Step 2: Create a mask
mask_rast <- rasterize(mask_shp, focus_rast)

# Step 3: Prepare kriging data (only for points within mask)
total_cells <- ncell(focus_rast)
coords <- xyFromCell(focus_rast, 1:total_cells)
values_focus <- values(focus_rast)[,1]
mask_values <- values(mask_rast)[,1]

# Create dataframes for known and prediction points (within mask only)
known_points <- data.frame(
  x = coords[!is.na(values_focus) & !is.na(mask_values), 1],
  y = coords[!is.na(values_focus) & !is.na(mask_values), 2],
  value = values_focus[!is.na(values_focus) & !is.na(mask_values)]
)

pred_points <- data.frame(
  x = coords[is.na(values_focus) & !is.na(mask_values), 1],
  y = coords[is.na(values_focus) & !is.na(mask_values), 2]
)

# Print summary
cat("Total cells:", total_cells, "\n")
cat("Known points:", nrow(known_points), "\n")
cat("Points to predict:", nrow(pred_points), "\n")

# Step 4: Perform kriging
start_time <- Sys.time()

# Fit kriging model
cat("Fitting kriging model...\n")
fit <- try(fields::Krig(
  x = as.matrix(known_points[, c("x", "y")]),
  Y = known_points$value,
  theta = 50000
))

# Make predictions
cat("Making predictions...\n")
predictions <- predict(fit, xnew = as.matrix(pred_points[, c("x", "y")]))

# Create result raster and fill values
result_rast <- focus_rast  # create copy
na_cells <- which(is.na(values(result_rast)) & !is.na(values(mask_rast)))
values(result_rast)[na_cells] <- predictions

# Mask the result to shapefile boundary
result_rast <- mask(result_rast, mask_shp)

# Calculate elapsed time
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")
cat("\nKriging completed in:", round(elapsed, 2), "minutes\n")

# Create visualization
focus_df <- as.data.frame(focus_rast, xy=TRUE)
names(focus_df)[3] <- "value"
focus_df$type <- "Focus"

ref_df <- as.data.frame(reference_rast, xy=TRUE)
names(ref_df)[3] <- "value"
ref_df$type <- "Reference"

result_df <- as.data.frame(result_rast, xy=TRUE)
names(result_df)[3] <- "value"
result_df$type <- "Filled"

# Combine all data
all_data <- bind_rows(focus_df, ref_df, result_df)

# Create plot
p <- ggplot(all_data) +
  geom_raster(aes(x=x, y=y, fill=value)) +
  facet_wrap(~type, nrow=1) +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(-0.8, 2.5)) +
  coord_equal() +
  theme_minimal() +
  labs(title = "October 2017 SPEI-48 Comparison",
       fill = "SPEI-48")

# Save outputs
print(p)
ggsave("kriging_comparison.png", p, width=15, height=5, dpi=300)
writeRaster(result_rast, "filled_oct2017.tif", overwrite=TRUE)







