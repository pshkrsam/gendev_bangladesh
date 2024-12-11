# Load required libraries
library(raster)
library(gstat)
library(sp)
library(rgdal)
library(zoo)
library(tidyverse)

#===============================================================================
# STEP 1: Data Loading and Initial Setup
#===============================================================================

# Function to load and prepare SPEI rasters
load_spei_data <- function(focus_month_path, preceding_months_paths, alt_source_path) {
  # Load the focus month with missing values
  focus_raster <- raster(focus_month_path)
  
  # Load preceding months (for temporal correlation)
  preceding_rasters <- stack(preceding_months_paths)
  
  # Load alternative source for the same month (for validation)
  alt_source <- raster(alt_source_path)
  
  # Ensure all rasters have the same extent and resolution
  list(
    focus = focus_raster,
    preceding = preceding_rasters,
    alternative = alt_source
  )
}

#===============================================================================
# STEP 2: Convert Raster Data to Spatial Points for Interpolation
#===============================================================================

prepare_spatial_points <- function(raster_data) {
  # Convert raster to points, maintaining spatial information
  coords <- coordinates(raster_data)
  values <- getValues(raster_data)
  
  # Create SpatialPointsDataFrame
  spdf <- SpatialPointsDataFrame(
    coords = coords,
    data = data.frame(spei = values),
    proj4string = CRS(projection(raster_data))
  )
  
  # Remove NaN points but keep their locations for later interpolation
  valid_points <- spdf[!is.na(spdf$spei), ]
  na_points <- spdf[is.na(spdf$spei), ]
  
  list(
    valid = valid_points,
    missing = na_points
  )
}

#===============================================================================
# STEP 3: Variogram Modeling and Interpolation
#===============================================================================

interpolate_missing_values <- function(valid_points, missing_points, 
                                       preceding_values, alt_source_values) {
  # Create variogram model incorporating temporal correlation
  vgm_model <- variogram(
    spei ~ preceding_mean + alt_source,
    data = valid_points,
    width = 1000  # Adjust based on your data resolution
  )
  
  # Fit variogram model
  fit_model <- fit.variogram(
    vgm_model,
    model = vgm(
      psill = var(valid_points$spei, na.rm = TRUE)/2,
      model = "Sph",
      range = mean(vgm_model$dist)/2
    )
  )
  
  # Perform kriging with external drift
  kriging_result <- krige(
    formula = spei ~ preceding_mean + alt_source,
    locations = valid_points,
    newdata = missing_points,
    model = fit_model
  )
  
  return(kriging_result)
}

#===============================================================================
# STEP 4: Main Processing Function
#===============================================================================

process_spei_month <- function(focus_month_path, preceding_months_paths, 
                               alt_source_path, output_path) {
  # Load all required data
  data <- load_spei_data(focus_month_path, preceding_months_paths, alt_source_path)
  
  # Prepare spatial points
  points <- prepare_spatial_points(data$focus)
  
  # Calculate mean of preceding months for temporal correlation
  preceding_mean <- calc(data$preceding, mean)
  points$valid$preceding_mean <- extract(preceding_mean, points$valid)
  points$missing$preceding_mean <- extract(preceding_mean, points$missing)
  
  # Extract alternative source values
  points$valid$alt_source <- extract(data$alternative, points$valid)
  points$missing$alt_source <- extract(data$alternative, points$missing)
  
  # Perform interpolation
  interpolated_values <- interpolate_missing_values(
    points$valid,
    points$missing,
    preceding_mean,
    data$alternative
  )
  
  # Merge original and interpolated values
  complete_raster <- data$focus
  complete_raster[is.na(complete_raster)] <- interpolated_values$var1.pred
  
  # Save results
  writeRaster(complete_raster, output_path, overwrite = TRUE)
  
  # Return validation metrics
  return(list(
    original_na_count = sum(is.na(getValues(data$focus))),
    remaining_na_count = sum(is.na(getValues(complete_raster))),
    rmse = calculate_rmse(complete_raster, data$alternative)
  ))
}

#===============================================================================
# STEP 5: Validation Function
#===============================================================================

calculate_rmse <- function(interpolated, reference) {
  # Calculate RMSE for non-NA overlapping areas
  valid_mask <- !is.na(interpolated) & !is.na(reference)
  diff <- interpolated[valid_mask] - reference[valid_mask]
  sqrt(mean(diff^2, na.rm = TRUE))
}


#===============================================================================
# Function to generate paths for month abbreviation named files
#===============================================================================

get_spei_paths <- function(base_dir) {
  # Define focus months
  focus_periods <- c(
    # 2017-2018 period
    "oct2017", "nov2017", "dec2017", "jan2018", "feb2018", "mar2018",
    # 2022 period
    "jun2022", "jul2022", "aug2022", "sep2022", "oct2022", "nov2022", "dec2022"
  )
  
  # Create lookup for preceding 3 months
  month_seq <- c("jan", "feb", "mar", "apr", "may", "jun", 
                 "jul", "aug", "sep", "oct", "nov", "dec")
  
  # Function to get previous 3 months
  get_prev_months <- function(month_year) {
    # Split into month and year
    month <- substr(month_year, 1, 3)
    year <- substr(month_year, 4, 7)
    
    # Find position in month sequence
    pos <- match(month, month_seq)
    
    # Get previous 3 months
    prev_pos <- pos - (1:3)
    
    # Handle year change
    prev_year <- as.numeric(year)
    prev_months <- month_seq[ifelse(prev_pos < 1, prev_pos + 12, prev_pos)]
    prev_years <- ifelse(prev_pos < 1, prev_year - 1, prev_year)
    
    # Combine month and year
    paste0(prev_months, prev_years)
  }
  
  # Generate paths for each focus month
  paths <- list(
    focus = character(),
    preceding = list(),
    alternative = character(),
    output = character()
  )
  
  for(period in focus_periods) {
    # Focus month path
    paths$focus <- c(
      paths$focus,
      file.path(base_dir, "focus", paste0(period, ".tif"))
    )
    
    # Preceding months
    prev_months <- get_prev_months(period)
    paths$preceding[[length(paths$preceding) + 1]] <- 
      file.path(base_dir, "preceding", paste0(prev_months, ".tif"))
    
    # Alternative source path
    paths$alternative <- c(
      paths$alternative,
      file.path(base_dir, "alternative", paste0("alt_", period, ".tif"))
    )
    
    # Output path
    paths$output <- c(
      paths$output,
      file.path(base_dir, "output", paste0(period, "_interpolated.tif"))
    )
  }
  
  return(paths)
}

#===============================================================================
# STEP 5: PAth Handling 
#===============================================================================
# Define the function with output directory creation
get_spei_paths <- function(base_dir) {
  # Create output directory if it doesn't exist
  output_dir <- file.path(base_dir, "output")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
    cat("Created output directory at:", output_dir, "\n")
  }
  
  # Rest of the function remains same
  focus_periods <- c(
    # 2017-2018 period
    "oct2017", "nov2017", "dec2017", "jan2018", "feb2018", "mar2018",
    # 2022 period
    "jun2022", "jul2022", "aug2022", "sep2022", "oct2022", "nov2022", "dec2022"
  )
  
  month_seq <- c("jan", "feb", "mar", "apr", "may", "jun", 
                 "jul", "aug", "sep", "oct", "nov", "dec")
  
  get_prev_months <- function(month_year) {
    month <- substr(month_year, 1, 3)
    year <- substr(month_year, 4, 7)
    pos <- match(month, month_seq)
    prev_pos <- pos - (1:3)
    prev_year <- as.numeric(year)
    prev_months <- month_seq[ifelse(prev_pos < 1, prev_pos + 12, prev_pos)]
    prev_years <- ifelse(prev_pos < 1, prev_year - 1, prev_year)
    paste0(prev_months, prev_years)
  }
  
  paths <- list(
    focus = character(),
    preceding = list(),
    alternative = character(),
    output = character()
  )
  
  for(period in focus_periods) {
    paths$focus <- c(
      paths$focus,
      file.path(base_dir, "focus_months", paste0(period, ".tif"))
    )
    
    prev_months <- get_prev_months(period)
    paths$preceding[[length(paths$preceding) + 1]] <- 
      file.path(base_dir, "preceding_months", paste0(prev_months, ".tif"))
    
    paths$alternative <- c(
      paths$alternative,
      file.path(base_dir, "alt_sources", paste0("alt_", period, ".tif"))
    )
    
    paths$output <- c(
      paths$output,
      file.path(output_dir, paste0(period, "_interpolated.tif"))
    )
  }
  
  return(paths)
}

# Use the function
base_dir <- "C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/exposure_spei/analysis"

# Get all paths (this will also create the output directory)
paths <- get_spei_paths(base_dir)

# Print paths to verify
cat("Focus months paths:\n")
print(paths$focus[1])  # Print first path of each type
cat("\nPreceding months paths (first set):\n")
print(paths$preceding[[1]])
cat("\nAlternative source paths:\n")
print(paths$alternative[1])
cat("\nOutput paths:\n")
print(paths$output[1])

#===============================================================================
# STEP 6A: Understanding the Process and Adding Visualization
#===============================================================================

# The interpolation process follows these steps for each focus month:
# 1. Load data
# 2. Create variogram model
# 3. Perform kriging
# 4. Validate results
# 5. Visualize results

# Let's modify the process_spei_month function to make this explicit and add visualization

process_spei_month <- function(focus_month_path, preceding_months_paths, 
                               alt_source_path, output_path) {
  
  #===============================================================================
  # STEP 6A: Data Loading and Preparation
  #===============================================================================
  
  # Load required packages
  library(raster)
  library(gstat)
  library(sp)
  library(rgdal)
  
  # Function for initial data loading and preparation
  prepare_spei_data <- function(focus_month_path, preceding_months_paths, alt_source_path) {
    # Load rasters
    cat("Loading rasters...\n")
    focus_raster <- raster(focus_month_path)
    preceding_stack <- stack(preceding_months_paths)
    alt_source <- raster(alt_source_path)
    
    # Convert focus raster to points
    cat("Converting to spatial points...\n")
    coords <- coordinates(focus_raster)
    values <- getValues(focus_raster)
    
    # Create spatial points dataframe
    spdf <- SpatialPointsDataFrame(
      coords = coords,
      data = data.frame(spei = values),
      proj4string = CRS(projection(focus_raster))
    )
    
    # Calculate mean of preceding months
    cat("Calculating preceding months mean...\n")
    preceding_mean <- calc(preceding_stack, mean)
    spdf$preceding_mean <- raster::extract(preceding_mean, spdf@coords)
    
    # Extract alternative source values
    cat("Extracting alternative source values...\n")
    spdf$alt_source <- raster::extract(alt_source, spdf@coords)
    
    # Separate valid and missing points
    cat("Separating valid and missing points...\n")
    valid_points <- spdf[!is.na(spdf$spei), ]
    missing_points <- spdf[is.na(spdf$spei), ]
    
    # Return list of prepared data
    return(list(
      focus_raster = focus_raster,
      preceding_mean = preceding_mean,
      alt_source = alt_source,
      valid_points = valid_points,
      missing_points = missing_points
    ))
  }
  
  # Test the data preparation for one month
  test_month_index <- 1
  
  # Try preparing data for the test month
  prepared_data <- prepare_spei_data(
    paths$focus[test_month_index],
    paths$preceding[[test_month_index]],
    paths$alternative[test_month_index]
  )
  
  # Print summary to verify
  cat("\nSummary of prepared data:\n")
  cat("Number of total points:", nrow(prepared_data$valid_points) + 
        nrow(prepared_data$missing_points), "\n")
  cat("Number of valid points:", nrow(prepared_data$valid_points), "\n")
  cat("Number of missing points:", nrow(prepared_data$missing_points), "\n")
  
  # Optional: Quick plot to verify data
  if(interactive()) {
    par(mfrow = c(1, 3))
    plot(prepared_data$focus_raster, main = "Focus Month")
    plot(prepared_data$preceding_mean, main = "Preceding Months Mean")
    plot(prepared_data$alt_source, main = "Alternative Source")
  }
  
  
  #===============================================================================
  # STEP 6B: Variogram Modeling and Kriging
  #===============================================================================
  
  # Load required packages explicitly
  library(gstat)
  library(sp)
  library(raster)
  
  # First, let's verify the structure of our prepared data
  cat("Checking prepared data structure...\n")
  print(class(prepared_data$valid_points))
  print(names(prepared_data$valid_points@data))
  
  # Now proceed with variogram modeling and kriging
  variogram_kriging <- function(prepared_data) {
    cat("Starting variogram modeling...\n")
    
    # Create empirical variogram
    cat("Creating empirical variogram...\n")
    v_emp <- gstat::variogram(
      spei ~ preceding_mean + alt_source,
      data = prepared_data$valid_points,
      width = 5000  # adjust based on your data resolution
    )
    
    # Fit variogram model
    cat("Fitting variogram model...\n")
    v_fit <- gstat::fit.variogram(
      v_emp,
      model = vgm(
        psill = var(prepared_data$valid_points$spei, na.rm = TRUE)/2,
        model = "Sph",  # spherical model
        range = mean(v_emp$dist)/2
      )
    )
    
    # Plot variogram
    png(file.path(dirname(paths$output[1]), "variogram_model.png"),
        width = 800, height = 600)
    plot(v_emp, v_fit, main = "Fitted Variogram Model")
    dev.off()
    
    # Perform kriging
    cat("Performing kriging interpolation...\n")
    kriging_result <- gstat::krige(
      formula = spei ~ preceding_mean + alt_source,
      locations = prepared_data$valid_points,
      newdata = prepared_data$missing_points,
      model = v_fit
    )
    
    # Create complete raster
    cat("Creating complete raster...\n")
    complete_raster <- prepared_data$focus_raster
    complete_raster[is.na(complete_raster)] <- kriging_result$var1.pred
    
    # Create comparison plot
    png(file.path(dirname(paths$output[1]), "interpolation_comparison.png"),
        width = 1200, height = 400)
    par(mfrow = c(1, 3))
    
    # Original data
    plot(prepared_data$focus_raster, 
         main = "Original SPEI",
         col = colorRampPalette(c("red", "white", "blue"))(100))
    
    # Interpolated data
    plot(complete_raster, 
         main = "Interpolated SPEI",
         col = colorRampPalette(c("red", "white", "blue"))(100))
    
    # Difference
    plot(complete_raster - prepared_data$focus_raster, 
         main = "Difference (Interpolated - Original)",
         col = colorRampPalette(c("red", "white", "blue"))(100))
    dev.off()
    
    # Return results
    return(list(
      variogram_model = v_fit,
      complete_raster = complete_raster,
      kriging_result = kriging_result,
      original_na_count = sum(is.na(getValues(prepared_data$focus_raster))),
      remaining_na_count = sum(is.na(getValues(complete_raster))),
      rmse = sqrt(mean((getValues(complete_raster) - 
                          getValues(prepared_data$alt_source))^2, na.rm = TRUE))
    ))
  }
  
  # Run variogram modeling and kriging with error handling
  tryCatch({
    interpolation_result <- variogram_kriging(prepared_data)
    
    # Print summary statistics
    cat("\nInterpolation Summary:\n")
    cat("Original NA count:", interpolation_result$original_na_count, "\n")
    cat("Remaining NA count:", interpolation_result$remaining_na_count, "\n")
    cat("RMSE with alternative source:", round(interpolation_result$rmse, 4), "\n")
    
    # Print variogram model parameters
    cat("\nVariogram Model Parameters:\n")
    print(interpolation_result$variogram_model)
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    cat("Debug information:\n")
    str(prepared_data)
  })