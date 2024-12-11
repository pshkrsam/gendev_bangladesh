library(terra)
library(gstat)
library(sp)
library(ggplot2)
library(tidyr)
library(dplyr)

spatiotemporal_kriging <- function(focus_rast, preceding_rasts) {
  start_time <- Sys.time()
  
  # Get coordinates
  coords <- xyFromCell(focus_rast, 1:ncell(focus_rast))
  
  # Create space-time dataset
  st_data <- data.frame()
  
  # Add focus month (time = 0)
  focus_vals <- values(focus_rast)[,1]
  st_data <- rbind(st_data, 
                   data.frame(x = coords[,1],
                              y = coords[,2],
                              time = 0,
                              value = focus_vals))
  
  # Add preceding months
  for(i in 1:length(preceding_rasts)) {
    month_vals <- values(preceding_rasts[[i]])[,1]
    st_data <- rbind(st_data,
                     data.frame(x = coords[,1],
                                y = coords[,2],
                                time = i,
                                value = month_vals))
  }
  
  # Remove NA values for variogram calculation
  st_data_clean <- st_data[!is.na(st_data$value),]
  
  # Convert to SpatialPointsDataFrame
  coordinates(st_data_clean) <- ~x+y
  
  cat("Total points:", nrow(st_data_clean), "\n")
  
  # Create gstat object
  g <- gstat(formula = value ~ 1, 
             data = st_data_clean)
  
  # Calculate sample variogram
  cat("Calculating variogram...\n")
  v <- variogram(g, cutoff = 100000, width = 5000)
  
  # Fit variogram model
  cat("Fitting variogram model...\n")
  v_fit <- fit.variogram(v, model = vgm(psill = var(st_data_clean$value),
                                        model = "Exp",
                                        range = 50000,
                                        nugget = 0))
  
  # Create prediction locations (only for focus month)
  pred_locations <- data.frame(
    x = coords[is.na(focus_vals), 1],
    y = coords[is.na(focus_vals), 2]
  )
  coordinates(pred_locations) <- ~x+y
  
  # Perform kriging
  cat("Performing kriging...\n")
  k <- krige(formula = value ~ 1, 
             locations = st_data_clean,
             newdata = pred_locations,
             model = v_fit)
  
  # Fill values in result raster
  result_rast <- focus_rast
  result_rast[is.na(result_rast)] <- k$var1.pred
  
  # Calculate elapsed time
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cat("\nKriging completed in:", round(elapsed, 2), "minutes\n")
  
  # Return results
  return(list(
    raster = result_rast,
    variogram = v_fit,
    kriging_output = k
  ))
}

# Main execution
# Read data
focus_rast <- rast("focus_months/oct2017.tif")
preceding_rasts <- list(
  rast("preceding_months/sep2017.tif"),
  rast("preceding_months/aug2017.tif"),
  rast("preceding_months/jul2017.tif")
)

# Perform kriging
cat("Starting kriging process...\n")
result <- spatiotemporal_kriging(focus_rast, preceding_rasts)

# Create visualization
focus_df <- as.data.frame(focus_rast, xy=TRUE)
names(focus_df)[3] <- "value"
focus_df$type <- "Original"

result_df <- as.data.frame(result$raster, xy=TRUE)
names(result_df)[3] <- "value"
result_df$type <- "Filled (Spatial)"

# Combine and plot
all_data <- bind_rows(focus_df, result_df)

# Create main plot
p1 <- ggplot(all_data) +
  geom_raster(aes(x=x, y=y, fill=value)) +
  facet_wrap(~type, nrow=1) +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(-0.8, 2.5)) +
  coord_equal() +
  theme_minimal() +
  labs(title = "October 2017 SPEI-48 Comparison",
       fill = "SPEI-48")

# Plot variogram
p2 <- plot(variogramLine(result$variogram, maxdist = 100000),
           type = 'l', 
           xlab = 'distance', 
           ylab = 'semivariance')

# Save outputs
print(p1)
print(p2)
ggsave("kriging_comparison.png", p1, width=15, height=5, dpi=300)
ggsave("variogram.png", p2, width=8, height=6, dpi=300)
writeRaster(result$raster, "filled_oct2017.tif", overwrite=TRUE)

# Print some statistics about the kriging
cat("\nKriging Statistics:\n")
cat("Variogram model:", result$variogram$model[2], "\n")
cat("Range:", round(result$variogram$range[2]), "meters\n")
cat("Nugget:", round(result$variogram$psill[1], 4), "\n")
cat("Partial sill:", round(result$variogram$psill[2], 4), "\n")