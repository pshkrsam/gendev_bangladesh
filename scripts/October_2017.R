# Load libraries
library(terra)
library(sf)
library(dplyr)
library(haven)
library(raster)
library(terra)
library(lubridate)
library(ggplot2)
library(ggspatial)
library(gstat)

#Set working directory
setwd("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/exposure_spei/analysis")

# Read the DHS data
dhs_data_combined <- read_dta("BDHS_combined.dta")
dhs_data_combined <- readRDS("dhs_data_combined_rds.rds")


###########################Uploading DHS data##################################
# Create the first dataset for 2017 and 2018
dhs_data_2017_18 <- subset(dhs_data_combined, year %in% c(2017, 2018))
# View the results
print("Data for 2017 and 2018:")
print(dhs_data_2017_18)


# Create the second dataset for 2022
dhs_data_2022 <- subset(dhs_data_combined, year == 2022)
print("Data for 2022:")
print(dhs_data_2022)


#####################Loading cluster coordinates#################################
# Load the BD cluster coordinate shapefile
bd_gps_2017<- st_read("BDGE7SFL/BDGE7SFL.shp")
bd_gps_2017

bd_gps_2022<- st_read("BDGE81FL/BDGE81FL.shp")
bd_gps_2022

#Some clusters contain unverified coordinates and 
#have been assigned a location of (0, 0). 
#Since we have no way of linking these clusters to environmental data, 
#we will remove them for this analysis using dplyr's filter() function:

# Remove empty geographies
bd_gps_2017 <- bd_gps_2017 |> filter(LATNUM != 0, LONGNUM != 0)
bd_gps_2022 <- bd_gps_2022 |> filter(LATNUM != 0, LONGNUM != 0)

#Setting cluster coordinates to EPSG:32646
bd_gps_2017_btm <- st_transform(bd_gps_2017,crs =32646)
bd_gps_2022_btm <- st_transform(bd_gps_2022,crs =32646)

#Finding missing DHS clusters
#2017
# Get the range of expected cluster numbers
expected_clusters <- seq(min(bd_gps_2017_btm$DHSCLUST), max(bd_gps_2017_btm$DHSCLUST))
# Find missing clusters
missing_clusters <- setdiff(expected_clusters, bd_gps_2017_btm$DHSCLUST)
# Print missing clusters
print(missing_clusters)

#2022
# Get the range of expected cluster numbers
expected_clusters <- seq(min(bd_gps_2022_btm$DHSCLUST), max(bd_gps_2022_btm$DHSCLUST))
# Find missing clusters
missing_clusters <- setdiff(expected_clusters, bd_gps_2022_btm$DHSCLUST)
# Print missing clusters
print(missing_clusters)


##################Load administrative boundary shapefile#########################
bd_borders <- st_read("Bangladesh_shp_fixed.shp", quiet=TRUE)
bd_districts<- st_read("ADM_2/geoBoundaries-BGD-ADM2.shp",quiet = TRUE)
bd_subdistricts <- st_read("ADM_3/geoBoundaries-BGD-ADM3.shp",quiet = TRUE)

#Setting shapefiles to EPSG:32646
bd_borders_btm <-st_transform(bd_borders, crs = 32646)
bd_districts_btm <-st_transform(bd_districts, crs = 32646)
bd_subdistricts_btm <-st_transform(bd_subdistricts, crs = 32646)

#########################Loading SPEI Rasters###################################
#Loading raster data for October 2017

spei_oct2017<- rast("output/oct2017.tif")
spei_oct2017 <- project (spei_oct2017, "EPSG:32646")

summary(values(spei_oct2017))
plot(spei_oct2017, col = terrain.colors(100), main = "SPEI Oct 2017")

#Create map of SPEI for October 2017

library(ggplot2)
library(ggspatial)
library(terra)
library(sf)

# Ensure all spatial objects are in the same CRS
bd_borders <- st_transform(bd_borders, crs(spei_oct2017))
bd_gps_2017 <- st_transform(bd_gps_2017, crs(spei_oct2017))

# Color palette
# Categorize raster values using cut
spei_categorized <- classify(spei_oct2017, 
                                   rcl = matrix(c(-Inf, -1.83, 1,    # Extreme drought
                                                  -2, -1.43, 2,    # Moderate drought
                                                  -1.5, -1, 3,    # Mild drought
                                                  -1, 0.99, 4,    # Near normal
                                                  0.99, 1.43, 5,  # Slightly wet
                                                  1.43, 1.83, 6,  # Moderately wet
                                                  1.83, Inf, 7),  # Extremely wet
                                                ncol = 3, byrow = TRUE))        #rcl is the reclassification matrix that defines the value ranges and assigns a unique ID for each interval.

spei_categorized <- project (spei_categorized, "EPSG:32646")

# Convert the reclassified raster to a data.frame
spei_categorized_df <- as.data.frame(spei_categorized, xy = TRUE)

# Rename the column for reclassified values
colnames(spei_categorized_df)[3] <- "category"


# Define discrete colors
colors <- c(
  "1" = "#FF0000",  # Extreme drought
  "2" = "#FF8C00",  # Moderate drought
  "3" = "#FFA500",  # Mild drought
  "4" = "#FFFF00",  # Near normal
  "5" = "#98FB98",  # Slightly wet
  "6" = "#4682B4",  # Moderately wet
  "7" = "#0000FF"   # Extremely wet
)



# Create the plot using geom_tile()
ggplot() +
  # Add raster layer
  layer_spatial(data = spei_oct2017, alpha = 1, na.rm = TRUE) +
  
  # Add border layer
  layer_spatial(data = bd_borders$geometry, fill = NA, color = "#888888") +
  
  # Add point layer
  layer_spatial(data = bd_gps_2017, color = "black", size = 1, alpha = 0.4) +
  
  # Apply the custom color palette and breaks
  scale_fill_gradient2(
    low = "#D7191C",     # Red for drought
    mid = "white",       # White for normal
    high = "#2C7BB6",    # Blue for wet
    midpoint = 0,        # Center at SPEI = 0
    name="SPEI-48",
    na.value = "transparent"
  ) +
  
  # Add titles and labels
  labs(
    title = "Bangladesh SPEI-48 for October 2017",
    subtitle = "SPEI-48 data with DHS cluster locations",
    caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA"
  ) +
  
  # Add a scale bar
  annotation_scale(
    location = "br", style = "ticks",
    text_col = "#999999", line_col = "#999999"
  ) +
  
  # Add a north arrow
  annotation_north_arrow(
    location = "tr", 
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Use a minimal theme
  theme_minimal()



########################Cluster Buffers########################################

# Project cluster locations to the  reference system
st_geometry(bd_gps_2017_btm)

# Apply buffering based on urban/rural status
bd_gps_2017_btm <- bd_gps_2017_btm %>%
  mutate(buffer_distance = ifelse(URBAN_RURA=="U", 2000, 5000)) %>% # Set buffer distance
  mutate(geometry = st_geometry(st_buffer(geometry, dist = buffer_distance)))

# Check the updated geometries
st_geometry(bd_gps_2017_btm)


ggplot() +
  # Add raster layer
  layer_spatial(data = spei_oct2017, alpha = 1, na.rm = TRUE) +
  
  # Add border layer
  layer_spatial(data = bd_borders$geometry, fill = NA, color = "#888888") +
  
  # Add point layer
  layer_spatial(data = bd_gps_2017, color = "black", size = 1, alpha = 0.4) +
  
  # Apply the custom color palette and breaks
  scale_fill_gradient2(
    low = "#D7191C",     # Red for drought
    mid = "white",       # White for normal
    high = "#2C7BB6",    # Blue for wet
    midpoint = 0,        # Center at SPEI = 0
    name="SPEI-48",
    na.value = "transparent"
  )+
  
  # Add titles and labels
  labs(
    title = "Bangladesh SPEI-48 for October 2017",
    subtitle = "SPEI-48 data with DHS cluster locations",
    caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA"
  ) +
  
  # Add a scale bar
  annotation_scale(
    location = "br", style = "ticks",
    text_col = "#999999", line_col = "#999999"
  ) +
  
  # Add a north arrow
  annotation_north_arrow(
    location = "tr", 
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Use a minimal theme
  theme_minimal()

####################Example Cluster#############################################


clust <- bd_gps_2017_btm |> 
  filter(DHSCLUST == 107)               #selecting a rural cluster to highlight SPEI calculation at cluster level

spei_clust <- crop(spei_oct2017, clust, snap = "out")

st_crs(clust)          # CRS of the sf object
crs(spei_oct2017) # CRS of the raster

# If they differ, reproject the sf object to match the raster
# Check if the CRS of the raster and vector are the same
if (!st_crs(clust) == st_crs(spei_oct2017)) {
  # Transform the vector CRS to match the raster CRS
  clust <- st_transform(clust, st_crs(oct_2017_spei_btm)$wkt)
}



ggplot() +
  # Add raster layer
  layer_spatial(data = spei_clust, alpha = 1, na.rm = TRUE) +
  
  # Add border layer
  layer_spatial(data = clust, fill = NA, alpha = 0, linewidth = 0.5, color = "white") +
  
 
  # Apply the custom color palette and breaks
  scale_fill_gradient2(
    low = "#D7191C",     # Red for drought
    mid = "white",       # White for normal
    high = "#2C7BB6",    # Blue for wet
    midpoint = 0,        # Center at SPEI = 0
    name="SPEI-48",
    na.value = "transparent"
  ) +
  
  # Add titles and labels
  labs(
    title = "Bangladesh SPEI-48 for October 2017",
    subtitle = "SPEI-48 data for DHSCLUST 107",
    caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA"
  ) +
  
  # Add a scale bar
  annotation_scale(
    location = "br", style = "ticks",
    text_col = "#999999", line_col = "#999999"
  ) +
  
  # Add a north arrow
  annotation_north_arrow(
    location = "tr", 
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Use a minimal theme
  theme_minimal()


extract(spei_oct2017, clust)


# New SpatRaster with same extent as spei_`month', but with empty values
ext_cells <- rast(spei_oct2017, vals = NA)

# ID cell locations for each cell that is extracted
cell_idx <- extract(ext_cells, clust, cells = TRUE)$cell

ext_cells[cell_idx] <- spei_oct2017[cell_idx]

ggplot() + 
  layer_spatial(
    crop(ext_cells, clust, snap = "out"), 
    alpha = 0.8, 
    na.rm = TRUE
  ) +
  layer_spatial(clust, alpha = 0, linewidth = 0.5, color = "black") +
  scale_fill_gradient2(
    low = "#D7191C", mid = "white", high = "#2C7BB6", midpoint = 0
  ) 

clust_spei <- extract(spei_oct2017, clust, touches = TRUE)
clust_spei

clust_spei <- extract(spei_oct2017, clust, weights = TRUE)
clust_spei

# Extract area-weighted average for all cells covered by `clust`

extract(
  spei_oct2017,
  clust,
  fun = "mean", # Average all extracted values
  weights = TRUE # Use area weights
)


# Convert sf to SpatVector if necessary
bd_gps_2017_btm_vect <- vect(bd_gps_2017_btm)


# Extract mean SPEI values at points
clust_means <- terra::extract(
  spei_oct2017,           # Raster
  bd_gps_2017_btm_vect,           # Points
  fun = mean,                     # Aggregation function
  weights = TRUE,                 # Apply weights for raster cells
  na.rm = TRUE                    # Ignore NA values
)

# Count total null values in the entire dataframe
total_null_values <- sum(is.na(clust_means))
print(total_null_values)

# Add a new column to highlight rows with null values
clust_means$has_null <- apply(clust_means, 1, function(x) any(is.na(x)))

# View highlighted rows
highlighted_rows <- clust_means[clust_means$has_null == TRUE, ]
print(highlighted_rows)

#Mean
mean_spei_oct2017 <- mean(clust_means$"Standardized Precipitation-Evapotranspiration Index", na.rm = TRUE)
print(mean_spei_oct2017)

#Range
range_spei_oct2017 <- range(clust_means$"Standardized Precipitation-Evapotranspiration Index", na.rm = TRUE)

# Print the range
print(range_spei_oct2017)

clust_means <- clust_means %>% rename(spei = `Standardized Precipitation-Evapotranspiration Index`)

#Spatially interpolating missing values in the clust means

# Convert cluster means to spatial visualization
bd_gps_2017_btm$spei_oct <- clust_means$spei

ggplot() +
  geom_sf(data = bd_borders_btm, fill = NA, color = "#888888") +
  geom_sf(data = bd_gps_2017_btm, aes(color = spei_oct), size = 2) +
  scale_color_gradient2(
    low = "#D7191C",
    mid = "white",
    high = "#2C7BB6",
    midpoint = 0,
    name = "SPEI-48"
  ) +
  labs(title = "SPEI Cluster Means - October 2017",
       subtitle = "Mean SPEI-48 data for clusters extracted",
       caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA") +
  theme_minimal()


bd_gps_2017$spei_oct <- clust_means$spei

cluster_spei_oct <- data.frame(
  DHSCLUST = bd_gps_2017$DHSCLUST,
  spei_oct = bd_gps_2017$spei_oct,
  x = st_coordinates(bd_gps_2017)[,1],
  y = st_coordinates(bd_gps_2017)[,2]
)

# Check missing values
print(paste("Number of missing values:", sum(is.na(cluster_spei_oct$spei_oct))))

# Convert to spatial points for interpolation
coordinates(cluster_spei_oct) <- ~x+y

library(gstat)
# Create IDW model using non-missing values (Inverse-Distance Weighting)
idw_model <- gstat(
  formula = spei_oct ~ 1,
  locations = cluster_spei_oct[!is.na(cluster_spei_oct$spei_oct),],
  nmax = 5,
  set = list(idp = 2)
)

# Predict missing values
missing_idx <- is.na(cluster_spei_oct$spei_oct)
if(sum(missing_idx) > 0) {
  predicted <- predict(idw_model, cluster_spei_oct[missing_idx,])
  cluster_spei_oct$spei_oct[missing_idx] <- predicted$var1.pred
}

print(paste("Number of missing values:", sum(is.na(cluster_spei_oct$spei_oct))))


# Convert interpolated values back to original shapefile
bd_gps_2017_btm$spei_oct_interpolated <- cluster_spei_oct$spei_oct

# Visualize final interpolated values
ggplot()+
  geom_sf(data = bd_borders_btm, fill = NA, color = "#888888") +
  geom_sf(data = bd_gps_2017_btm, aes(color = spei_oct_interpolated), size = 2) +
  scale_color_gradient2(
    low = "#D7191C",
    mid = "white",
    high = "#2C7BB6",
    midpoint = 0,
    name = "SPEI-48"
  ) +
  labs(title = "SPEI Cluster Means with Interpolated Values - October 2017",
       subtitle = "Mean SPEI-48 data for clusters extracted",
       caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA") +
  theme_minimal()

# Check final dataset
print("Summary of final interpolated SPEI values:")
summary(bd_gps_2017_btm$spei_oct_interpolated)
print("Number of missing values after interpolation:")
sum(is.na(bd_gps_2017_btm$spei_oct_interpolated))

summary(bd_gps_2017_btm$spei_oct_interpolated)


##########Converting the process above into a function#########################

# Define the months for each survey round
months_2017_18 <- c("oct2017", "nov2017", "dec2017", "jan2018", "feb2018", "mar2018")
months_2022 <- c("jun2022", "jul2022", "aug2022", "sep2022", "oct2022", "nov2022", "dec2022")

# Verify the vectors
print("2017-18 months:")
print(months_2017_18)
print("2022 months:")
print(months_2022)


# Function definition
process_monthly_spei <- function(month_year, cluster_sf, cluster_sf2) {
  # Load and process raster
  spei_raster <- rast(paste0("output/", month_year, ".tif"))
  spei_raster <- project(spei_raster, "EPSG:32646")
  
  # Convert sf to SpatVector
  cluster_vect <- vect(cluster_sf)
  
  # Extract means
  clust_means <- terra::extract(
    spei_raster,
    cluster_vect,
    fun = mean,
    weights = TRUE,
    na.rm = TRUE
  )
  
  # Rename SPEI column
  clust_means <- clust_means %>% 
    rename(spei = `Standardized Precipitation-Evapotranspiration Index`)
  
  # Create interpolation dataframe
  cluster_spei <- data.frame(
    DHSCLUST = cluster_sf2$DHSCLUST,
    spei = clust_means$spei,
    x = st_coordinates(st_centroid(cluster_sf2))[,1],
    y = st_coordinates(st_centroid(cluster_sf2))[,2]
  )
  
  # Interpolate missing values
  coordinates(cluster_spei) <- ~x+y
  
  # Create IDW model
  idw_model <- gstat(
    formula = spei ~ 1,
    locations = cluster_spei[!is.na(cluster_spei$spei),],
    nmax = 5,
    set = list(idp = 2)
  )
  
  # Predict missing values
  missing_idx <- is.na(cluster_spei$spei)
  if(sum(missing_idx) > 0) {
    predicted <- predict(idw_model, cluster_spei[missing_idx,])
    cluster_spei$spei[missing_idx] <- predicted$var1.pred
  }
  
  return(cluster_spei$spei)
}

# Test with November 2017
test_result <- process_monthly_spei("nov2017", bd_gps_2017_btm, bd_gps_2017)

# Check the results
print("Summary of test result:")
summary(test_result)
print("Number of missing values:")
sum(is.na(test_result))


################Doing it for the remaining months in 2017-18###################

# Process 2017-18 months
for(month in months_2017_18) {
  print(paste("Processing", month))
  col_name <- paste0("spei_", month)
  bd_gps_2017_btm[[col_name]] <- process_monthly_spei(month, bd_gps_2017_btm,bd_gps_2017)
}

# Verify 2017-18 results
print("2017-18 SPEI columns:")
names(bd_gps_2017_btm)[grep("spei_", names(bd_gps_2017_btm))]

#Checking null values in interpolated values

# Count total null values in the entire dataframe
print(paste("Number of missing values in :", sum(is.na(bd_gps_2017_btm$spei_oct2017))))
print(paste("Number of missing values in :", sum(is.na(bd_gps_2017_btm$spei_feb2018))))


summary(bd_gps_2017_btm$spei_oct2017)
summary(bd_gps_2017_btm$spei_nov2017)
summary(bd_gps_2017_btm$spei_dec2017)
summary(bd_gps_2017_btm$spei_jan2017)
summary(bd_gps_2017_btm$spei_feb2018)
summary(bd_gps_2017_btm$spei_mar2018)

#################Doing the same for 2022 BDHS##########################################


# Project cluster locations to the  reference system
st_geometry(bd_gps_2022_btm)

# Apply buffering based on urban/rural status
bd_gps_2022_btm <- bd_gps_2022_btm %>%
  mutate(buffer_distance = ifelse(URBAN_RURA=="U", 2000, 5000)) %>% # Set buffer distance
  mutate(geometry = st_geometry(st_buffer(geometry, dist = buffer_distance)))

# Check the updated geometries
st_geometry(bd_gps_2022_btm)



# Process 2022 months
for(month in months_2022) {
  print(paste("Processing", month))
  col_name <- paste0("spei_", month)
  bd_gps_2022_btm[[col_name]] <- process_monthly_spei(month, bd_gps_2022_btm, bd_gps_2022)
}

# Verify 2022 results
print("2022 SPEI columns:")
names(bd_gps_2022_btm)[grep("spei_", names(bd_gps_2022_btm))]

summary(bd_gps_2022_btm$spei_jun2022)
summary(bd_gps_2022_btm$spei_jul2022)
summary(bd_gps_2022_btm$spei_aug2022)
summary(bd_gps_2022_btm$spei_sep2022)
summary(bd_gps_2022_btm$spei_oct2022)
summary(bd_gps_2022_btm$spei_nov2022)
summary(bd_gps_2022_btm$spei_dec2022)

###################Faceted plots for 2017-18 and 2022 waves#####################

# Function to convert raster to dataframe with month label
raster_to_df <- function(raster, month_label) {
  df <- as.data.frame(raster, xy = TRUE)
  names(df)[3] <- "value"  # Rename the value column
  df$month <- month_label  # Add month identifier
  return(df)
}

# Process 2017-18 rasters with ordered months
spei_2017_18_dfs <- list()
for(i in seq_along(months_2017_18)) {
  rast <- load_spei_raster(months_2017_18[i])
  spei_2017_18_dfs[[i]] <- raster_to_df(rast, months_2017_18[i])
}

# Combine and set factor levels for 2017-18
spei_2017_18_combined <- do.call(rbind, spei_2017_18_dfs)
spei_2017_18_combined$month <- factor(spei_2017_18_combined$month, 
                                      levels = months_2017_18,  # This ensures chronological order
                                      labels = c("October 2017", "November 2017", "December 2017",
                                                 "January 2018", "February 2018", "March 2018"))

# Same for 2022
spei_2022_dfs <- list()
for(i in seq_along(months_2022)) {
  rast <- load_spei_raster(months_2022[i])
  spei_2022_dfs[[i]] <- raster_to_df(rast, months_2022[i])
}

# Combine and set factor levels for 2022
spei_2022_combined <- do.call(rbind, spei_2022_dfs)
spei_2022_combined$month <- factor(spei_2022_combined$month, 
                                   levels = months_2022,  # This ensures chronological order
                                   labels = c("June 2022", "July 2022", "August 2022", 
                                              "September 2022", "October 2022", "November 2022", 
                                              "December 2022"))


# Create 2017-18 plot
plot_2017_18 <- ggplot() +
  geom_tile(data = spei_2017_18_combined, aes(x = x, y = y, fill = value)) +
  facet_wrap(~month, ncol = 3) +
  geom_sf(data = bd_borders_btm, fill = NA, color = "#888888") +
  geom_sf(data = bd_gps_2017_btm, size = 0.5, alpha = 0.4) +
  scale_fill_gradient2(
    low = "#D7191C",
    mid = "white",
    high = "#2C7BB6",
    midpoint = 0,
    name = "SPEI-48",
    na.value = "transparent"
  ) +
  labs(
    title = "Bangladesh SPEI-48 Values (2017-18)",
    subtitle = "Monthly SPEI-48 data with DHS cluster locations",
    caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

plot_2022 <- ggplot() +
  geom_tile(data = spei_2022_combined, aes(x = x, y = y, fill = value)) +
  facet_wrap(~month, ncol = 4) +
  geom_sf(data = bd_borders_btm, fill = NA, color = "#888888") +
  geom_sf(data = bd_gps_2022_btm, size = 0.5, alpha = 0.4) +
  scale_fill_gradient2(
    low = "#D7191C",
    mid = "white",
    high = "#2C7BB6",
    midpoint = 0,
    name = "SPEI-48",
    na.value = "transparent"
  ) +
  labs(
    title = "Bangladesh SPEI-48 Values (2022)",
    subtitle = "Monthly SPEI-48 data with DHS cluster locations",
    caption = "Source: DHS Program and Gridded SPEI-48 data from CEDA"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# Display plots
print(plot_2017_18)
print(plot_2022)

# Save plots
ggsave("spei_2017_18_monthly.png", plot_2017_18, bg="white",width = 12, height = 8)
ggsave("spei_2022_monthly.png", plot_2022, bg="white",width = 15, height = 10)


#################Integrating SPEI and other spatial covariates into DHS data###################
bd_gps_2017_btm <- bd_gps_2017_btm[, !names(bd_gps_2017_btm) %in% c("spei_oct", "spei_oct_interpolated")] #Should have 28 variables now


spatial_covariates_2017 <- read.csv("gc/gc2017.csv")
spatial_covariates_2022 <- read.csv("gc/gc2022.csv")


#Merging SPEI data

# First check the structure of survey dates in DHS data
print("Unique survey months in DHS data:")
table(dhs_data_combined$v007, dhs_data_combined$v006)


###########################Saving files########################################

library(sf)
st_write(bd_gps_2017_btm, "bd_gps_2017_btm.shp")
st_write(bd_gps_2022_btm, "bd_gps_2022_btm.shp")


saveRDS(dhs_data_combined, "dhs_data_combined_rds.rds")

library(haven)
write_dta(dhs_data_combined, "dhs_data_combined.dta", version = 13)
write.csv(dhs_data_combined, "dhs_data_combined.csv", row.names = FALSE)


##################October2017###################################################

bd_gps_2017_btm <- st_read("bd_gps_2017_btm.shp")
bd_gps_2022_btm <- st_read("bd_gps_2022_btm.shp")

# Create subset of October 2017 observations
oct2017_dhs <- dhs_data_combined %>%
  filter(v007 == 2017, v006 == 10)

# Create dataframe of October 2017 SPEI values
oct2017_spei <- bd_gps_2017_btm %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  dplyr::select(DHSCLUS, sp_c2017)

#Rename v001 to DHSCLUST in DHS data

oct2017_dhs <- oct2017_dhs %>% rename(DHSCLUS = DHSCLUST)

# Merge October 2017 data
oct2017_merged <- oct2017_dhs %>%
  left_join(oct2017_spei, by = "DHSCLUS")

# Verify merge
print("Number of October 2017 observations:")
nrow(oct2017_dhs)
print("Number of observations after merge:")
nrow(oct2017_merged)
print("Number of missing SPEI values:")
sum(is.na(oct2017_merged$spei_oct2017))

# Check a few rows to verify merge
print("Sample of merged data:")
head(oct2017_merged %>% dplyr::select(DHSCLUS, v007, v006, oct2017_spei))

##################All months in 2017-18###################################################

#rm(oct2017_dhs, oct2017_spei, plot_2017_18,plot_2022, bd_gps_2017_btm_vect, clust, clust_means, clust_spei)  #Reducing pressure on memory
#rm(spei_2017_18_combined, spei_2022_combined, spei_2017_18_dfs, spei_2022_dfs, spei_2022_rasters, spei_adjusted)  #Reducing pressure on memory

# Create subset of November 2017 observations
nov2017_dhs <- dhs_data_2017_18[dhs_data_2017_18$v007 == 2017 & dhs_data_2017_18$v006 == 11, ]

# Create dataframe of November 2017 SPEI values
nov2017_spei <- as.data.frame(bd_gps_2017_btm)
nov2017_spei <- nov2017_spei[, c("DHSCLUST", "spei_nov2017")]

# Rename v001 to DHSCLUST in DHS data
names(nov2017_dhs)[names(nov2017_dhs) == "v001"] <- "DHSCLUST"

# Merge November 2017 data using base R merge function
nov2017_merged <- merge(nov2017_dhs, nov2017_spei, by = "DHSCLUST", all.x = TRUE)

# Verify merge
cat("Number of November 2017 observations:\n")
print(nrow(nov2017_dhs))
cat("Number of observations after merge:\n")
print(nrow(nov2017_merged))
cat("Number of missing SPEI values:\n")
print(sum(is.na(nov2017_merged$spei_nov2017)))

# Check a few rows to verify merge
cat("Sample of merged data:\n")
print(head(nov2017_merged[, c("DHSCLUST", "v007", "v006", "spei_nov2017")]))


# Create vector of months and corresponding SPEI column names
months_data <- data.frame(
  year = c(2017, 2017, 2017, 2018, 2018, 2018),
  month = c(10, 11, 12, 1, 2, 3),
  spei_col = c("sp_c2017", "sp_n2017", "sp_d2017", 
               "sp_j2018", "sp_f2018", "sp_m2018")
)

# Create empty list to store merged datasets
merged_datasets <- list()

# Loop through each month
for(i in 1:nrow(months_data)) {
  # Get current month's info
  current_year <- months_data$year[i]
  current_month <- months_data$month[i]
  current_spei <- months_data$spei_col[i]
  
  # Get DHS data for current month
  month_dhs <- dhs_data_combined %>%
    filter(v007 == current_year, v006 == current_month) %>%
    rename(DHSCLUS = v001)
  
  # Get SPEI data
  month_spei <- bd_gps_2017_btm %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    dplyr::select(DHSCLUS, !!current_spei)
  
  # Merge
  month_merged <- month_dhs %>%
    left_join(month_spei, by = "DHSCLUS")
  
  # Store in list
  merged_datasets[[i]] <- month_merged
  
  # Print verification
  cat(sprintf("\nProcessing %d-%d:\n", current_year, current_month))
  cat(sprintf("Original observations: %d\n", nrow(month_dhs)))
  cat(sprintf("Merged observations: %d\n", nrow(month_merged)))
  cat(sprintf("Missing SPEI values: %d\n", sum(is.na(month_merged[[current_spei]]))))
}

# Combine all datasets
all_merged_data_2017 <- bind_rows(merged_datasets)

# Final verification
print("\nFinal merged dataset:")
print(paste("Total observations:", nrow(all_merged_data_2017)))

saveRDS(all_merged_data_2017, "dhs_data_2017_rds.rds")

library(haven)
write_dta(all_merged_data_2017, "dhs_data_2017.dta", version = 13)
write.csv(all_merged_data_2017, "dhs_data_2017.csv", row.names = FALSE)


#########################2022#############################################

# Create vector of months and corresponding SPEI column names for 2022
months_data_2022 <- data.frame(
  year = rep(2022, 7),
  month = 6:12,  # June to December
  spei_col = c("sp_jn2022", "sp_jl2022", "sp_g2022", 
               "sp_s2022", "sp_c2022", "sp_n2022", "sp_d2022")
)

# Create empty list to store merged datasets
merged_datasets_2022 <- list()

# Loop through each month
for(i in 1:nrow(months_data_2022)) {
  # Get current month's info
  current_year <- months_data_2022$year[i]
  current_month <- months_data_2022$month[i]
  current_spei <- months_data_2022$spei_col[i]
  
  # Get DHS data for current month
  month_dhs <- dhs_data_combined %>%
    filter(v007 == current_year, v006 == current_month) %>%
    rename(DHSCLUS = v001)
  
  # Get SPEI data from 2022 shapefile
  month_spei <- bd_gps_2022_btm %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    dplyr::select(DHSCLUS, !!current_spei)
  
  # Merge
  month_merged <- month_dhs %>%
    left_join(month_spei, by = "DHSCLUS")
  
  # Store in list
  merged_datasets_2022[[i]] <- month_merged
  
  # Print verification
  cat(sprintf("\nProcessing %d-%d:\n", current_year, current_month))
  cat(sprintf("Original observations: %d\n", nrow(month_dhs)))
  cat(sprintf("Merged observations: %d\n", nrow(month_merged)))
  cat(sprintf("Missing SPEI values: %d\n", sum(is.na(month_merged[[current_spei]]))))
}

# Combine all 2022 datasets
all_merged_data_2022 <- bind_rows(merged_datasets_2022)

# Final verification
print("\nFinal merged 2022 dataset:")
print(paste("Total observations:", nrow(all_merged_data_2022)))

saveRDS(all_merged_data_2022, "dhs_data_2022_rds.rds")

library(haven)
write_dta(all_merged_data_2022, "dhs_data_2022.dta", version = 13)
write.csv(all_merged_data_2022, "dhs_data_2022.csv", row.names = FALSE)


################Merging other geospatial covariates###############################

dhs_spei_2017 <- readRDS("dhs_data_2017_rds.rds")
dhs_spei_2022 <- readRDS("dhs_data_2022_rds.rds")

spatial_covariates_2017 <- read.csv("gc/gc2017.csv")
spatial_covariates_2022 <- read.csv("gc/gc2022.csv")

dhs_spei_2017 <- dhs_spei_2017 %>% rename(DHSCLUST=DHSCLUS)
dhs_spei_2022 <- dhs_spei_2022 %>% rename(DHSCLUST=DHSCLUS)


#Performing merge

dhs_spei_cov_2017 <- merge(dhs_spei_2017, spatial_covariates_2017, by = "DHSCLUST", all.x = TRUE)
saveRDS(dhs_spei_cov_2017, "total_2017_rds.rds")
write_dta(dhs_spei_cov_2017, "total_2017.dta", version = 13)
write.csv(dhs_spei_cov_2017, "total_2017.csv", row.names = FALSE)


dhs_spei_cov_2022 <- merge(dhs_spei_2022, spatial_covariates_2022, by = "DHSCLUST", all.x = TRUE)
saveRDS(dhs_spei_cov_2022, "total_2022_rds.rds")
write_dta(dhs_spei_cov_2022, "total_2022.dta", version = 13)
write.csv(dhs_spei_cov_2022, "total_2022.csv", row.names = FALSE)

