# Required packages
library(sf)
library(spdep)
library(ggplot2)
library(dplyr)
library(writexl)
library(RColorBrewer)

# Create a vector of variable codes and their corresponding column names
var_mapping <- c(
  "a1" = "sna_goesout_bn",
  "a5" = "sna_burns_food_bn",
  "b1" = "dmh_womans_healthcare_bn",
  "b2" = "dmh_large_hh_purchases_bn",
  "b3" = "dmh_visit_family_bn",
  "b4" = "dmc_using_contra_bn",
  "c1" = "sa_can_say_no_si_bn"
)

# Create a vector of variable descriptions for plot labels
var_descriptions <- c(
  "a1" = "Disapproving violence against women for going out without husband's consent",
  "a5" = "Disapproving violence against women by husband, if wife burns food",
  "b1" = "Participation in decisions regarding their own healthcare",
  "b2" = "Participation in decisions regarding large household purchases",
  "b3" = "Participation in decisions regarding visits to family/relatives",
  "b4" = "Participation in decisions regarding use of contraception",
  "c1" = "Ability to refuse sexual intercourse"
)

process_variable <- function(var_code, var_name, var_description) {
  # Calculate district means
  district_means <- dhs_data %>%
    group_by(dist_spatial) %>%
    summarize(
      variable_mean = weighted.mean(!!sym(var_name), w = DHSwt, na.rm = TRUE)
    )
  
  district_means <- district_means[-1, ]
  
  # Merge with spatial object
  bd_districts_temp <- bd_districts %>%
    left_join(district_means, by = "dist_spatial")
  
  # Calculate cluster means
  cluster_means <- dhs_data %>%
    group_by(wave_clust) %>%
    summarize(
      cluster_means = mean(!!sym(var_name), na.rm = TRUE)
    )
  
  # Get coordinates
  coords <- dhs_data %>%
    group_by(wave_clust) %>%
    slice(1) %>%
    dplyr::select(LONGNUM, LATNUM) %>%
    as.data.frame()
  
  coords_sf <- st_as_sf(as.data.frame(coords), coords = c("LONGNUM", "LATNUM"), crs = 32646)
  nb <- dnearneigh(coords_sf, d1=0, d2=10000)
  nb <- include.self(nb)
  weights <- nb2listw(nb, style="W")
  
  # Calculate LISA statistics
  ext_spei <- dhs_data %>%
    group_by(wave_clust) %>%
    slice(1) %>%
    dplyr::select(extreme_spei) %>%
    as.data.frame()
  
  lag_ext_spei <- lag.listw(weights, ext_spei$extreme_spei)
  
  z_autonomy <- scale(cluster_means$cluster_means)
  z_lag_ext_spei <- scale(lag_ext_spei)
  
  bivariate_moran <- z_autonomy * z_lag_ext_spei
  moran_test <- moran.test(bivariate_moran, weights)
  
  # Classify clusters
  cluster_means$z_autonomy <- z_autonomy
  cluster_means$lag_ext_spei <- lag_ext_spei
  
  cluster_means <- cluster_means %>%
    mutate(
      cluster_type = case_when(
        z_autonomy > 1 & lag_ext_spei > mean(lag_ext_spei) ~ "High Score-Extreme SPEI exposure",
        z_autonomy < -1 & lag_ext_spei > mean(lag_ext_spei) ~ "Low Score-Extreme SPEI exposure",
        z_autonomy > 1 & lag_ext_spei < mean(lag_ext_spei) ~ "High Score-Moderate to Normal SPEI exposure",
        z_autonomy < -1 & lag_ext_spei < mean(lag_ext_spei) ~ "Low Score-Moderate to Normal SPEI exposure",
        TRUE ~ "Not Significant"
      )
    )
  
  # Prepare cluster points
  data_points <- dhs_data %>% 
    group_by(wave_clust) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(wave_clust, LONGNUM, LATNUM, dist_spatial, extreme_spei)
  
  data_points <- data_points %>%
    left_join(cluster_means, by = "wave_clust")
  
  cluster_points <- data_points %>%
    group_by(wave_clust) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(wave_clust, LONGNUM, LATNUM, cluster_type, dist_spatial, cluster_means, extreme_spei) %>%
    st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = "EPSG:32646")
  
  # Save results
  write.csv(cluster_points, paste0("spatial_autocorrelation/cluster_results_", var_code, ".csv"), row.names = FALSE)
  
  cluster_summary <- cluster_points %>%
    group_by(cluster_type) %>%
    summarise(count = n()) %>%
    write.csv(paste0("spatial_autocorrelation/cluster_summary_", var_code, ".csv"), row.names = FALSE)
  
  # Calculate vulnerable districts
  vulnerable_districts <- cluster_points %>%
    group_by(dist_spatial) %>%
    summarize(
      n_low_extreme = sum(cluster_type == "Low Score-Extreme SPEI exposure"),
      total_clusters = n()
    ) %>%
    mutate(
      percent_low_extreme = (n_low_extreme / total_clusters) * 100
    ) %>%
    arrange(desc(n_low_extreme))
  
  # Calculate resilient districts
  resilient_districts <- cluster_points %>%
    group_by(dist_spatial) %>%
    summarize(
      n_high_extreme = sum(cluster_type == "High Score-Extreme SPEI exposure"),
      total_clusters = n()
    ) %>%
    mutate(
      percent_high_extreme = (n_high_extreme / total_clusters) * 100
    ) %>%
    arrange(desc(n_high_extreme))
  
  # Save additional cluster information
  write.csv(vulnerable_districts, paste0("spatial_autocorrelation/vulnerable_districts_", var_code, ".csv"), row.names = FALSE)
  write.csv(resilient_districts, paste0("spatial_autocorrelation/resilient_districts_", var_code, ".csv"), row.names = FALSE)
  
  
  # Return objects needed for plotting
  return(list(
    district_data = bd_districts_temp,
    cluster_points = cluster_points,
    moran_test = moran_test
  ))
}

# Function to create district map
create_district_map <- function(district_data, var_description) {
  breaks <- c(25, 50, 75, 100)
  colors <- c("#FF0000", "#FFA500", "#00FF00", "#006837")
  
  ggplot() +
    geom_sf(data = district_data, aes(fill = variable_mean*100)) +
    geom_sf_text(data = district_data, aes(label = dist_spatial), size = 1.5, color = "black", fontface = "bold") +
    scale_fill_gradientn(
      colors = colors,
      values = scales::rescale(breaks, to = c(0, 1)),
      limits = c(0, 100),
      name = "Percentage"
    ) +
    labs(
      title = paste0(var_description,"(in %)", "\n(by District)"),
      subtitle = "Weighted mean",
      caption = "Data Source: Pooled data from BDHS 2017-18 & BDHS 2022",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0)
    )
}

# Function to create bivariate map
create_bivariate_map <- function(cluster_points, var_description) {
  ggplot() +
    geom_sf(data = cluster_points, 
            aes(color = cluster_type), 
            size = 1) +
    scale_color_manual(values = c(
      "High Score-Extreme SPEI exposure" = "#2166AC",
      "Low Score-Extreme SPEI exposure" = "#B2182B",
      "High Score-Moderate to Normal SPEI exposure" = "#92C5DE",
      "Low Score-Moderate to Normal SPEI exposure" = "#EF8A62",
      "Not Significant" = "gray80"
    )) +
    labs(
      title = paste0(var_description, "\nand SPEI Exposure Clusters"),
      subtitle = "Bivariate LISA Clusters",
      caption = "Data Source: Pooled data from BDHS 2017-18 & BDHS 2022",
      color = "Cluster Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0)
    )
}

# Loop through variables and create plots
for (var_code in names(var_mapping)) {
  results <- process_variable(var_code, var_mapping[var_code], var_descriptions[var_code])
  
  # Create and save district map
  district_map <- create_district_map(results$district_data, var_descriptions[var_code])
  ggsave(paste0("maps/district_map_", var_code, ".png"), district_map, width = 1000 / 72, height = 512 / 72, units = "in", bg = "white")
  
  # Create and save bivariate map
  bivariate_map <- create_bivariate_map(results$cluster_points, var_descriptions[var_code])
  ggsave(paste0("maps/bivariate_map_", var_code, ".png"), bivariate_map, width = 1000 / 72, height = 512 / 72, units = "in", bg = "white")
  
  # Print Moran's I test results1
  cat("\nMoran's I test results for", var_code, ":", var_mapping[var_code], "\n")
  print(results$moran_test)
}

process_variable("a1",dhs_data$sna_goesout_bn,"Disapproving violence against women for going out without husband's consent")
