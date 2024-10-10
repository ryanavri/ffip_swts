# 
# Spatial Analysis: Calculating Habitat and Deforested Proportions
# 
# This script performs spatial analysis to calculate the proportion of
# habitat and deforested areas within a 5 km grid over an area of interest (AOI).
# The final output includes raster layers representing these proportions.
# To conduct this analysis you neeed at least 3 shapefiles;
# 1. Area of Interest
# 2. Area considered as habitat (All forest type; Dryland, swamp and mangrove)
# 3. Area considered as deforested (Area that previously forest change and remain to non-forest for 5 years span)

# 
# 1. Setup and Libraries----
# 

# Install necessary packages if they are not already installed
required_packages <- c("sf", "terra", "dplyr", "ggplot2")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load Libraries
library(sf)        # For vector data operations
library(terra)     # For raster data operations
library(dplyr)     # For data manipulation
library(ggplot2)   # For visualization

# 
# 2. Define File Paths----
# 

# Define file paths (modify these paths as per your directory structure)
# Ensure that the "output" directory exists or create it
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Paths to input data
aoi_path <- "aoi_mid_sumatra.shp"          # Area of Interest Shapefile
habitat_path <- "habitat.shp"         # Habitat Shapefile
deforested_path <- "deforested.shp"   # Deforested Shapefile

# Paths to output data
grid_output_path <- file.path(output_dir, "grid_5km.gpkg")
habitat_raster_path <- file.path(output_dir, "habitat_proportion_5km.tif")
deforested_raster_path <- file.path(output_dir, "deforested_proportion_5km.tif")

# 
# 3. Create 5 km Grid----
# 

# Read Area of Interest (AOI) Vector Data
admin_boundaries <- st_read(aoi_path, quiet = FALSE)


# Check and Transform CRS to Projected (e.g., UTM Zone 33N)
# It's crucial to have a projected CRS (units in meters) for accurate distance calculations
if (st_is_longlat(admin_boundaries)) {
  # Replace '32633' with the appropriate EPSG code for your AOI's location
  admin_boundaries <- st_transform(admin_boundaries, crs = 32633)
}

# Define Grid Cell Size (5 km)
cell_size_5km <- 5000  # in meters

# Create 5 km Grid Covering the AOI
grid_5km <- st_make_grid(admin_boundaries,
                         cellsize = cell_size_5km,
                         square = TRUE)  # Ensures square cells

# Convert Grid to sf Object and Assign Unique Identifiers
grid_5km_sf <- st_sf(grid_5km)
grid_5km_sf <- grid_5km_sf %>%
  mutate(
    grid_id = row_number(),    # Unique identifier for each grid cell
    size = "5 km"              # Attribute indicating grid size
  )

# Save 5 km Grid as GeoPackage
st_write(grid_5km_sf, grid_output_path, delete_dsn = TRUE)

# 
# 4. Load and Prepare Spatial Data----
# 

# Read the 5 km Grid as a SpatVector for terra compatibility
grid_5km_vect <- vect(grid_output_path)

# Read Habitat and Deforested Polygons as SpatVectors
habitat_vect <- vect(habitat_path)
deforested_vect <- vect(deforested_path)

# Convert SpatVectors to sf Objects for easier manipulation
grid_5km_sf <- st_as_sf(grid_5km_vect)
habitat_sf <- st_as_sf(habitat_vect)
deforested_sf <- st_as_sf(deforested_vect)

# Ensure Consistent CRS Across All Layers
crs_grid <- st_crs(grid_5km_sf)
crs_habitat <- st_crs(habitat_sf)
crs_deforested <- st_crs(deforested_sf)

if (crs_habitat != crs_grid) {
  habitat_sf <- st_transform(habitat_sf, crs = crs_grid)
}

if (crs_deforested != crs_grid) {
  deforested_sf <- st_transform(deforested_sf, crs = crs_grid)
}

# 
# 5. Spatial Intersection and Area Calculation----
# 

# Ensure Grid has a Unique Identifier
if (!"grid_id" %in% names(grid_5km_sf)) {
  grid_5km_sf <- grid_5km_sf %>%
    mutate(grid_id = row_number())
}

# 
## 5a. Intersection with Habitat----
# 

# Perform Spatial Intersection between Grid and Habitat
habitat_intersection <- st_intersection(grid_5km_sf, habitat_sf)

# Calculate the Area of Each Habitat Intersection in Square Meters
habitat_intersection <- habitat_intersection %>%
  mutate(area_habitat = st_area(.) %>% as.numeric())

# 
## 5b. Intersection with Deforested Areas----
# 

# Perform Spatial Intersection between Grid and Deforested Areas
deforested_intersection <- st_intersection(grid_5km_sf, deforested_sf)

# Calculate the Area of Each Deforested Intersection in Square Meters
deforested_intersection <- deforested_intersection %>%
  mutate(area_deforested = st_area(.) %>% as.numeric())

# 
# 6. Summarize Overlapping Areas per Grid Cell
# 

# 
# 6a. Summarize Habitat Areas----
# 

habitat_agg <- habitat_intersection %>%
  group_by(grid_id) %>%
  summarise(total_area_habitat = sum(area_habitat, na.rm = TRUE)) %>%
  ungroup()

# 
## 6b. Summarize Deforested Areas----
# 

deforested_agg <- deforested_intersection %>%
  group_by(grid_id) %>%
  summarise(total_area_deforested = sum(area_deforested, na.rm = TRUE)) %>%
  ungroup()

# 
# 7. Merge Summarized Data with Grid----
# 

# Merge Habitat Aggregated Data with Grid
grid_5km_sf <- grid_5km_sf %>%
  st_join(habitat_agg, by = "grid_id")

# Merge Deforested Aggregated Data with Grid
grid_5km_sf <- grid_5km_sf %>%
  st_join(deforested_agg, by = "grid_id")



# 
# 8. Handle Missing Values----
# 

# Replace NA Values with 0 for Grid Cells with No Habitat or Deforested Areas
grid_5km_sf <- grid_5km_sf %>%
  mutate(
    total_area_habitat = ifelse(is.na(total_area_habitat), 0, total_area_habitat),
    total_area_deforested = ifelse(is.na(total_area_deforested), 0, total_area_deforested)
  )



# 
# 9. Calculate Proportions----
# 

# Define Grid Cell Area in Square Meters (5 km x 5 km)
grid_cell_area <- 5000 * 5000  # 25,000,000 m²

# Calculate Proportions of Habitat and Deforested Areas within Each Grid Cell
grid_5km_sf <- grid_5km_sf %>%
  mutate(
    prop_habitat = total_area_habitat / grid_cell_area,
    prop_deforested = total_area_deforested / grid_cell_area
  ) %>%
  # Ensure Proportions are Between 0 and 1
  mutate(
    prop_habitat = pmin(pmax(prop_habitat, 0), 1),
    prop_deforested = pmin(pmax(prop_deforested, 0), 1)
  )


# 
# 10. Inspect the Updated Grid----
# 

# View the First Few Rows of the Updated Grid
print(head(grid_5km_sf))

# Optional: Save the Updated Grid with Proportions
# st_write(grid_5km_sf, file.path(output_dir, "grid_5km_with_proportions.gpkg"), delete_dsn = TRUE)

# 
# 11. Convert Grid with Proportions to Raster----
# 

# Convert Updated Grid sf Object Back to SpatVector for terra
grid_5km_vect_updated <- vect(grid_5km_sf)

# Define Raster Resolution (5 km)
raster_res <- 5000  # in meters

# Create an Empty Raster Template Based on Grid Extent
raster_template <- rast(ext(grid_5km_vect_updated), resolution = raster_res, crs = crs(grid_5km_vect_updated))

# Rasterize Habitat Proportion
# Using 'first' as the function since grid cells do not overlap
raster_habitat <- rasterize(grid_5km_vect_updated,
                            raster_template,
                            field = "prop_habitat",
                            background = 0)

# Rasterize Deforested Proportion
raster_deforested <- rasterize(grid_5km_vect_updated,
                               raster_template,
                               field = "prop_deforested",
                               background = 0)

# 
# 12. Visualize the Rasters----
# 

# Define Color Palettes for Visualization
habitat_colors <- colorRampPalette(c("white", "green"))(100)
deforested_colors <- colorRampPalette(c("white", "brown"))(100)

# Plot Habitat Proportion Raster
plot(raster_habitat,
     main = "Habitat Proportion (5 km Grid)",
     col = habitat_colors)

# Plot Deforested Proportion Raster
plot(raster_deforested,
     main = "Deforested Proportion (5 km Grid)",
     col = deforested_colors)

# 
# 13. Export the Raster Layers----
# 

# Write Habitat Proportion Raster to GeoTIFF
writeRaster(raster_habitat,
            habitat_raster_path,
            overwrite = TRUE)

# Write Deforested Proportion Raster to GeoTIFF
writeRaster(raster_deforested,
            deforested_raster_path,
            overwrite = TRUE)