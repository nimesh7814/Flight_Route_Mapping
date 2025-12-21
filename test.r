# ============================================================================
# GLOBAL FLIGHT ROUTE VISUALIZATION WITH CURVED PATHS
# ============================================================================
# This tutorial creates beautiful maps showing how airplanes actually fly!
# We'll draw curved routes that follow Earth's surface (great circle routes)
# 
# What you'll learn:
# 1. Download and load flight data
# 2. Create curved lines that follow Earth's sphere (geodetic lines)
# 3. Count how many routes pass through each region
# 4. Make professional heatmaps and visualizations
# ============================================================================

# ----------------------------------------------------------------------------
# STEP 1: INSTALL AND LOAD PACKAGES
# ----------------------------------------------------------------------------
# Think of packages as toolboxes with specialized tools

# First time? Run these install commands (remove the # symbol):
# install.packages("readr")      # For reading CSV files
# install.packages("dplyr")      # For manipulating data
# install.packages("stringr")    # For text operations
# install.packages("sf")         # For maps and geographic data ⭐ MOST IMPORTANT!
# install.packages("ggplot2")    # For making beautiful plots
# install.packages("viridis")    # For color schemes
# install.packages("scales")     # For number formatting
# install.packages("rnaturalearth") # For world map data

# Load the toolboxes we'll use
library(readr)          # Reading files
library(dplyr)          # Data manipulation (filter, mutate, etc.)
library(stringr)        # String/text operations
library(sf)             # ⭐ THE STAR: handles geographic shapes and lines
library(ggplot2)        # Making plots
library(viridis)        # Pretty color schemes
library(scales)         # Number formatting (commas, etc.)
library(rnaturalearth)  # World map boundaries

cat("✅ All packages loaded successfully!\n\n")

# ----------------------------------------------------------------------------
# STEP 2: CREATE FOLDERS AND DOWNLOAD DATA
# ----------------------------------------------------------------------------
# We need two things:
# - airports.dat: Where airports are located (latitude/longitude)
# - routes.dat: Which airports connect to which

# Create folders to organize our work
dir.create("data", showWarnings = FALSE)    # Folder for data files
dir.create("output", showWarnings = FALSE)  # Folder for final maps

# Download airports data (only if we don't have it already)
if (!file.exists("data/airports.dat")) {
  cat("📥 Downloading airports data...\n")
  download.file(
    "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
    "data/airports.dat",
    mode = "wb"
  )
  cat("✅ Airports data downloaded!\n\n")
} else {
  cat("✅ Airports data already exists!\n\n")
}

# Download routes data (only if we don't have it already)
if (!file.exists("data/routes.dat")) {
  cat("📥 Downloading routes data...\n")
  download.file(
    "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat",
    "data/routes.dat",
    mode = "wb"
  )
  cat("✅ Routes data downloaded!\n\n")
} else {
  cat("✅ Routes data already exists!\n\n")
}

# ----------------------------------------------------------------------------
# STEP 3: LOAD ROUTES DATA
# ----------------------------------------------------------------------------
# Routes data tells us: "This airline flies from Airport A to Airport B"

# The file has no headers, so we need to name the columns ourselves
routes_column <- c(
  "Airline",                # Airline code (e.g., "AA" for American Airlines)
  "Airline_ID",             # Unique number for that airline
  "Source_airport",         # Starting airport (3-letter code like "JFK")
  "Source_airport_ID",      # Unique number for starting airport
  "Destination_airport",    # Ending airport (3-letter code like "LAX")
  "Destination_airport_ID", # Unique number for ending airport
  "Codeshare",              # Is this a shared flight?
  "Stops",                  # Number of stops along the way
  "Equipment"               # Type of airplane used
)

# Read the file
routes <- read_delim(
  "data/routes.dat",        # File location
  delim = ",",              # Comma-separated values
  col_names = routes_column,# Use our column names
  show_col_types = FALSE    # Don't show data type messages
)

cat("✅ Loaded", format(nrow(routes), big.mark = ","), "flight routes\n\n")

# ----------------------------------------------------------------------------
# STEP 4: LOAD AIRPORTS DATA
# ----------------------------------------------------------------------------
# Airports data tells us: "Airport X is located at latitude Y, longitude Z"

# Define column names for airports
airports_column <- c(
  "Airport_ID",           # Unique airport number
  "Name",                 # Airport name (e.g., "John F Kennedy Intl")
  "City",                 # City name
  "Country",              # Country name
  "IATA",                 # 3-letter code (JFK, LAX, LHR, etc.)
  "ICAO",                 # 4-letter code
  "Latitude",             # How far north/south (e.g., 40.6413)
  "Longitude",            # How far east/west (e.g., -73.7781)
  "Altitude",             # Height above sea level
  "Timezone",             # Time zone offset
  "DST",                  # Daylight saving time
  "Tz_database_timezone", # Timezone name
  "Type",                 # Airport type
  "Source"                # Where this data came from
)

# Read the file
airports <- read_delim(
  "data/airports.dat",
  delim = ",",
  col_names = airports_column,
  show_col_types = FALSE
)

cat("✅ Loaded", format(nrow(airports), big.mark = ","), "airports\n\n")

# ----------------------------------------------------------------------------
# STEP 5: CONVERT AIRPORTS TO SPATIAL FORMAT
# ----------------------------------------------------------------------------
# Right now, airports are just a table with numbers
# We need to tell R: "These numbers represent LOCATIONS on Earth"

cat("🗺️  Converting airports to spatial format...\n")

airports_sf <- airports %>%
  # Make sure coordinates are numbers (not text)
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  # Remove airports with missing coordinates
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  # ⭐ MAGIC STEP: Convert to spatial object
  # This tells R: "These lat/lon pairs are POINTS on Earth"
  st_as_sf(
    coords = c("Longitude", "Latitude"),  # Which columns have coordinates
    crs = 4326,                            # CRS 4326 = standard GPS coordinates
    remove = FALSE                         # Keep the original columns too
  )

# Transform to Robinson projection (makes world maps look nice)
# Think of this like changing from a globe to a flat map
airports_sf <- st_transform(airports_sf, crs = "+proj=robin")

cat("✅ Created", format(nrow(airports_sf), big.mark = ","), 
    "spatial airport points\n\n")

# ----------------------------------------------------------------------------
# STEP 6: CLEAN ROUTES DATA
# ----------------------------------------------------------------------------
# Some routes have bad data - remove them!

cat("🧹 Cleaning routes data...\n")

routes_clean <- routes %>%
  # Remove extra spaces from airport codes
  mutate(
    Source_airport = str_trim(Source_airport),
    Destination_airport = str_trim(Destination_airport)
  ) %>%
  # Keep only routes with valid 3-letter codes (like JFK, LAX, LHR)
  filter(
    !is.na(Source_airport), 
    !is.na(Destination_airport),
    Source_airport != "", 
    Destination_airport != "",
    str_detect(Source_airport, "^[A-Z]{3}$"),      # Exactly 3 capital letters
    str_detect(Destination_airport, "^[A-Z]{3}$")  # Exactly 3 capital letters
  )

# How many did we remove?
removed <- nrow(routes) - nrow(routes_clean)
cat("   Removed", format(removed, big.mark = ","), "invalid routes\n")
cat("✅ Kept", format(nrow(routes_clean), big.mark = ","), "valid routes\n\n")

# ----------------------------------------------------------------------------
# STEP 7: MATCH ROUTES WITH AIRPORT COORDINATES
# ----------------------------------------------------------------------------
# Problem: Routes say "JFK to LAX" but don't include coordinates
# Solution: Look up each airport code to find its lat/lon

cat("🔗 Matching routes with airport coordinates...\n")

# Create a lookup table: Airport Code → Coordinates
airports_lookup <- airports %>%
  mutate(
    IATA = str_trim(IATA),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(
    !is.na(IATA), 
    IATA != "",
    str_detect(IATA, "^[A-Z]{3}$"),
    !is.na(Latitude), 
    !is.na(Longitude)
  ) %>%
  distinct(IATA, .keep_all = TRUE) %>%  # Keep only first occurrence
  select(IATA, Latitude, Longitude)

# Join routes with coordinates for STARTING airport
routes_joined <- routes_clean %>%
  left_join(
    airports_lookup %>% rename(src_lat = Latitude, src_lon = Longitude),
    by = c("Source_airport" = "IATA")
  ) %>%
  # Join routes with coordinates for ENDING airport
  left_join(
    airports_lookup %>% rename(dst_lat = Latitude, dst_lon = Longitude),
    by = c("Destination_airport" = "IATA")
  ) %>%
  # Keep only routes where we found BOTH coordinates
  filter(
    !is.na(src_lon), !is.na(src_lat),
    !is.na(dst_lon), !is.na(dst_lat)
  )

cat("✅ Matched", format(nrow(routes_joined), big.mark = ","), 
    "routes with coordinates\n\n")

# ----------------------------------------------------------------------------
# STEP 8: CREATE CURVED LINES (GEODETIC ROUTES) ⭐⭐⭐ MOST IMPORTANT!
# ----------------------------------------------------------------------------
# This is where the magic happens!
# We create lines that CURVE to follow Earth's surface (great circle routes)
#
# WHY CURVED?
# - Earth is a SPHERE, not flat
# - The shortest path between two points on a sphere is a CURVE (great circle)
# - Example: Flying NYC to Tokyo curves over Alaska (that's actually shorter!)

cat("🌍 Creating curved geodetic routes...\n")
cat("   This may take 1-2 minutes for", format(nrow(routes_joined), big.mark = ","), 
    "routes...\n")

# ⭐⭐⭐ THE KEY FUNCTION THAT CREATES CURVED LINES ⭐⭐⭐
create_geodetic_line <- function(lon1, lat1, lon2, lat2) {
  # STEP 1: Create a simple straight line between two points
  # This line is in geographic coordinates (latitude/longitude)
  simple_line <- st_sfc(
    st_linestring(
      matrix(c(lon1, lat1, lon2, lat2), ncol = 2, byrow = TRUE)
    ),
    crs = 4326  # CRS 4326 = WGS84 = standard GPS coordinates
  )
  
  # STEP 2: ⭐ THE MAGIC ⭐ - Add points every 100km along the line
  # st_segmentize() adds intermediate points that follow Earth's curve
  # Think of it like: instead of drawing 1 straight line, we draw 50 tiny 
  # line segments that together form a smooth curve
  curved_line <- st_segmentize(simple_line, dfMaxLength = 100000)  # 100,000 meters = 100km
  
  # STEP 3: Extract just the geometry (the shape) and return it
  return(curved_line[[1]])
}

# Apply this function to ALL routes
# This creates one curved line for each route
routes_lines_sf <- routes_joined %>%
  mutate(
    # For each row, create a curved line from source to destination
    geom_list = mapply(
      create_geodetic_line,
      src_lon, src_lat,  # Starting point
      dst_lon, dst_lat,  # Ending point
      SIMPLIFY = FALSE
    )
  )

# Convert the list of geometries into a proper spatial object
routes_lines_sf <- st_sf(
  routes_lines_sf,
  geometry = st_sfc(routes_lines_sf$geom_list, crs = 4326),
  crs = 4326
) %>%
  select(-geom_list)  # Remove the temporary list column

# NOW transform to Robinson projection (for pretty world maps)
# IMPORTANT: We do this AFTER creating curves, not before!
routes_lines_sf <- st_transform(routes_lines_sf, crs = "+proj=robin")

cat("✅ Created", format(nrow(routes_lines_sf), big.mark = ","), 
    "curved geodetic route lines!\n")

# Let's check one route to see if it worked
first_route_points <- nrow(st_coordinates(routes_lines_sf$geometry[[1]]))
cat("   Example: First route has", first_route_points, 
    "points (should be >2 if curved)\n\n")

# ----------------------------------------------------------------------------
# STEP 9: CREATE HEXAGONAL GRID
# ----------------------------------------------------------------------------
# We'll divide the world into hexagons and count routes in each hexagon
# WHY HEXAGONS? They're better than squares because each one touches
# 6 neighbors equally (squares have corners that create bias)

cat("🔷 Creating hexagonal grid...\n")

# Load world boundaries
world <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_transform("+proj=robin") %>%
  st_make_valid()

# Create hexagonal grid covering the world
# cellsize = 100,000 meters = 100km (each hexagon is ~100km wide)
hexgrid <- st_make_grid(
  world,
  cellsize = 1e5,      # 100km hexagons
  what = "polygons",
  square = FALSE       # FALSE = hexagons, TRUE = squares
) %>%
  st_as_sf() %>%
  mutate(id = row_number())

# Keep only hexagons that touch land (remove ocean-only hexagons)
land_union <- st_union(world)
hexgrid_world <- hexgrid[st_intersects(hexgrid, land_union, sparse = FALSE), ]

cat("✅ Created", format(nrow(hexgrid_world), big.mark = ","), 
    "hexagons over land areas\n\n")

# ----------------------------------------------------------------------------
# STEP 10: COUNT ROUTES PER HEXAGON
# ----------------------------------------------------------------------------
# For each hexagon, count how many flight routes pass through it

cat("🔢 Counting routes per hexagon...\n")
cat("   This may take a few minutes...\n")

# Find which routes intersect which hexagons
# This is like asking: "Which routes fly over this region?"
intersections <- st_intersects(hexgrid_world, routes_lines_sf)

# Add route count to each hexagon
hexgrid_world_rt_cnt <- hexgrid_world %>%
  mutate(nm_rts = lengths(intersections))  # Count routes in each hexagon

# Show statistics
cat("\n📊 Route Density Statistics:\n")
cat("──────────────────────────────\n")
print(summary(hexgrid_world_rt_cnt$nm_rts))
cat("\n✅ Highest density:", 
    format(max(hexgrid_world_rt_cnt$nm_rts), big.mark = ","), 
    "routes in one hexagon!\n\n")

# ----------------------------------------------------------------------------
# STEP 11: CREATE VISUALIZATIONS
# ----------------------------------------------------------------------------

cat("🎨 Creating visualizations...\n\n")

# ------------------------------------
# VISUALIZATION 1: Airport Locations
# ------------------------------------
cat("   1️⃣  Plotting airport locations...\n")

map_airports <- ggplot() +
  geom_sf(data = world, fill = "gray20", color = "gray40", size = 0.1) +
  geom_sf(data = airports_sf, color = "#FFD700", size = 0.3, alpha = 0.6) +
  labs(
    title = "Global Airport Locations",
    subtitle = paste(format(nrow(airports_sf), big.mark = ","), "airports worldwide"),
    caption = "Data: OpenFlights (2014) | Projection: Robinson"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray70"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 1),
    plot.background = element_rect(fill = "gray10", color = NA),
    panel.background = element_rect(fill = "gray10", color = NA)
  ) +
  coord_sf(crs = "+proj=robin")

print(map_airports)

# ------------------------------------
# VISUALIZATION 2: Curved Routes
# ------------------------------------
cat("   2️⃣  Plotting curved geodetic routes...\n")

# Sample 5,000 routes for clearer visualization
set.seed(123)
routes_sample <- routes_lines_sf %>% 
  slice_sample(n = min(5000, nrow(routes_lines_sf)))

map_routes_curved <- ggplot() +
  geom_sf(data = world, fill = "gray20", color = "gray40", size = 0.1) +
  geom_sf(data = routes_sample, color = "#FF6B6B", size = 0.1, alpha = 0.1) +
  labs(
    title = "Global Flight Routes (Geodetic - Following Earth's Curvature)",
    subtitle = paste("Showing", format(nrow(routes_sample), big.mark = ","), 
                     "routes as great circle paths"),
    caption = "Data: OpenFlights (2014) | Projection: Robinson"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray70"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 1),
    plot.background = element_rect(fill = "gray10", color = NA),
    panel.background = element_rect(fill = "gray10", color = NA)
  ) +
  coord_sf(crs = "+proj=robin")

print(map_routes_curved)

# ------------------------------------
# VISUALIZATION 3: Hexagonal Grid
# ------------------------------------
cat("   3️⃣  Plotting hexagonal grid...\n")

map_hexgrid <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "white", size = 0.1) +
  geom_sf(data = hexgrid_world, fill = NA, color = "red", size = 0.2, alpha = 0.5) +
  labs(
    title = "Hexagonal Analysis Grid",
    subtitle = paste(format(nrow(hexgrid_world), big.mark = ","), 
                     "hexagons (~100km width each)"),
    caption = "Projection: Robinson"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 1)
  ) +
  coord_sf(crs = "+proj=robin")

print(map_hexgrid)

# ------------------------------------
# VISUALIZATION 4: Density Heatmap
# ------------------------------------
cat("   4️⃣  Creating density heatmap...\n")

map_density <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "white", size = 0.1) +
  geom_sf(data = hexgrid_world_rt_cnt, aes(fill = nm_rts), color = NA, alpha = 0.85) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Number of\nFlight Routes",
    trans = "sqrt",
    breaks = c(0, 10, 50, 100, 250, 500, 1000, 2500, 5000),
    labels = comma,
    guide = guide_colorbar(
      barwidth = 15,
      barheight = 0.5,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Global Flight Route Density",
    subtitle = "Based on curved geodetic routes passing through each hexagonal cell",
    caption = "Data: OpenFlights (2014) | Projection: Robinson"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")

print(map_density)

# ------------------------------------
# VISUALIZATION 5: Combined Map
# ------------------------------------
cat("   5️⃣  Creating combined visualization...\n")

map_combined <- ggplot() +
  geom_sf(data = world, fill = "gray20", color = "gray40", size = 0.1) +
  geom_sf(data = hexgrid_world_rt_cnt, aes(fill = nm_rts), color = NA, alpha = 0.7) +
  geom_sf(data = routes_sample, color = "white", size = 0.05, alpha = 0.02) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Route\nDensity",
    trans = "sqrt",
    breaks = c(0, 50, 250, 1000, 2500),
    labels = comma,
    guide = guide_colorbar(
      barwidth = 12,
      barheight = 0.5,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Global Aviation Network: Routes + Density",
    subtitle = "Individual curved flight paths overlaid on regional density analysis",
    caption = "Data: OpenFlights (2014) | Projection: Robinson"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray70"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold", color = "white"),
    legend.text = element_text(size = 8, color = "gray70"),
    plot.background = element_rect(fill = "gray10", color = NA),
    panel.background = element_rect(fill = "gray10", color = NA)
  ) +
  coord_sf(crs = "+proj=robin")

print(map_combined)

# ----------------------------------------------------------------------------
# STEP 12: SAVE ALL MAPS
# ----------------------------------------------------------------------------

cat("\n💾 Saving high-resolution maps...\n")

ggsave("output/01_airport_locations.png", map_airports, 
       width = 14, height = 9, dpi = 300, bg = "gray10")
cat("   ✅ Saved: 01_airport_locations.png\n")

ggsave("output/02_routes_curved_geodetic.png", map_routes_curved, 
       width = 14, height = 9, dpi = 300, bg = "gray10")
cat("   ✅ Saved: 02_routes_curved_geodetic.png\n")

ggsave("output/03_hexgrid_overlay.png", map_hexgrid, 
       width = 14, height = 9, dpi = 300, bg = "white")
cat("   ✅ Saved: 03_hexgrid_overlay.png\n")

ggsave("output/04_flight_density_heatmap.png", map_density, 
       width = 14, height = 9, dpi = 300, bg = "white")
cat("   ✅ Saved: 04_flight_density_heatmap.png\n")

ggsave("output/04_flight_density_heatmap.pdf", map_density, 
       width = 14, height = 9, device = "pdf")
cat("   ✅ Saved: 04_flight_density_heatmap.pdf (vector format)\n")

ggsave("output/05_combined_routes_density.png", map_combined, 
       width = 14, height = 9, dpi = 300, bg = "gray10")
cat("   ✅ Saved: 05_combined_routes_density.png\n")

cat("\n🎉 ALL DONE! Check the 'output' folder for your maps!\n")
cat("═══════════════════════════════════════════════════════\n")
cat("What you created:\n")
cat("  • Airport location map\n")
cat("  • Curved geodetic route visualization\n")
cat("  • Hexagonal analysis grid\n")
cat("  • Flight density heatmap\n")
cat("  • Combined routes + density map\n")
cat("═══════════════════════════════════════════════════════\n")