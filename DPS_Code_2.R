# For Parcel
# Start everything in python then move to R (Doing a transition)
# Importing Durham Open data parcel data

# Code 2.1 starts here
setwd("C:/Users/akunna1/Desktop/DPS_Files_Akunna") #can modify this to any directory

# Importing the relevant libraries
library(httr) # Gets information in web format and convert it into json string
library(jsonlite) # Converts json string to a table
library(sf) # For spatial data operations
library(data.table) # Used for fast aggregation of large datasets, low latency add/update/remove of columns, quicker ordered joins, and a fast file reader
library(sfheaders) # For manipulating and working with spatial data headers
library(dplyr) # For data manipulation

# Function for fetching all pages of data
fetch_all_pages <- function(url) {
  all_data <- list() # Creates an empty list to store all the fetched data from pages
  page <- 0 # Initializes the page number to 0
  has_more <- TRUE # Initializes the "has_more" variable to indicate if there are more pages to fetch
  
  while (has_more) { # Enters a loop that continues as long as there are more pages to fetch
    page <- page + 1 # Increments the page number by 1
    query_url <- modify_url(url, query = list(where = "1=1", outFields = "*", outSR = 4326, f = "json", resultOffset = (page - 1) * 2000)) # Constructs the query URL by modifying the given URL with additional parameters for pagination and data format
    response <- httr::GET(query_url) # Sends an HTTP GET request to the query URL
    content <- httr::content(response, as = "text") # Extracts the content of the response as text
    data <- jsonlite::fromJSON(content)$features$attributes # Parse the JSON content and extract the "attributes" field
    geometry <- jsonlite::fromJSON(content)$features$geometry # Extracts the "geometry" field
    
    if (length(data) > 0) { # Checks if the fetched data is not empty
      data$geometry <- geometry  # Adds the "geometry" field to the data
      all_data <- bind_rows(all_data, data) # Appends the fetched data to the existing data in the list
    } else {
      has_more <- FALSE # Sets the "has_more" variable to false to exit the loop since there are no more pages
    }
  }
  
  return(all_data) # Returns the accumulated data from all the fetched pages
}

# Defining the URL for parcels
parcel_url <- "https://services2.arcgis.com/G5vR3cOjh6g2Ed8E/arcgis/rest/services/Parcels/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"

# Fetching all pages of data for parcels
parcel_df <- fetch_all_pages(parcel_url)

# Viewing the parcel_df
View(parcel_df)
colnames(parcel_df)

#View(parcel_df$geometry)
#str(parcel_df$geometry)
dim(parcel_df$geometry)


# Code 2.2 starts here
# Converting the rings to polygons
parcel_polygons <- lapply(parcel_df$geometry$rings, function(ring) {
  # Extracting coordinates and convert to numeric
  coordinates <- as.numeric(gsub("c", "", unlist(ring)))
  # Reshaping the coordinates into a matrix with two columns (x and y)
  coordinates <- matrix(coordinates, ncol = 2, byrow = TRUE)
  # Closing the polygon by adding the first point at the end
  closed_coordinates <- rbind(coordinates, coordinates[1, ])
  # Creating the polygon
  sf::st_polygon(list(closed_coordinates))
})

# Converting the list of polygons into an sf geometry object
parcel_geometry <- sf::st_sfc(parcel_polygons)

# Creating a new data frame with the geometry column
parcel_sf <- sf::st_sf(parcel_df, geometry = parcel_geometry)
View(parcel_sf)

# Code 2.3 starts here
# Defining the CRS for the sf object
st_crs(parcel_sf) <- st_crs("EPSG:4326")

# Writing the sf object to a GeoPackage file
parcel_filepath <- "C:/Users/akunna1/Desktop/DPS_Files_Akunna/parcel.gpkg"
sf::st_write(parcel_sf, parcel_filepath, layer = "parcel", driver = "GPKG", append = FALSE)

# Writing the sf object to a shapefile
parcel_shp_path <- "C:/Users/akunna1/Desktop/DPS_Files_Akunna/parcel.shp"
sf::st_write(parcel_sf, parcel_shp_path, layer = "parcel", driver = "ESRI Shapefile", append = FALSE)


