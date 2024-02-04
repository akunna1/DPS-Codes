# For Address
# Importing Durham Open data address data

# Code 1.1 starts here
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
      data$geometry <- geometry # Adds the "geometry" field to the data
      all_data <- bind_rows(all_data, data) # Appends the fetched data to the existing data in the list
    } else {
      has_more <- FALSE # Sets the "has_more" variable to false to exit the loop since there are no more pages
    }
  }
  
  return(all_data) # Returns the accumulated data from all the fetched pages
}

# Defining the URLs for active addresses
address_url <- "https://services2.arcgis.com/G5vR3cOjh6g2Ed8E/arcgis/rest/services/Active_Addresses/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"

# Fetching all pages of data for addresses
address_df <- fetch_all_pages(address_url)

# Grouping address_df by parcel ID and counting the number of addresses in each parcel
address_count <- address_df %>%
  group_by(PARCEL_ID) %>%
  count()

# Viewing the address count dataframe
View(address_count)

# Viewing the geometry of address_df
str(address_df$geometry)
colnames(address_df$geometry)
View(address_df$geometry)

# Code 1.2 starts here
# Filtering out rows with missing coordinates
complete_rows <- complete.cases(address_df$geometry)

# Creating a data frame with the coordinates
coords_df <- address_df$geometry[complete_rows, ]

# Converting the address_df to an sf object
address_sf <- cbind(address_df[complete_rows, -which(names(address_df) == "geometry")], coords_df)
address_sf <- sf::st_as_sf(address_sf, coords = c("x", "y"), crs = 4326)

# Code 1.3 starts here
# Writing the sf object to a GeoPackage file
address_filepath <- "C:/Users/akunna1/Desktop/DPS_Files_Akunna/address.gpkg"
sf::st_write(address_sf, address_filepath, layer = "address", driver = "GPKG", append = FALSE)

# Writing the sf object to a shapefile
address_shp_path <- "C:/Users/akunna1/Desktop/DPS_Files_Akunna/address.shp"
sf::st_write(address_sf, address_shp_path, layer = "address", driver = "ESRI Shapefile", append = FALSE)
