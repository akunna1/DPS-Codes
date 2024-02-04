# For Address and Parcel
# Importing Durham Open data address and parcel data

# Code 3.1 starts here
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

# Defining the URLs for active addresses and parcels
address_url <- "https://services2.arcgis.com/G5vR3cOjh6g2Ed8E/arcgis/rest/services/Active_Addresses/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
parcel_url <- "https://services2.arcgis.com/G5vR3cOjh6g2Ed8E/arcgis/rest/services/Parcels/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"

# Fetching all pages of data for addresses and parcels
address_df <- fetch_all_pages(address_url)
parcel_df <- fetch_all_pages(parcel_url)

# Code 3.2 starts here
# Grouping address_df by parcel ID and counting the number of addresses in each parcel
address_count <- address_df %>%
  group_by(PARCEL_ID) %>%
  count() %>%
  rename(addresses_in_parcel = n) # Renaming the count column to 'addresses_in_parcel'

# Viewing the address count dataframe
#View(address_count)

# Code 3.2 starts here
# Creating a new df which consists of parcel_df and address_count
new_parcel_df <- parcel_df %>%
  left_join(address_count, by = "PARCEL_ID")

# Viewing new_parcel_df
#View(new_parcel_df)

# Code 3.3 starts here
du_file <- "DUs_2022.csv"  # File name of the CSV file

# Checking if the file exists in the working directory
if (file.exists(du_file)) {
  # Reading the CSV file into a dataframe called dwelling_units
  dwelling_units <- read.csv(du_file)
  
  # Viewing the dwelling_units dataframe
  #View(dwelling_units)
} else {
  # Displaying an error message if the file does not exist
  message("The file '", du_file, "' does not exist in the working directory.")
}

# Code 3.4 starts here
# Calculating the numbers of dwelling units per parcel
# Joining dwelling_units and new_parcel_df by Parcel_Number and PARCEL_ID
code_3_df <- merge(dwelling_units, new_parcel_df, by.x = "Parcel_Number", by.y = "PARCEL_ID")
#View (code_3_df)
#colnames(code_3_df)

# Selecting the desired columns in code_3_df
code_3_df_selection  <- code_3_df[, c("Parcel_Number", "addresses_in_parcel", "DU_Est", "LAND_USE", "LANDUSE_DESC")]

# Renaming the column "addresses_in_parcel" to "du_est_addr" in code_3_df_selection
code_3_df_selection <- code_3_df_selection %>%
  rename(du_est_addr = addresses_in_parcel)

# Renaming the column "DU_Est" to "du_est_city" in code_3_df_selection
code_3_df_selection <- code_3_df_selection %>%
  rename(du_est_city = DU_Est)

View(code_3_df_selection)

# Code 3.5 starts here
# Setting the file path of the CSV file
csv_file <- "C:\\Users\\akunna1\\Desktop\\DPS_Files_Akunna\\parcels_datadictionary.csv"

# Reading the CSV file into a dataframe called parcels_data_dict
parcels_data_dict <- read.csv(csv_file)

# Viewing the parcels_datadictionary dataframe
View(parcels_data_dict)

# Code 3.6 starts here
# Joining the land_use column from parcels_data_dict with the LAND_USE column of code_3_df_selection
result_df <- left_join(code_3_df_selection, parcels_data_dict, by = c("LAND_USE" = "land_use"))

# Trimming all entries in all the columns of result_df
result_df <- as.data.frame(lapply(result_df, trimws, "both"))

# Removing duplicates from result_df
result_df <- distinct(result_df)

# Viewing the code_3_df_selection dataframe without duplicates
View(result_df)

# Saving the new dataframe as a CSV file
csv_file_path <- "dc_parcels_dus_20230724.csv"
tryCatch(
  {
    write.csv(result_df, file = csv_file_path, row.names = FALSE)
    cat("The dataframe has been saved as", csv_file_path, "in your working directory.\n")
  },
  error = function(e) {
    cat("An error occurred while saving the dataframe as a CSV file.\n")
  }
)