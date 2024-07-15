# Set the working directory and verify it
setwd('C:/Users/Aakash/Desktop/SCMA')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "sf")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for MEG
df <- data %>%
  filter(state_1 == "MEG")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
megnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
megnew$Meals_At_Home <- impute_with_mean(megnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  megnew <- remove_outliers(megnew, col)
}

# Summarize consumption
megnew$total_consumption <- rowSums(megnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- megnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "North West","2" = "North","3" = "North East","4" = "East","5" = "New Delhi","6" = "Central Delhi","7" = "West","8" = "South West","9" = "South")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

megnew$District <- as.character(megnew$District)
megnew$Sector <- as.character(megnew$Sector)
megnew$District <- ifelse(megnew$District %in% names(district_mapping), district_mapping[megnew$District], megnew$District)
megnew$Sector <- ifelse(megnew$Sector %in% names(sector_mapping), sector_mapping[megnew$Sector], megnew$Sector)

View(megnew)

hist(megnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Meghalaya State")

MEG_consumption <- aggregate(total_consumption ~ District, data = megnew, sum) 
View(MEG_consumption)

# Plot total consumption per district using barplot
barplot(MEG_consumption$total_consumption, 
        names.arg = MEG_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

# b) Plot {'any variable of your choice'} on the Meghalaya state map using NSSO68.csv data


# Plot total consumption on the Meghalaya state map

# Set environment variable for shapefiles
Sys.setenv("SHAPE_RESTORE_SHX" = "YES")

# Load the GeoJSON file for Meghalaya districts
data_map <- st_read("C:\\Users\\Aakash\\Downloads\\MEGHALAYA_DISTRICTS.geojson")

# Rename columns to match for merging
data_map <- data_map %>% rename(District = dtname)

# Convert district names to lowercase for consistency
MEG_consumption$District <- tolower(MEG_consumption$District)
data_map$District <- tolower(data_map$District)

# Create a mapping of incorrect district names to correct ones
district_mapping <- c(
  "central delhi" = "central delhi",
  "east" = "east",
  "new delhi" = "new delhi",
  "north" = "north",
  "north east" = "north east",
  "north west" = "north west",
  "west" = "west"
  # Add any other necessary mappings here
)

# Apply district mapping to correct names
MEG_consumption$District <- ifelse(MEG_consumption$District %in% names(district_mapping),
                                   district_mapping[MEG_consumption$District],
                                   MEG_consumption$District)

# Verify the corrected district names
cat("Corrected District names in MEG_consumption:\n")
print(unique(MEG_consumption$District))
cat("District names in data_map:\n")
print(unique(data_map$District))

# Merge the consumption data with the GeoJSON data
data_map_data <- merge(data_map, MEG_consumption, by = "District", all.x = TRUE)

# Verify if the merging was successful
cat("Data after merging:\n")
print(head(data_map_data))

# Check if data_map_data is not empty
if (nrow(data_map_data) == 0) {
  stop("Merged data frame is empty. Please check the merging process.")
}

# Plot the total consumption by district on the map
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
