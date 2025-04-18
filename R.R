## Install and load packages
# Load Packages
#packages <- c('sf', 'terra', 'raster', 'RStoolbox', 'rasterVis',
#              'gridExtra', 'caret', 'mapview', 'plotly',
#              'RColorBrewer', 'leaflet', 'leaflet.providers')


# Load packages
#lapply(packages, library, character.only = TRUE)


##you can also choose to load individual package
# To load individual packages
library(sf)
library(terra)
library(RStoolbox)
library(raster)
library(rasterVis)
library(gridExtra)
library(caret)
library(mapview)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(leaflet.providers)
library(randomForest)

## set seed
set.seed(12345)


##To create a new folder for your direction
#"New_folder <- paste()(
#  main_path,
#  "/",name
#  )"


##set your working directory
setwd("C:/Users/reube/Desktop/EAGLE/Rpackage")


## List Sentinel-2 bands with custom file names
 raslist_18 <- paste0("image1_B", 2:8,".tif")
 raslist_24 <- paste0("image1_B", 2:8,".tif")


 #--------------------------------------- 2018 Data----------------------------------------------------#
#You need to point to the actual 2018 raster file
list.files("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2018")

b2 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2018/B2.vrt")
b3 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2018/B3.vrt")
b4 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2018/B4.vrt")
b8 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2018/B8.vrt")
#-----------------------------------------------------------------------------------------------#


#--------------------------------------- 2024 Data ----------------------------------------------------#
# You need to point to the 2024 actual raster file
list.files("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2024")

B2 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2024/B2.vrt")
B3 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2024/B3.vrt")
B4 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2024/B4.vrt")
B8 <- rast("C:/Users/reube/Desktop/EAGLE/Rpackage/ClassificationAnalysis/2024/B8.vrt")
#------------------------------------------------------------------------------------------------#


# Use Band 2 as the reference for 2018
b3 <- resample(b3, b2)
b4 <- resample(b4, b2)
b8 <- resample(b8, b2)

# Now combine and plot 2018 stacked band
sent2_2018 <- c(b2, b3, b4, b8)
plot(sent2_2018)
plotRGB(sent2_2018, r = 3, g = 2, b = 1, stretch = "lin")


# Resample other bands to match Band 2
B3 <- resample(B3, B2)
B4 <- resample(B4, B2)
B8 <- resample(B8, B2)

# Stack all aligned bands
sent2_2024 <- c(B2, B3, B4, B8)

# Now combine and plot 2024 stacked ban
plotRGB(sent2_2024, r = 3, g = 2, b = 1, stretch = "lin")


#----------------------------------------- NDVI calculate ------------------------------------#
## NDVI <- (NIR - RED) /(NIR + RED)

#calculate the NDVI for 2018

#NIV <- sent2_b8
#RED <- sent2_b4

VI <- function(sent2_2018, k, i){
  bk <- sent2_2018[[k]]
  bi <- sent2_2018[[i]]

  VI <- (bk - bi)/(bk + bi)
  return (VI)
}

ndvi_2018 <- VI(sent2_2018, 4, 3)

plot(ndvi_2018, main = "NDVI 2018")


## Calculate the NDVI for 2024

#NIV <- sent24_b8
#RED <- sent2_4b4

VI <- function(sent2_2018, k, i){
  bk <- sent2_2018[[k]]
  bi <- sent2_2018[[i]]

  VI <- (bk - bi)/(bk + bi)
  return (VI)
}

ndvi_2024 <- VI(sent2_2024, 4, 3)

plot(ndvi_2024, main = "NDVI 2024")


## To Plot histograms for both years
hist(ndvi_2018, main = "NDVI Histogram - 2018", col = "lightgreen", xlab = "NDVI", ylab = "Frequency", breaks = 50)
hist(ndvi_2024, main = "NDVI Histogram - 2024", col = "red", xlab = "NDVI", ylab = "Frequency", breaks = 50)


## To comparing both histogram
hist(ndvi_2018, main = "NDVI Histogram Comparison", col = rgb(0,0,1,0.5), xlab = "NDVI", ylab = "Frequency", breaks = 50)
hist(ndvi_2024, col = rgb(1,0,0,0.5), add = TRUE, breaks = 50)
legend("topright", legend = c("2018", "2024"), fill = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))


#-------------------------------------- Unsupervised classification ------------------------------------------------#

## Unsupervised classification using the k-means

# Convert NDVI raster to matrix
ndvi_values_2018 <- values(ndvi_2018, mat = TRUE, na.rm = TRUE)
ndvi_values_2024 <- values(ndvi_2024, mat = TRUE, na.rm = TRUE)

# Define number of clusters
k <- 5

# Apply K-means clustering for both years
kmeans_2018 <- kmeans(ndvi_values_2018, centers = k, iter.max = 100, nstart = 10)
kmeans_2024 <- kmeans(ndvi_values_2024, centers = k, iter.max = 100, nstart = 10)

# Compute mean NDVI for each cluster
cluster_means_2018 <- tapply(ndvi_values_2018, kmeans_2018$cluster, mean)
cluster_means_2024 <- tapply(ndvi_values_2024, kmeans_2024$cluster, mean)

# Print mean NDVI values to see the distribution
print(cluster_means_2018)
print(cluster_means_2024)

# Get the correct order of clusters for both years
correct_order_2018 <- order(cluster_means_2018) # Sort by increasing NDVI
correct_order_2024 <- order(cluster_means_2024)

# Create new rasters to hold the classified results
classified_2018 <- rast(ndvi_2018)
classified_2024 <- rast(ndvi_2024)

# 2. Clear values
values(classified_2018) <- NA
values(classified_2024) <- NA

# 3. Reassign clusters based on NDVI order
reordered_2018 <- match(kmeans_2018$cluster, correct_order_2018)
reordered_2024 <- match(kmeans_2024$cluster, correct_order_2024)

# 4. Put classified values into raster (only where NDVI is not NA)
values(classified_2018)[!is.na(values(ndvi_2018))] <- reordered_2018
values(classified_2024)[!is.na(values(ndvi_2024))] <- reordered_2024

# Define class labels and colors
class_labels <- c("Water", "Built-up", "Bare Soil", "Low Vegetation", "Dense Vegetation")
class_colors <- c("blue", "pink", "brown", "lightgreen", "darkgreen")

# Plot 2018
plot(classified_2018, col = class_colors, main = "K-Means Classification - 2018")
legend("topleft", legend = class_labels, fill = class_colors, title = "Land Cover")

# Plot 2024
plot(classified_2024, col = class_colors, main = "K-Means Classification - 2024")
legend("topleft", legend = class_labels, fill = class_colors, title = "Land Cover")

#----------------------------- Area Calculation ---------------------------------#
## Define pixel area (Sentinel-2 = 10m x 10m)
pixel_area_m2 <- 100
pixel_area_ha <- pixel_area_m2 / 10000
pixel_area_km2 <- pixel_area_m2 / 1e6

# Extract raster values
vals_2018 <- values(classified_2018)
vals_2024 <- values(classified_2024)

# Remove NAs
vals_2018 <- vals_2018[!is.na(vals_2018)]
vals_2024 <- vals_2024[!is.na(vals_2024)]

# Frequency tables
freq_2018 <- as.data.frame(table(vals_2018))
freq_2024 <- as.data.frame(table(vals_2024))

# Rename columns
colnames(freq_2018) <- c("class", "count")
colnames(freq_2024) <- c("class", "count")

# Convert class column to numeric
freq_2018$class <- as.numeric(as.character(freq_2018$class))
freq_2024$class <- as.numeric(as.character(freq_2024$class))

# Add area calculations
freq_2018$area_ha <- freq_2018$count * pixel_area_ha
freq_2018$area_km2 <- freq_2018$count * pixel_area_km2

freq_2024$area_ha <- freq_2024$count * pixel_area_ha
freq_2024$area_km2 <- freq_2024$count * pixel_area_km2

# Add class labels
freq_2018$class_label <- class_labels
freq_2024$class_label <- class_labels

# Print area summaries
print("Land Cover Area - 2018:")
print(freq_2018)

print("Land Cover Area - 2024:")
print(freq_2024)

######------------------------- Change Detection -------------------------------###
## Inputs: Make sure 'classified_2018' and 'classified_2024' exist
# And have values from 1 to 5 representing land cover classes

# Define class labels
class_labels <- c("Water", "Built-up", "Bare Soil", "Low Vegetation", "Dense Vegetation")

# Align rasters if needed
classified_2024 <- resample(classified_2024, classified_2018, method = "near")

# Create a change map
change_map <- classified_2024 - classified_2018

# Plot the change map
plot(change_map, main = "Change Detection (2024 - 2018)", col = terrain.colors(10))
legend("topright", legend = -4:4, title = "Change Value", bty = "n")

# Extract raster values
vals_2018 <- values(classified_2018)
vals_2024 <- values(classified_2024)

# Remove NA values
valid_idx <- !is.na(vals_2018) & !is.na(vals_2024)
vals_2018 <- vals_2018[valid_idx]
vals_2024 <- vals_2024[valid_idx]

# You can ghoose to create change dataframe and transition matrix
change_df <- data.frame(from = vals_2018, to = vals_2024)
change_matrix <- table(change_df$from, change_df$to)

# Add class labels to the matrix
rownames(change_matrix) <- class_labels
colnames(change_matrix) <- class_labels

# Print transition matrix
print("Land Cover Transition Matrix (2018 → 2024):")
print(change_matrix)

# You can choose to Visualize change matrix as heatmap
change_long <- as.data.frame(as.table(change_matrix))
colnames(change_long) <- c("From", "To", "Count")

ggplot(change_long, aes(From, To, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Land Cover Change Matrix (2018 → 2024)", fill = "Pixel Count") +
  theme_minimal()



