
# ACcalibration

<!-- badges: start -->
<!-- badges: end -->

This package contains functions for the workflow to determine the number of calibration points for new areas.

## Installation

You can install the development version of ACcalibration like so:

``` r
library(devtools)
install_github("RoosjenP/ACcalibration")
```

## Example

This example shows the workflow for calibration of new areas:

``` r
# calibration library
library(ACcalibration)

# other libraries
library(terra)
library(data.table)
library(randomForest)
library(tibble)
library(fields)
library(scales)

#### define inputs ####

# output directory and name for the 'project'
working_directory <- 'D:/calibration/projects'
aoi_name <- 'The_Netherlands_custom'

# location of current calibration point data
gps_directory <- 'D:/calibration/data/calibration_points'
gps_file <- 'gps.csv'
covar_directory <- 'D:/calibration/data/covariables/soil_grids_2.0'

# location of required data sets
vito_landcover_raster <- 'D:/calibration/data/rasters/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif'
base_raster_template <- 'D:/calibration/data/rasters/base_raster.tif'
country_shapefile <- 'D:/calibration/data/admin_regions/NLD_adm1.shp'
current_calibration_points <- 'D:/calibration/data/calibration_points/gps_covars.csv'
custom_aoi <- 'D:/calibration/data/custom_aoi/NL_AOI.shp'


# specific settings for AOI
target_regions <- c('Gelderland', 'Utrecht')
complete_country <- FALSE

# for clustering
distance_threshold <- 0.15

# to determine number of calibration points
within_threshold_distance <- 1
significant_cluster_size <- 2

# for plotting
legend_range <- 100
legend_location <- 'bottomleft'
cropland_only <- TRUE

# to determine location of calibration points (KMeans)
n_samples <- 60
iter.max <- 5000
nstart <- 100
algorithm <- 'Lloyd'



#### workflow ####
# step 0.
# First, the covariables corresponding to our current calibration points needs to be extracted. This script only needs to be ran once, and thereafter, only whenever new calibration points have been added to our database.The GPS coordinates of our calibration points should be stored in a .csv-file, with the following columns: 'SampleId',	'lat',	'lng' 
get_covariables_for_gps(gps_directory = gps_directory,
                        gps_file = gps_file,
                        covar_directory = covar_directory)

# step 1.
# Per calibration project, we create a folder with a name for the project ('aoi_name'). In this folder, three sub-folder are created ('aoi', 'clustered', 'results'). These folders will be populated in the following steps of the workflow.
create_folder_structure(working_directory = working_directory,
                        aoi_name = aoi_name)

# step 2.
# For calibration of one or more provinces in a country, use this:
get_cropland_for_admin_aoi(working_directory = working_directory,
                           aoi_name = aoi_name,
                           vito_landcover_raster = vito_landcover_raster,
                           base_raster_template = base_raster_template,
                           country_shapefile = country_shapefile,
                           target_regions = target_regions,
                           complete_country = complete_country,
                           cropland_only = cropland_only)

# For calibration of a manually drawn AOI, use this:
get_cropland_for_custom_aoi(working_directory = working_directory,
                            aoi_name = aoi_name,
                            vito_landcover_raster = vito_landcover_raster,
                            base_raster_template = base_raster_template,
                            custom_aoi = custom_aoi,
                            cropland_only = cropland_only)

# For calibration of a large country, use this:
get_cropland_for_large_country(working_directory = working_directory,
                               aoi_name = aoi_name,
                               vito_landcover_raster = vito_landcover_raster,
                               base_raster_template = base_raster_template,
                               country_shapefile = country_shapefile,
                               cropland_only = cropland_only)

# step 3.
# after we have determined the AOI in step 2, the covariables are extracted for it in step 3.
extract_covariables_for_aoi(working_directory = working_directory,
                            aoi_name = aoi_name,
                            covar_directory = covar_directory)

# step 4.
# Once the covariables have been extracted, they are clustered by a distance_threshold in feature space.
cluster_covariable_data(working_directory = working_directory,
                        aoi_name = aoi_name,
                        distance_threshold = distance_threshold,
                        current_calibration_points = current_calibration_points)

# step 5.
# after clustering, the calibrated clusters are determined, and a calibration curve is drawn.
create_calibration_curve(working_directory = working_directory,
                         aoi_name = aoi_name,
                         country_shapefile = country_shapefile,
                         distance_threshold = distance_threshold,
                         within_threshold_distance = within_threshold_distance,
                         significant_cluster_size = significant_cluster_size,
                         legend_range = legend_range,
                         legend_location = legend_location)

# step 6.
# Once client agree upon a number of calibration samples, they can be determined with the following function:
get_location_of_calibration_points_kmeans(working_directory = working_directory,
                                          aoi_name = aoi_name,
                                          n_samples = n_samples,
                                          iter.max = iter.max,
                                          nstart = nstart,
                                          algorithm = algorithm)
```

