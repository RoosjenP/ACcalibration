
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

# other required libraries
library(terra)
library(data.table)
library(randomForest)
library(tibble)
library(fields)
library(scales)
library(Rfast)


#### workflow ####


#############
## step A. ##
#############

# inputs
gps_directory <- 'D:/calibration/data/calibration_points'           # directory where calibration points are stored
gps_file <- 'gps.csv'                                               # name of the file where GPS locations of calibration points are stored
covar_directory <- 'D:/calibration/data/covariables/soil_grids_2.0' # directory where co variables are stored

# First, the covariables corresponding to our current calibration points needs to be extracted. This script only needs to be ran once, and thereafter, only whenever new calibration points have been added to our database.The GPS coordinates of our calibration points should be stored in a .csv-file, with the following columns: 'SampleId',	'lat',	'lng'
get_covariables_for_gps(gps_directory = gps_directory,
                        gps_file = gps_file,
                        covar_directory = covar_directory)


#############
## step B. ##
#############

# inputs
working_directory <- 'D:/calibration/projects' # directory where to store the calibarion project
aoi_name <- 'Switzerland'                      # name to give to the project (same name will be used as title of plot results)

# Per calibration project, we create a folder with a name for the project ('aoi_name'). In this folder, three sub-folder are created ('aoi', 'clustered', 'results'). These folders will be populated in the following steps of the workflow.
create_folder_structure(working_directory = working_directory,
                        aoi_name = aoi_name)


# inputs
vito_landcover_raster <- 'D:/calibration/data/rasters/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif' # VITO landcover map
base_raster_template <- 'D:/calibration/data/rasters/base_raster.tif' # base raster to reproject all data to
country_shapefile <- 'D:/calibration/data/admin_regions/CHE_adm1.shp' # shapefile of country downloaded from: https://www.diva-gis.org/gdata
target_regions <- c('', '')                                           # name of the target provinces (under NAME_1 in the country shapefile)
complete_country <- FALSE                                             # process the complete country (TRUE/FALSE). This ignores target regions
cropland_only <- TRUE                                                 # process only cropland pixels: TRUE, processes all pixels: FALSE

# For calibration of one or more provinces in a country, use this:
get_cropland_for_admin_aoi(working_directory = working_directory,
                           aoi_name = aoi_name,
                           vito_landcover_raster = vito_landcover_raster,
                           base_raster_template = base_raster_template,
                           country_shapefile = country_shapefile,
                           target_regions = target_regions,
                           complete_country = complete_country,
                           cropland_only = cropland_only)


# inputs
vito_landcover_raster <- 'D:/calibration/data/rasters/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif' # VITO landcover map
base_raster_template <- 'D:/calibration/data/rasters/base_raster.tif' # base raster to reproject all data to
custom_aoi <- 'D:/calibration/data/custom_aoi/NL_AOI.shp'             # custom draw aoi (shapefile)
cropland_only <- TRUE                                                 # process only cropland pixels: TRUE, processes all pixels: FALSE

# For calibration of a manually drawn AOI, use this:
get_cropland_for_custom_aoi(working_directory = working_directory,
                            aoi_name = aoi_name,
                            vito_landcover_raster = vito_landcover_raster,
                            base_raster_template = base_raster_template,
                            custom_aoi = custom_aoi,
                            cropland_only = cropland_only)


# inputs
vito_landcover_raster <- 'D:/calibration/data/rasters/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif' # VITO landcover map
base_raster_template <- 'D:/calibration/data/rasters/base_raster.tif' # base raster to reproject all data to
country_shapefile <- 'D:/calibration/data/admin_regions/CHE_adm1.shp' # shapefile of country downloaded from: https://www.diva-gis.org/gdata
cropland_only <- TRUE                                                 # process only cropland pixels: TRUE, processes all pixels: FALSE

# For calibration of a large country, use this:
get_cropland_for_large_country(working_directory = working_directory,
                               aoi_name = aoi_name,
                               vito_landcover_raster = vito_landcover_raster,
                               base_raster_template = base_raster_template,
                               country_shapefile = country_shapefile,
                               cropland_only = cropland_only)


# inputs
covar_directory <- 'D:/calibration/data/covariables/soil_grids_2.0' # directory where co variables are stored

# step 3.
# after we have determined the AOI, the covariables are extracted for it:
extract_covariables_for_aoi(working_directory = working_directory,
                            aoi_name = aoi_name,
                            covar_directory = covar_directory)


#############
## step C. ##
#############

# inputs
distance_threshold <- 0.15                                                            # distance used for clustering. Default = 0.15
current_calibration_points <- 'D:/calibration/data/calibration_points/gps_covars.csv' # covar data at our current calibration points

# Once the covariables have been extracted, they are clustered by a distance_threshold in feature space.
cluster_covariable_data(working_directory = working_directory,
                        aoi_name = aoi_name,
                        distance_threshold = distance_threshold,
                        current_calibration_points = current_calibration_points)


# inputs
within_threshold_distance <- 1  # within which distance is a point considered calibrated? Use 1 for countries with calibration points (nearby) and number_of_covariables for countries without.
significant_cluster_size <- 2   # remove clusters with this amount of pixels
legend_range <- 100             # maximum range of legend (for plotting)
legend_location <- 'bottomleft' # where the legend will be plotted

# after clustering, the calibrated clusters are determined, and a calibration curve can be created.
create_calibration_curve(working_directory = working_directory,
                         aoi_name = aoi_name,
                         country_shapefile = country_shapefile,
                         distance_threshold = distance_threshold,
                         within_threshold_distance = within_threshold_distance,
                         significant_cluster_size = significant_cluster_size,
                         legend_range = legend_range,
                         legend_location = legend_location)


#############
## step D. ##
#############

# inputs
n_samples <- 60      # the number of samples that will be collected
iter.max <- 100      # number of iterations of kmeans
nstart <- 1          # number of starts of kmeans
algorithm <- 'Lloyd' # kmeans algorithm

# Once client agree upon a number of calibration samples, they can be determined with the following function:
get_location_of_calibration_points_kmeans(working_directory = working_directory,
                                          aoi_name = aoi_name,
                                          n_samples = n_samples,
                                          iter.max = iter.max,
                                          nstart = nstart,
                                          algorithm = algorithm)


#############
## step E. ##
#############

# inputs
n_selected_pixels <- 5000 # subset of pixels in AOI to calculate distance to
plot_resolution <- 300    # resolution of preview plot (default is 300)
plot_width <- 4000        # with of plot in pixels
plot_heigth <- 3000       # height of plot in pixels

# this function creates a map showing which areas are calibrated and which areas are not.
create_threshold_raster(working_directory = working_directory,
                        aoi_name = aoi_name,
                        distance_threshold = distance_threshold,
                        n_selected_pixels = n_selected_pixels,
                        current_calibration_points = current_calibration_points,
                        country_shapefile = country_shapefile,
                        plot_resolution = plot_resolution,
                        plot_width = plot_width,
                        plot_heigth = plot_heigth)

```

