
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

This is a basic example which shows you how to solve a common problem:

``` r
library(ACcalibration)

#### define inputs ####

# output directory and name for the 'project'
working_directory <- 'C:/Users/peter/Documents' 
aoi_name <- 'The_Netherlands'

# location of required data sets
vito_landcover_raster <- 'D:/calibration/data/rasters/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif'
base_raster_template <- 'D:/calibration/data/rasters/base_raster.tif'
country_shapefile <- 'D:/calibration/data/admin_regions/NLD_adm1.shp'
current_calibration_points <- 'D:/calibration/data/calibration_points/gps_covars.csv'
covar_data_folder <- 'D:/calibration/data/covariables/soil_grids_2.0'

# specific settings for AOI
target_regions <- c('Gelderland', 'Utrecht', 'Groningen', 'Limburg', 'Zeeland', 'Overijssel')
complete_country <- TRUE
distance_threshold <- 0.15
significant_cluster_size <- 2
legend_range <- 100
legend_location <- 'bottomleft'


#### workflow ####

# step 1.
create_folder_structure(working_directory = working_directory,
                        aoi_name = aoi_name)

# step 2.
get_cropland_for_aoi(working_directory = working_directory,
                     aoi_name = aoi_name,
                     vito_landcover_raster = vito_landcover_raster,
                     base_raster_template = base_raster_template,
                     country_shapefile = country_shapefile,
                     target_regions = target_regions,
                     complete_country = complete_country)

# step 3.
extract_covariables_for_aoi(working_directory = working_directory,
                            aoi_name = aoi_name,
                            covar_data_folder = covar_data_folder)

# step 4.
cluster_covariable_data(working_directory = working_directory, 
                        aoi_name = aoi_name,
                        distance_threshold = distance_threshold,
                        current_calibration_points = current_calibration_points)

# step 5.
create_calibration_curve(working_directory = working_directory, 
                         aoi_name = aoi_name,
                         distance_threshold = distance_threshold,
                         significant_cluster_size = significant_cluster_size,
                         legend_range = legend_range,
                         legend_location = legend_location)
```

