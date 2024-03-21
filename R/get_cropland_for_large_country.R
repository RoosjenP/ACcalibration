#' This function defines the to be calibrated area for a large country
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param vito_landcover_raster The 2019 VITO landcover raster ('D:/calibration/data/rasters/data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif')
#' @param base_raster_template Empty raster to reproject all data to ('D:/calibration/data/rasters/base_raster.tif')
#' @param country_shapefile Name of the admin_1 level of the country shapefile (e.g., 'NLD_adm1.shp')
#' @param cropland_only should all pixels in AOI be considered for calibration (set to FALSE), or only cropland pixels (set to TRUE)
#' @import terra
#' @returns The to be calibrated area in for a large country will be defined and reprojected to a standard raster.
#' @examples
#' get_cropland_for_aoi(working_directory = 'D:/calibration/data/projects',
#'                      aoi_name = 'The_Netherlands',
#'                      vito_landcover_raster = 'D:/calibration/data/rasters/data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif',
#'                      base_raster_template = 'D:/calibration/data/rasters/base_raster_template.tif',
#'                      country_shapefile = 'D:/calibration/data/admin_regions/NLD_adm1.shp',
#'                      complete_country = TRUE)


# function to extract cropland in AOI and resample to soilgrids resolution
get_cropland_for_admin_aoi <- function(working_directory,
                                       aoi_name,
                                       vito_landcover_raster,
                                       base_raster_template,
                                       country_shapefile,
                                       cropland_only){

  # print status
  print(paste0('Extracting cropland for: ', aoi_name))

  # load datasets
  country <- vect(country_shapefile) # load country shapefile
  landcover <- rast(vito_landcover_raster) # read landcover raster
  base_raster <- rast(base_raster_template) # read base raster to reproject cropland to

  # list all provinces in country shapefile
  provinces <- country$NAME_1
  files_to_remove <- NULL
  target_area <- 0

  for(province in provinces){

    # print progress
    print(paste0('Processing province: ', province))
    aoi <- subset(country, country$NAME_1 == province)

    # subset if only regions need to be calibrated
    if(complete_country){
      aoi <- country
    } else {
      aoi <- subset(country, country$NAME_1 %in% target_regions)
    }

    # crop the landcover raster by AOI and select only cropland (class=40)
    aoi <- project(aoi, crs(landcover))
    landcover <- crop(x=landcover, y=aoi)
    landcover <- mask(landcover, aoi)

    # select only cropland according to landcover map
    if(cropland_only) {
      landcover[landcover != 40] <- 0
    }

    cropland <- landcover/landcover

    # calculate total area of cropland
    target_area <- target_area + sum(expanse(cropland, unit="km"))

    # project cropland in AOI to base_raster
    cropland <- as.points(cropland, values=TRUE, na.rm=TRUE)
    cropland <- project(cropland, crs(base_raster))
    base_raster <- crop(base_raster, cropland)
    cropland <- rasterize(cropland, base_raster, fun=min)
    cropland <- cropland / cropland

    # write to cropland raster file
    writeRaster(cropland,
                file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '-', province, '.tif'),
                datatype='INT1U',
                overwrite=T)

    # make a list of the temporary rasters to be removed later
    file_to_remove <- paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '-', province, '.tif')
    files_to_remove <- c(files_to_remove, file_to_remove)
  }

  # remove temp files
  for(file_to_remove in files_to_remove){
    file.remove(file_to_remove)
  }

  # make cropland area file
  target_area <- data.frame(target_area)
  names(target_area) <- 'area_km2'
  write.csv(target_area, file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '_area_km2.txt'), row.names=F)

  # write AOI to file
  country_aoi <- aggregate(country, dissolve=TRUE)
  writeVector(country_aoi, paste0(working_directory, '/',  aoi_name, '/aoi/', aoi_name, '.kml'), overwrite=TRUE)

  # print status
  print(paste0('Cropland mask is saved here: ', working_directory, '/',  aoi_name, '/aoi'))
}
