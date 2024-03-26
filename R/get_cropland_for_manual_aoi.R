#' This function defines the to be calibrated area for a manually drawn AOI
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param vito_landcover_raster The 2019 VITO landcover raster ('D:/calibration/data/rasters/data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif')
#' @param base_raster_template Empty raster to reproject all data to ('D:/calibration/data/rasters/base_raster.tif')
#' @param custom_aoi A shapefile of a manually drawn AOI
#' @param cropland_only should all pixels in AOI be considered for calibration (set to FALSE), or only cropland pixels (set to TRUE)
#' @import terra
#' @returns The to be calibrated area in for a manually drawn AOI will be defined and reprojected to a standard raster.
#' @examples
#' get_cropland_for_manual_aoi(working_directory = 'D:/calibration/data/projects',
#'                             aoi_name = 'The_Netherlands',
#'                             vito_landcover_raster = 'D:/calibration/data/rasters/data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif',
#'                             base_raster_template = 'D:/calibration/data/rasters/base_raster.tif',
#'                             custom_aoi = 'D:/calibration/data/shapefiles/custom_shapefile.shp',
#'                             cropland_only = TRUE)


# function to extract cropland in AOI and resample to soilgrids resolution
get_cropland_for_manual_aoi <- function(working_directory,
                                        aoi_name,
                                        vito_landcover_raster,
                                        base_raster_template,
                                        custom_aoi,
                                        cropland_only){

  # print status
  print(paste0('Extracting AOI for: ', aoi_name))

  # load datasets
  aoi <- vect(custom_aoi)
  landcover <- rast(vito_landcover_raster) # read landcover raster
  base_raster <- rast(base_raster_template) # read base raster to reproject cropland to

  # crop the landcover raster by AOI and select only cropland (class=40)
  aoi <- project(aoi, crs(landcover))
  landcover <- crop(x=landcover, y=aoi)
  landcover <- mask(landcover, aoi)

  # select only cropland according to landcover map
  if(cropland_only) {
    print('Selecting only coprland area')
    landcover[landcover != 40] <- 0
  } else {
    print('Selecting all landcover types')
  }

  # make 1's and NA's
  cropland <- landcover/landcover

  # calculate total area of cropland
  cropland_area <- data.frame(sum(expanse(cropland, unit="km")))
  names(cropland_area) <- 'area_km2'

  # project cropland in AOI to base_raster
  cropland <- as.points(cropland, values=TRUE, na.rm=TRUE)
  cropland <- project(cropland, crs(base_raster))
  base_raster <- crop(base_raster, cropland)
  cropland <- rasterize(cropland, base_raster, fun=min)
  cropland <- cropland / cropland

  # write to cropland raster file
  writeRaster(cropland,
              file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif'),
              datatype='INT1U',
              overwrite=T)

  # write cropland area to file
  write.csv(cropland_area, file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '_area_km2.txt'), row.names=F)

  # write AOI to file
  writeVector(aoi, paste0(working_directory, '/',  aoi_name, '/aoi/', aoi_name, '.kml'), overwrite=TRUE)

  # print status
  cat(paste0('Cropland mask is saved here: \n', working_directory, '/',  aoi_name, '/aoi'))
}
