#' This function creates the folder structure for calibration requests
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param vito_landcover_raster The 2019 VITO landcover raster ('C:/data/vito_landcover_raster.tif')
#' @param base_raster_template Empty raster to reproject all data to ('C:/data/base_raster_template.tif')
#' @param country_shapefile Name of the admin_1 level of the country shapefile (e.g., 'NLD_adm1.shp')
#' @param target_regions A vector with names of target admin regions (e.g., c('Gelderland', 'Utrecht'))
#' @param complete_country Process the complete county (T/F). If T: target_regions will be ignored, if F, target_regions will be processed only.
#' @import terra
#' @returns A folder called 'aoi_name' will be created with three folder in there: 'aoi', 'clustered', and 'results'.
#' @examples
#' get_cropland_for_aoi(working_directory = 'C:/Users/peter/Documents',
#'                      aoi_name = 'The_Netherlands',
#'                      vito_landcover_raster = 'C:/data/vito_landcover_raster.tif',
#'                      base_raster_template =' C:/data/base_raster_template.tif',
#'                      country_shapefile = 'NLD_adm1.shp',
#'                      target_regions = c('Gelderland', 'Utrecht'),
#'                      complete_country = TRUE)


# function to extract cropland in AOI and resample to soilgrids resolution
get_cropland_for_aoi <- function(working_directory,
                                 aoi_name,
                                 vito_landcover_raster,
                                 base_raster_template,
                                 country_shapefile,
                                 target_regions,
                                 complete_country){

  # print status
  print(paste0('Extracting cropland for: ', aoi_name))

  # load datasets
  country <- vect(country_shapefile) # load country shapefile
  landcover <- rast(vito_landcover_raster) # read landcover raster
  base_raster <- rast(base_raster_template) # read base raster to reproject cropland to


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
  landcover[landcover != 40] <- 0
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
  print(paste0('Cropland mask is saved here: ', working_directory, '/',  aoi_name, '/aoi'))
}
