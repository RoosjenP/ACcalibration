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
#' get_cropland_for_large_country(working_directory = 'D:/calibration/data/projects',
#'                                aoi_name = 'The_Netherlands',
#'                                vito_landcover_raster = 'D:/calibration/data/rasters/data/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif',
#'                                base_raster_template = 'D:/calibration/data/rasters/base_raster_template.tif',
#'                                country_shapefile = 'D:/calibration/data/admin_regions/NLD_adm1.shp',
#'                                cropland_only = TRUE)


# function to extract cropland in AOI and resample to soilgrids resolution
get_cropland_for_large_country <- function(working_directory,
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

  for(province in provinces){

    # print progress
    print(paste0('Processing province: ', province))
    aoi <- subset(country, country$NAME_1 == province)

    # crop the landcover raster by AOI and select only cropland (class=40)
    aoi <- project(aoi, crs(landcover))
    landcover <- crop(x=landcover, y=aoi)
    landcover <- mask(landcover, aoi)

    # select only cropland according to landcover map
    if(cropland_only) {
      landcover[landcover != 40] <- 0
    }

    cropland <- landcover/landcover

    # project cropland in AOI to base_raster
    cropland <- as.points(cropland, values=TRUE, na.rm=TRUE)
    cropland <- project(cropland, crs(base_raster))
    base_raster <- crop(base_raster, country)
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


  # make SpatRasterCollection of temporary rasters
  rsrc <- sprc(files_to_remove)

  # merge raster list
  m <- mosaic(rsrc)

  # remove temp files
  for(file_to_remove in files_to_remove){
    file.remove(file_to_remove)
  }

  # make cropland area file
  target_area <- data.frame(sum(expanse(m, unit="km")))
  names(target_area) <- 'area_km2'
  write.csv(target_area, file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '_area_km2.txt'), row.names=F)

  # write AOI to file
  writeVector(country, paste0(working_directory, '/',  aoi_name, '/aoi/', aoi_name, '.kml'), overwrite=TRUE)

  # write to cropland raster file
  writeRaster(m,
              file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif'),
              datatype='INT1U',
              overwrite=T)

  # print status
  print(paste0('Cropland mask is saved here: ', working_directory, '/',  aoi_name, '/aoi'))
}
