#' This function extracts covariables for the cropland in an AOI
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param covar_data_folder The folder where the covariable rasters are stored
#' @import terra, data.table
#' @returns Stores a csv-file with covariables for all potential sampling locations.
#' @examples
#' extract_covariables_for_aoi(working_directory = 'C:/Users/peter/Documents',
#'                             aoi_name = 'The_Netherlands',
#'                             covar_data_folder = 'C:/covar_data')


# function extract covariables from aoi
extract_covariables_for_aoi <- function(working_directory,
                                        aoi_name,
                                        covar_data_folder){

  # get coordinates of current AOI
  cropland <- rast(paste(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif', sep=''))
  xy <- crds(cropland, df=T)

  # list covariable data
  covariable_layers <- list.files(path = covar_data_folder,
                                  full.names = T,
                                  pattern = '.tif$')

  # print status
  print(paste0(length(covariable_layers), ' covariable layers found.'))

  ## loop through each covariable layer
  for(i in 1:length(covariable_layers)){

    # read covariable layer
    print(paste0('Extracting covariables from: ', covariable_layers[i]))
    covariable_layer <- covariable_layers[i]
    covariable_raster <- rast(covariable_layer)
    layer_name <- basename(covariable_layers[i])

    # extract covariable properties at all possible locations
    extracted_covariable <- extract(covariable_raster, xy, xy=T)

    if(i == 1){
      extracted_covariables <- extracted_covariable[,c(3,4,2)]
      colnames(extracted_covariables) <- c('Longitude', 'Latitude', layer_name)
    } else {
      extracted_covariables <- data.table(extracted_covariables, extracted_covariable[,2])
      colnames(extracted_covariables)[ncol(extracted_covariables)] <- layer_name
    }
  }

  # standardize coordinates
  extracted_covariables$Longitude <- xy$x
  extracted_covariables$Latitude <- xy$y

  # find locations with erroneous data (e.g., where pH = 0)
  extracted_covariables$`sg_phh2o_15-30cm_mean.tif` <- ifelse(extracted_covariables$`sg_phh2o_15-30cm_mean.tif` == 0, NA, extracted_covariables$`sg_phh2o_15-30cm_mean.tif`)

  # remove nodata add index
  extracted_covariables <- extracted_covariables[complete.cases(extracted_covariables),]
  idx <- c(1:nrow(extracted_covariables))
  extracted_covariables <- cbind(idx, extracted_covariables)

  # write to CSV
  write.csv(extracted_covariables, file=paste(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.csv', sep=''), row.names=F)

  # print status
  print(paste0('Extracted covariables are saved here: ', working_directory, '/',  aoi_name, '/aoi'))
}
