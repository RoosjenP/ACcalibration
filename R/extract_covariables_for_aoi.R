#' This function extracts covariables for the cropland in an AOI
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param covar_directory The folder where the covariable rasters are stored.
#' @import terra, data.table
#' @returns Stores a csv-file with covariables for all potential sampling locations.
#' @examples
#' extract_covariables_for_aoi(working_directory = 'D:/calibration/projects',
#'                             aoi_name = 'The_Netherlands',
#'                             covar_directory = 'D:/calibration/data/covariables/soil_grids_2.0')


# function extract covariables from aoi
extract_covariables_for_aoi <- function(working_directory,
                                        aoi_name,
                                        covar_directory){


  # get coordinates of current AOI
  cropland <- rast(paste(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif', sep=''))
  xy <- crds(cropland, df=T)

  # list covariable data
  covariable_layers <- list.files(path = covar_directory,
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
    layer_name <- gsub('.tif', '', layer_name)

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

  n_covars <- c(3:ncol(extracted_covariables))
  extracted_covariables_data <- extracted_covariables[, ..n_covars]

  # remove pixels where only zeros are extracted
  print('removing pixel where only zeros are extracted..')
  row_sums <- rowSums(extracted_covariables_data, na.rm=T)
  i <- 0
  for(row_sum in row_sums){
    i <- i + 1
    if(row_sum == 0){
      extracted_covariables_data[i, ][extracted_covariables_data[i, ] == 0] <- NA
    }

  }

  # combine again
  extracted_covariables_xy <- extracted_covariables[, c(1,2)]
  extracted_covariables <- data.table(extracted_covariables_xy, extracted_covariables_data)

  # remove nodata add index
  extracted_covariables <- extracted_covariables[complete.cases(extracted_covariables),]
  idx <- c(1:nrow(extracted_covariables))
  extracted_covariables <- cbind(idx, extracted_covariables)

  # write to CSV
  write.csv(extracted_covariables, file=paste(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.csv', sep=''), row.names=F)

  # print status
  cat(paste0('Extracted covariables are saved here: \n', working_directory, '/',  aoi_name, '/aoi'))
}
