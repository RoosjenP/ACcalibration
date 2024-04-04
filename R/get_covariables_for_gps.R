#' This takes the GPS locations of our current calibration points and extracts covariables for them
#'
#' @param gps_directory The directory where the GPS file ('gps.csv') is stored.
#' @param gps_file The name of the GPS file. The column names of the GPS file should be: 'SampleId', 'lat', 'lng'.
#' @param covar_directory The folder where the covariable rasters are stored.
#' @returns A file called 'gps_covars.csv', which contains the GPS locations for of our calibration points and their corresponding covariables.
#' @import terra, data.table
#' @examples
#' get_covariables_for_gps(gps_directory = 'D:/calibration/data/calibration_points',
#'                         gps_file = 'gps.csv',
#'                         covar_folder = 'D:/calibration/data/covariables/soil_grids_2.0')


# function to create folder structure for calibration results
get_covariables_for_gps <- function(gps_directory,
                                    gps_file,
                                    covar_directory){


  ## read GPS of our current calibration points
  gps <- read.csv(paste0(gps_directory, '/', gps_file))
  gps <- gps[complete.cases(gps),]

  # get latitude & longitude, and sample IDs
  coords <- data.frame(latitude=gps$lat,
                       longitude=gps$lng)
  xy <- vect(gps, geom=c("lng", "lat"), crs="EPSG:4326")
  SampleId <- gps$SampleId

  # list covariable data
  covariable_layers <- list.files(path = covar_directory,
                                  full.names = T,
                                  pattern = '.tif$')

  ## loop through each covariable layer
  for(i in 1:length(covariable_layers)){

    # read covariable layer
    print(paste0('Extracting covariables from: ', (covariable_layers[i])))
    covariable_layer <- covariable_layers[i]
    covariable_raster <- rast(covariable_layer)
    layer_name <- basename(covariable_layers[i])
    layer_name <- gsub('.tif', '', layer_name)

    # extract covariable properties at all possible locations
    extracted_covariable <- extract(covariable_raster, xy, xy=F, ID=F)

    if(i == 1){
      extracted_covariables <- data.frame(coords, extracted_covariable)
      colnames(extracted_covariables) <- c('latitude', 'longitude', layer_name)
    } else {
      extracted_covariables <- data.table(extracted_covariables, extracted_covariable)
      colnames(extracted_covariables)[ncol(extracted_covariables)] <- layer_name
    }
  }

  # combine covariables with SampleId
  gps_covariables <- data.table(SampleId=SampleId,
                                extracted_covariables)

  # set all extracted zero's to NA
  gps_covariables[gps_covariables == 0] <- NA
  gps_covariables <- gps_covariables[complete.cases(gps_covariables),]

  # write to file
  write.csv(gps_covariables, file=paste0(gps_directory, '/gps_covars.csv'), row.names=F)

  # print status
  cat(paste0('A table with extracted covariables (gps_covars.csv) is stored here: \n', gps_directory))
}
