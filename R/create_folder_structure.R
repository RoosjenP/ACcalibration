#' This function creates the folder structure for calibration requests
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @returns A folder called 'aoi_name' will be created with three folder in there: 'aoi', 'clustered', and 'results'.
#' @examples
#' create_folder_structure(working_directory = 'C:/Users/peter/Documents',
#'                         aoi_name = 'The_Netherlands')


# function to create folder structure for calibration results
create_folder_structure <- function(working_directory, aoi_name){

  # create folder structure
  dir.create(file.path(working_directory, aoi_name), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'aoi'), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'clustered'), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'results'), showWarnings = FALSE)

  # print status
  print(paste0('Folders created here: ', working_directory))
}
