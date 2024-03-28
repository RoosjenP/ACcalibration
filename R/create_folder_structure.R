#' This function creates the folder structure to store all the results and intermediate data for a calibration project.
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration outputs will be stored.
#' @returns A folder called 'aoi_name' will be created with three folder in there: 'aoi', 'clustered', 'results', 'calibration points'.
#' @examples
#' create_folder_structure(working_directory = 'D:/calibration/projects',
#'                         aoi_name = 'The_Netherlands')


# function to create folder structure for calibration results
create_folder_structure <- function(working_directory, aoi_name){

  # create folder structure
  dir.create(file.path(working_directory, aoi_name), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'aoi'), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'clustered'), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'results'), showWarnings = FALSE)
  dir.create(file.path(working_directory, aoi_name, 'calibration points'), showWarnings = FALSE)

  # print status
  cat(paste0('Folders created here: \n', working_directory))
}
