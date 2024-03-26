#' This function creates a calibration curve based on the clustered covariable data.
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param distance_threshold The distance at which points can be considered a cluster (0.15 = default).
#' @param within_threshold_distance The distance at which points can be considered inside a cluster (set to 1 for countries with calibration points and set to 'number of covariables' for countries without calibration points)
#' @param country_shapefile Name of the admin_1 level of the country shapefile (e.g., 'NLD_adm1.shp')
#' @param significant_cluster_size Ignore clusters smaller than this number.
#' @param legend_range Range of the legend (0 to legend_range). Guesstimate this based on size of country. Fine-tune with trial and error.
#' @param legend_location Where should the legend be plotted? 'topright', 'topleft', 'bottomright', 'bottomleft'. Fine-tune with trial and error.
#' @import terra, fields, scales
#' @returns A plot with the calibration curve stored in the 'results' folder.
#' @examples
#' create_calibration_curve(working_directory = 'D:/calibration/data/projects',
#'                         aoi_name = 'The_Netherlands',
#'                         distance_threshold = 0.15,
#'                         country_shapefile = 'D:/calibration/data/admin_regions/NLD_adm1.shp',
#'                         distance_threshold = 0.15,
#'                         within_threshold_distance = 1,
#'                         significant_cluster_size = 3,
#'                         legend_range = 100,
#'                         legend_location = 'bottomleft')


# function to cluster covariable data in the aoi
create_calibration_curve <- function(working_directory,
                                     aoi_name,
                                     country_shapefile,
                                     distance_threshold,
                                     within_threshold_distance,
                                     significant_cluster_size,
                                     legend_range,
                                     legend_location){


  # read shapefiles for plotting
  complete_adm <- vect(country_shapefile)
  complete_country <- aggregate(complete_adm, dissolve=T)

  # settings
  stats_df <- NULL

  # read data
  aoi <- vect(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.kml'))
  cropland <- rast(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif'))
  cluster_df <- read.csv(paste0(working_directory, '/', aoi_name, '/clustered/', aoi_name, ' -- clusters_distThreshold=', distance_threshold, '.csv'))
  not_clustered_df <- read.csv(paste0(working_directory, '/', aoi_name, '/clustered/', aoi_name, ' -- nonClustered_distThreshold=', distance_threshold, '.csv'))
  scaled_calibration_data <- read.csv(paste0(working_directory, '/', aoi_name, '/clustered/', aoi_name, ' -- scaledCalibPoints=', distance_threshold, '.csv'))

  ## get only the soilgrids properties
  scaled_calibration_data <- scaled_calibration_data[,c(4:ncol(scaled_calibration_data))]
  distance_threshold_calc <- distance_threshold / within_threshold_distance

  ## get unique clusters and loop through them
  data_clusters <- unique(cluster_df$cluster_number)

  # print status
  print('Creating calibration curve')
  print(paste0('Checking ', length(data_clusters), ' clusters.' ))

  cluster_info_df <- NULL

  for(data_cluster in data_clusters){

    data_cluster_center <- cluster_df[which(cluster_df$cluster_number == data_cluster), c(4:ncol(cluster_df))]

    if(nrow(data_cluster_center) > 1){data_cluster_center <- data_cluster_center[1,]}

    dists <- fields::rdist(data_cluster_center, scaled_calibration_data)

    if(length(which(dists < distance_threshold_calc))){
      single_cluster_center <- not_clustered_df[which(not_clustered_df$cluster_number == data_cluster), c(4:ncol(not_clustered_df))]
      max_dist_in_cluster <- max(fields::rdist(single_cluster_center, single_cluster_center))
      dist_from_centre <- max(fields::rdist(data_cluster_center, single_cluster_center))
      calib_in_data_cluster <- which(dists < distance_threshold_calc)
      cluster_info <- data.frame(cluster_number = data_cluster,
                                 n_calib_points_in_cluster = length(calib_in_data_cluster),
                                 max_dist_in_cluster,
                                 dist_from_centre = dist_from_centre)
      cluster_info_df <- rbind(cluster_info_df, cluster_info)
    }
  }


  clusters_to_remove <- cluster_info_df$cluster_number
  remove <- !(cluster_df$cluster_number %in% clusters_to_remove)

  not_calibrated <- cluster_df[remove,]
  calibrated <- cluster_df[!remove,]

  unique_not_calibrated <- duplicated(not_calibrated$cluster_number)
  not_calibrated <- not_calibrated[!unique_not_calibrated,]

  unique_calibrated <- duplicated(calibrated$cluster_number)
  calibrated <- calibrated[!unique_calibrated,]

  not_signf_clusters <- not_calibrated$points_in_cluster > significant_cluster_size
  not_calibrated <- not_calibrated[not_signf_clusters,]
  not_calibrated <- not_calibrated[order(-not_calibrated$points_in_cluster),]
  not_calibrated$cumsum_points_in_cluster <- cumsum(not_calibrated$points_in_cluster)

  calibration_percentage <- (not_calibrated$cumsum_points_in_cluster + sum(calibrated$points_in_cluster))/
    (sum(calibrated$points_in_cluster) + sum(not_calibrated$points_in_cluster))
  already_calibrated <- sum(calibrated$points_in_cluster) / (sum(calibrated$points_in_cluster) + sum(not_calibrated$points_in_cluster))


  calib_thresh_0.8 <- min(which(calibration_percentage > 0.8))
  calib_thresh_0.9 <- min(which(calibration_percentage > 0.9))
  calib_thresh_0.95 <- min(which(calibration_percentage > 0.95))
  calib_thresh_0.99 <- min(which(calibration_percentage > 0.99))

  ## calculate some stats
  number_contributing_clusters <- length(unique(cluster_info_df$barcode))

  print('plot')

  # plot
  png(file=paste0(working_directory, '/', aoi_name, '/results/', aoi_name, ' -- Calibration curve.png'),
      res=300,
      width=3000,
      height=1500)

  layout(matrix(c(1,1,1,2,2), 1, 5, byrow = TRUE))

  # scale between 0 and 1 calibrated (remove already calibrated part from plot)
  calibration_df <- data.frame(number_of_samples = c(1:length(calibration_percentage)),
                               calibration_percentage)


  calibration_df$calibration_percentage <- scales::rescale(calibration_df$calibration_percentage)

  print('plot 1')

  # plot(c(0:length(calibration_percentage)), c(0,calibration_percentage),
  base::plot(calibration_df$number_of_samples, calibration_df$calibration_percentage,
       ylim=c(0, 1),
       type='l',
       col='red',
       lwd=3,
       las=1,
       xaxt='n',
       xlab='Number of calibration points',
       ylab='Percentage of calibrated area',
       main=paste0(aoi_name))

  print('plot 2')

  axis(side = 1, pretty(c(1:length(calibration_percentage)), n = 10))
  abline(v=calib_thresh_0.8, col='gray40', lty=2)
  text(x=calib_thresh_0.8, y=1, '80%', pos=4, col='gray40')
  text(x=calib_thresh_0.8, y=0.03, calib_thresh_0.8, pos=4, col='gray40')
  abline(v=calib_thresh_0.9, col='gray40', lty=2)
  text(x=calib_thresh_0.9, y=1, '90%', pos=4, col='gray40')
  text(x=calib_thresh_0.9, y=0.03, calib_thresh_0.9, pos=4, col='gray40')
  abline(v=calib_thresh_0.95, col='gray40', lty=2)
  text(x=calib_thresh_0.95, y=1, '95%', pos=4, col='gray40')
  text(x=calib_thresh_0.95, y=0.03, calib_thresh_0.95, pos=4, col='gray40')
  abline(v=calib_thresh_0.99, col='gray40', lty=2)
  text(x=calib_thresh_0.99, y=1, '99%', pos=4, col='gray40')
  text(x=calib_thresh_0.99, y=0.03, calib_thresh_0.99, pos=4, col='gray40')
  abline(h=0)

  print('plot 3')

  ## add plot of AOI
  plot(complete_country)
  plot(complete_adm, col='gray95', add=T, lwd=0.5)
  plot(aoi, col='Bisque', add=T, lwd=0.5)
  plot(cropland, add=T, col='red', legend=NULL)
  plot(aoi, add=T, lwd=0.5)
  plot(complete_adm, add=T, lwd=0.5)
  plot(complete_country, add=T)
  terra::sbar(d=legend_range, xy=legend_location, divs=4, type='bar', below="km")

  print('plot 4')

  dev.off()

  # print status
  print(paste0('The Calibration curve is saved here: ', working_directory, '/', aoi_name, '/results'))
}
