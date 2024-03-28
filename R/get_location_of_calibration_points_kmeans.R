#' This function uses kmeans clustering to calculate the the location for a number of calibration points in an AOI
#'
#' @param working_directory The directory where the folder calibration output is be stored. The previous steps of the calibration workflow have to be executed before running this.
#' @param aoi_name The name of the AOI as defined in the beginning of the workflow.
#' @param n_samples The number of calibration samples you want to collect in the AOI.
#' @param iter.max setting for KMeans clustering: maximum number of iterations.
#' @param nstart setting for KMeans clustering: number of starts.
#' @param algorithm setting for KMeans clustering: KMeans algorithm: choose one('Lloyd', 'Hartigan-Wong', 'Forgy', 'MacQueen').
#' @import terra, fields
#' @returns This function returns: 1) a .kml-file with the sampling locations named 1:n_samples, and 2) a raster with alternative sampling locations where each raster cell is numbered 1:n_samples.
#' @examples
#' get_location_of_calibration_points_kmeans(working_directory = 'D:/calibration/projects',
#'                                           aoi_name = 'The_Netherlands',
#'                                           n_samples = 60,
#'                                           iter.max = 50,
#'                                           nstart = 10,
#'                                           algorithm = 'Lloyd')


get_location_of_calibration_points_kmeans <- function(working_directory,
                                                      aoi_name,
                                                      n_samples,
                                                      iter.max,
                                                      nstart,
                                                      algorithm){

  # read soil grids parameters at cropland
  dat <- read.csv(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.csv'))
  aoi_coords <- dat[,c(2:3)]
  dat <- dat[,c(4:ncol(dat))]
  colnames(dat) <- gsub('.tif', '', colnames(dat))

  # scale data
  dat <- scale(dat, center = TRUE, scale = TRUE)
  dat[is.na(dat)] <- 0 ## change na to 0

  # read aoi raster
  aoi_rast <- rast(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif'))

  # do kmeans clustering of covariables. This might take very long depending on size of data
  print('Using KMeans clustering to find calibration points for AOI. This might take very long depending on size of data')
  set.seed(1)
  myclusters <- kmeans(dat, centers=n_samples, iter.max=iter.max, nstart=nstart, algorithm=algorithm, trace=T)

  # copy covariables data and add clusters
  dat <- data.frame(dat)
  dat2 <- dat
  dat2$cluster <- myclusters$cluster

  # loop through each cluster
  print('Calculating most central pixel (in feature space) within each cluster')
  cluster_centers_aoi <- NULL
  for(cluster in sort(unique(myclusters$cluster))){

    # print progress
    print(paste0('Processing point ', cluster, ' of ', length(unique(myclusters$cluster))))

    # select:
    cluster_data <- dat[which(dat2$cluster == cluster),] # all pixels within the cluster
    cluster_center <- myclusters$centers[cluster,]       # the coordinates of the cluster center

    # calculate the distance of cluster data to cluster center
    D1 <- fields::rdist(x1 = t(cluster_center), x2 = cluster_data)
    idx <- apply(D1, MARGIN = 1, FUN = which.min)
    idx <- as.numeric(rownames(cluster_data[idx,]))

    # store the coordinates of the pixel closest to the cluster centers
    cluster_center_aoi <- data.frame(cluster = cluster,
                                     Longitude = aoi_coords[idx,][1],
                                     Latitude = aoi_coords[idx,][2])

    # store all coordinates of the pixels closest to the cluster centers
    cluster_centers_aoi <- rbind(cluster_centers_aoi, cluster_center_aoi)
  }

  # write cluster centers to vector file
  cluster_centers_aoi$cluster <- as.numeric(cluster_centers_aoi$cluster)
  clusters_vect <- vect(cluster_centers_aoi, geom=c('Longitude', 'Latitude'), crs='EPSG:4326', keepgeom=FALSE)
  writeVector(clusters_vect,
              file=paste0(working_directory, '/', aoi_name, '/calibration points/', aoi_name, '_calibration-points.kml'), # .kml or .shp
              overwrite=TRUE)

  # add clusters to coordinates of pixels in AOI
  aoi_coords$cluster <- myclusters$cluster
  aoi_coords$cluster <- as.numeric(aoi_coords$cluster)

  # make into a vector file and rasterize to create a cluster map
  clusters_vect <- vect(aoi_coords, geom=c('Longitude', 'Latitude'), crs='EPSG:4326', keepgeom=FALSE)
  cluster_rast <- rasterize(x=clusters_vect, y=aoi_rast, field='cluster')

  # write raster with clusters
  writeRaster(cluster_rast,
              file=paste0(working_directory, '/', aoi_name, '/calibration points/', aoi_name, '_calibration-raster.tif'),
              datatype=ifelse(n_samples < 255, 'INT1U', 'INT2U'),
              overwrite=T)

  # print status
  cat(paste0('The calibration points and calibration raster are stored here: \n', working_directory, '/', aoi_name, '/calibration points'))
}
