#' This creates a map of the AOI showing the distance if feature space with our calibration points
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param distance_threshold The distance at which points can be considered a cluster (0.15 = default).
#' @param n_selected_pixels The number of pixels to take in the AOI to calculate closest calibration points (default = 5000).
#' @param country_shapefile Name of the admin_1 level of the country shapefile (e.g., 'NLD_adm1.shp').
#' @param current_calibration_points The link (path + filename) to the file of our current calibration points with their corresponding covariable data.
#' @param plot_resolution Resolution for plotting of distance preview. 300 is default.
#' @param plot_width Plot width in pixels for plotting of distance preview. Set in range between 2000 - 6000.
#' @param plot_heigth Plot heigth in pixels  for plotting of distance preview. Set in range between 2000 - 6000.
#' @returns This creates a map of the AOI showing the distance in feature space with our calibration points. A raster and a preview of it will be returned. When no calibration points are present in the target country, the closest in feature space will be used.
#' @import terra, Rfast
#' @examples
#' create_threshold_raster(working_directory = 'D:/calibration/projects',
#'                         aoi_name = 'The_Netherlands',
#'                         distance_threshold = 0.15,
#'                         n_selected_pixels = 5000,
#'                         current_calibration_points = 'D:/calibration/data/calibration_points/gps_covars.csv',
#'                         country_shapefile = 'D:/calibration/data/admin_regions/NLD_adm1.shp',
#'                         plot_resolution = 300,
#'                         plot_width = 4000,
#'                         plot_heigth = 3000)


# function create raster of distance between AOI and coveriables
create_threshold_raster <- function(working_directory,
                                    aoi_name,
                                    distance_threshold,
                                    n_selected_pixels,
                                    current_calibration_points,
                                    country_shapefile,
                                    plot_resolution,
                                    plot_width,
                                    plot_heigth){


  # read the raster for the AOI
  ras <- rast(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.tif'))

  # read admin shapefile
  complete_adm <- vect(country_shapefile)
  complete_country <- terra::aggregate(complete_adm, dissolve=T)
  complete_country <- terra::vect('C:/Users/peter/OneDrive - SPRINGG/Projects/R/Argentina/data/shapefiles/ARG_adm0.shp')

  # read covariable data and rename some columns
  dat <- read.csv(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.csv'))
  xy_aoi <- dat[,c(2,3)]
  dat_aoi <- dat[,c(4:ncol(dat))]

  # read all data and fix column names
  calib <- read.csv(file=current_calibration_points) # extracted from full layers
  calib <- calib[complete.cases(calib),]
  calib_barcode_xy <- data.frame(calib[,c(1:3)])
  dat_calib <- calib[,c(4:ncol(calib))]

  # calibration data and coords
  xy_calib <- data.frame(calib[,c(1:3)])

  # get scale factors
  minmax_min <- apply(dat_calib,2,min)
  minmax_max <- apply(dat_calib,2,max)
  min_max <- data.frame(min=minmax_min, max=minmax_max)


  # check if there are any calibration points in the country
  all_calibration_points <- vect(xy_calib, geom=c('longitude', 'latitude'), crs='EPSG:4326', keepgeom=FALSE)
  country_calibration_points <- terra::intersect(all_calibration_points, complete_country)


  # scale calibration data
  nom <- sweep(dat_calib, 2, min_max$min, '-')
  dat_calib <- sweep(nom, 2, (min_max$max - min_max$min), '/')

  # scale data in aoi
  nom <- sweep(dat_aoi, 2, min_max$min, '-')
  dat_aoi <- sweep(nom, 2, (min_max$max - min_max$min), '/')


  # if there are points
  if(length(country_calibration_points) != 0){

    #print status
    print(paste0(length(country_calibration_points, ' calibration points found in target country')))

          # get barcodes
          barcode_country_calibration_points <- country_calibration_points$SampleId

          # get only calibration points of country
          idx <- calib[,1] %in% barcode_country_calibration_points

  } else {

    #print status
    print(paste0('No calibration points found in target country. Finding the closest calibration points in feature space'))

    n_total_pixels <- c(1:nrow(dat_aoi))

    if(length(n_total_pixels) < n_selected_pixels){
      n_selected_pixels <- nrow(dat_aoi)
    }

    # take random sample from AOI
    set.seed(42)
    idx_selected_pixels <- sample(n_total_pixels, n_selected_pixels, replace=FALSE)
    dat_aoi_sub <- dat_aoi[idx_selected_pixels,]

    # calculate distance matrix
    distanceMatrix <- fields::rdist(dat_aoi_sub, dat_calib)
    minDist <- Rfast::rowMins(distanceMatrix, value=T)

    # find closest calibration points to pixels in AOI
    idxs <- NULL
    for(i in 1:nrow(distanceMatrix)){

      idx <- which(distanceMatrix[i,] == minDist[i])
      idxs <- c(idxs, idx)

    }

    # find index of closest calibration points
    barcode_closest_calibration_points <- unique(xy_calib[idxs,]$SampleId)
    idx <- calib[,1] %in% barcode_closest_calibration_points

  }

  ## subset calibration points
  xy_calib <- xy_calib[idx,]
  dat_calib <- dat_calib[idx,]


  # loop through each calibration point
  n_calib <- nrow(dat_calib)
  distances <- NULL
  print(paste0('Calculating distances between calibration points and pixels in AOI for: ', aoi_name))

  for(i in 1:n_calib){

    cat('-')
    current_calib <- dat_calib[i,]

    distance <- t(fields::rdist(current_calib, dat_aoi))
    distances <- cbind(distance, distances)

    if(i %% 10 == 0){
      cat(paste0(i, ' of ', n_calib, '\n'))
      distances <- apply(distances, 1, FUN = min, na.rm = TRUE)
    }
  }

  # in case the for the last batch i %% 10 != 0
  if(!is.null(dim(distances)[2])){
    distances <- apply(distances, 1, FUN = min, na.rm = TRUE)
  }

  # print to finish
  cat(paste0(n_calib, ' of ', n_calib, '\n'))
  print('Done. Creating raster distances rasters.')

  # create raster
  df <- data.frame(xy_aoi, distances)
  aoi_dist_vect <- vect(df, geom=c("Longitude", "Latitude"), crs="epsg:4326", keepgeom=FALSE)
  aoi_dist_ras <- rasterize(aoi_dist_vect, ras, field="distances")

  # pixels outside of threshold
  aoi_dist_ras_in <- aoi_dist_ras
  aoi_dist_ras_in[aoi_dist_ras_in >= distance_threshold] <- NA

  # pixels outside of threshold
  aoi_dist_ras_out <- aoi_dist_ras
  aoi_dist_ras_out[aoi_dist_ras_out < distance_threshold] <- NA


  # plot preview
  png(file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '_distance-raster.png'),
      res=plot_resolution,
      width=plot_width,
      height=plot_heigth)

  colfunc <- colorRampPalette(c("darkgreen",  "lightgreen", 'white'))

  terra::plot(complete_country,
              main=paste0(aoi_name, 'd = ', distance_threshold))
  terra::plot(aoi_dist_ras_in,
              col=colfunc(10),
              add=T)
  terra::plot(aoi_dist_ras_out, add=T, col='red', legend=NULL)
  terra::plot(complete_adm, add=T)
  terra::plot(complete_country, add=T, lwd=2)
  terra::points(xy_calib$longitude, xy_calib$latitude,
                pch = 21, cex=0.5, col="black", bg="yellow")


  dev.off()

  # write distance raster
  writeRaster(aoi_dist_ras,
              file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '_distance-raster.tif'),
              overwrite=T)

  # print status
  cat(paste0('The distance raster (.tif) and preview (.png) are saved here: \n', working_directory, '/', aoi_name, '/aoi'))
}
