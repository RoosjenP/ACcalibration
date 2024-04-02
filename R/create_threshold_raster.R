#' This creates a map of the AOI showing the distance if feature space with our calibration points
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param distance_threshold The distance at which points can be considered a cluster (0.15 = default).
#' @param country_shapefile Name of the admin_1 level of the country shapefile (e.g., 'NLD_adm1.shp').
#' @param current_calibration_points The link (path + filename) to the file of our current calibration points with their corresponding covariable data.
#' @param plot_resolution Resolution for plotting of distance preview. 300 is default.
#' @param plot_width Plot width in pixels for plotting of distance preview. Set in range between 2000 - 6000.
#' @param plot_heigth Plot heigth in pixels  for plotting of distance preview. Set in range between 2000 - 6000.
#' @returns This creates a map of the AOI showing the distance in feature space with our calibration points. A raster and a preview of it will be returned. NOTE: this map can only be generated for countries countries that already have calibration points.
#' @import terra
#' @examples
#' create_threshold_raster(working_directory = 'D:/calibration/projects',
#'                         aoi_name = 'The_Netherlands',
#'                         country_code = 'NLA',
#'                         distance_threshold = 0.15,
#'                         current_calibration_points = 'D:/calibration/data/calibration_points/gps_covars.csv',
#'                         country_shapefile = 'D:/calibration/data/admin_regions/NLD_adm1.shp',
#'                         plot_resolution = 300,
#'                         plot_width = 4000,
#'                         plot_heigth = 3000)


# function create raster of distance between AOI and coveriables
create_threshold_raster <- function(working_directory,
                                    aoi_name,
                                    distance_threshold,
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

  # read covariable data and rename some columns
  dat <- read.csv(paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.csv'))
  xy_aoi <- dat[,c(2,3)]
  dat_aoi <- dat[,c(4:ncol(dat))]
  # colnames(dat) <- gsub('_15.30cm_mean', '', colnames(dat))
  # colnames(dat) <- gsub('_0.30cm_mean', '', colnames(dat))

  # read all data and fix column names
  calib <- read.csv(file=current_calibration_points) # extracted from full layers
  calib <- calib[complete.cases(calib),]
  calib_barcode_xy <- data.frame(calib[,c(1:3)])
  dat_calib <- calib[,c(4:ncol(calib))]
  # colnames(calib) <- gsub('_15.30cm_mean', '', colnames(calib))
  # colnames(calib) <- gsub('_0.30cm_mean', '', colnames(calib))

  # calibration data and coords
  xy_calib <- data.frame(calib[,c(1:3)])

  # get scale factors
  minmax_min <- apply(dat_calib,2,min)
  minmax_max <- apply(dat_calib,2,max)
  min_max <- data.frame(min=minmax_min, max=minmax_max)


  all_calibration_points <- vect(xy_calib, geom=c('longitude', 'latitude'), crs='EPSG:4326', keepgeom=FALSE)
  country_calibration_points <- terra::intersect(all_calibration_points, complete_country)
  barcode_country_calibration_points <- country_calibration_points$SampleId


  # get only calibration points of country
  idx <- calib[,1] %in% barcode_country_calibration_points

  xy_calib <- xy_calib[idx,]
  dat_calib <- dat_calib[idx,]

  # scale calibration data
  nom <- sweep(dat_calib, 2, min_max$min, '-')
  dat_calib <- sweep(nom, 2, (min_max$max - min_max$min), '/')

  # scale data in aoi
  nom <- sweep(dat_aoi, 2, min_max$min, '-')
  dat_aoi <- sweep(nom, 2, (min_max$max - min_max$min), '/')

  # loop through each calibration point
  n_calib <- nrow(dat_calib)
  distances <- NULL
  print(paste0('Calculating distances between calibration points and pixels in AOI for: ', country_code))

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

  terra::plot(aoi_dist_ras_in,
              col=colfunc(10),
              main=paste0(aoi_name, ' - [', distance_threshold, ']'))
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
