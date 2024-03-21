#' This function clusters the covariable data in the aoi.
#'
#' @param working_directory The directory where the folder calibration output will be stored. This directory should exist.
#' @param aoi_name The folder where the folder calibration output will be stored.
#' @param distance_threshold The distance at which points can be considered a cluster (0.15 = default).
#' @param current_calibration_points The link (path + filename) to the file of our current calibration points with their corresponding covariable data.
#' @returns This function clusters the covariable data in the AOI based on the 'distance_threshold' and stores a table with clustered covariable data in the clustered-folder.
#' @import fields, tibble, randomForest
#' @examples
#' cluster_covariable_data(working_directory = 'D:/calibration/data/projects',
#'                         aoi_name = 'The_Netherlands',
#'                         distance_threshold = 0.15,
#'                         current_calibration_points = 'D:/calibration/data/calibration_points/gps_covars.csv')


# function to cluster covariable data in the aoi
cluster_covariable_data <- function(working_directory,
                                    aoi_name,
                                    distance_threshold,
                                    current_calibration_points){


  # read covariable data and rename some columns
  dat <- read.csv(file=paste0(working_directory, '/', aoi_name, '/aoi/', aoi_name, '.csv'))
  xy <- dat[,c(2,3)]
  dat <- dat[,c(4:ncol(dat))]
  colnames(dat) <- gsub('_15_30_mean', '', colnames(dat))
  colnames(dat) <- gsub('_0_30_mean', '', colnames(dat))
  colnames(dat) <- gsub('.tif', '', colnames(dat))
  colnames(dat) <- gsub('sg_', '', colnames(dat))

  # read all data and fix column names
  calib <- read.csv(file=current_calibration_points) # extracted from full layers
  calib <- calib[complete.cases(calib),]
  calib_barcode_xy <- data.frame(calib[,c(1:3)])
  calib <- calib[,c(4:ncol(calib))]
  colnames(calib) <- gsub('_15.30cm_mean', '', colnames(calib))
  colnames(calib) <- gsub('_0.30cm_mean', '', colnames(calib))

  # get the minimum and maximum value of the current database per covariable
  minmax_min <- apply(calib,2,min)
  minmax_max <- apply(calib,2,max)
  min_max <- data.frame(min=minmax_min, max=minmax_max)

  # scale calibration data
  nom <- sweep(calib, 2, min_max$min, '-')
  calib <- sweep(nom, 2, (min_max$max - min_max$min), '/')

  # scale data in aoi
  nom <- sweep(dat, 2, min_max$min, '-')
  dat <- sweep(nom, 2, (min_max$max - min_max$min), '/')

  # initial clustering of the data
  # calculate number of centers
  centers <- round(nrow(dat)/10000)
  if(centers == 0){centers <- 1}

  # take random subset (+- 50,000 pixels)
  set.seed(123)
  if(nrow(dat) < 50000){n_sample_points <- nrow(dat)} else (n_sample_points = 50000)
  rf_sub_data <- dat[sample(nrow(dat), n_sample_points), ]

  # cluster subset
  set.seed(123)
  kmeans2 <- kmeans(rf_sub_data, centers = centers+1, nstart = 25, algorithm="Lloyd", iter.max=1000)
  rf_sub_data <- cbind(rf_sub_data, cluster = kmeans2$cluster)
  rf_sub_data$cluster <- as.factor(rf_sub_data$cluster)

  # train RF to predict cluster
  print('Training random forest model on data subset')
  Sys.time()
  rf <- randomForest(cluster ~ .,
                     data=rf_sub_data)

  # predict clusters of all pixels
  print('Applying random forest model to all data')
  Sys.time()

  ## split all data in batches
  batches <- ceiling(nrow(dat) / 1000000)
  predicted_classes <- NULL

  for(batch in 1:batches){

    if(batch == 1){

      start_row <- 0
      end_row <- nrow(dat) - ((batches-1) * 1000000)
      data_batch <- dat[c(start_row:end_row),]
      predicted_classes_batch <- predict(rf, newdata=data_batch)
      predicted_classes_batch <- data.frame(predicted_classes_batch)
      predicted_classes <- rbind(predicted_classes, predicted_classes_batch)

    } else {

      start_row <- end_row + 1
      end_row <- (start_row-1) + 1000000
      data_batch <- dat[c(start_row:end_row),]
      predicted_classes_batch <- predict(rf, newdata=data_batch)
      predicted_classes_batch <- data.frame(predicted_classes_batch)
      predicted_classes <- rbind(predicted_classes, predicted_classes_batch)

    }
  }

  # add index
  idx <- data.frame(c(1:nrow(dat)))
  names(idx) <- 'index'
  dat <- data.frame(index=idx, dat)

  total_number_points <- nrow(dat)
  cluster_counter <- 0
  cluster_df <- NULL
  not_clustered_df <- NULL
  Sys.time()


  for(j in 1:centers){

    print(sprintf("Clustering cluster %s of %s", j, centers))
    sub_data <- dat[which(predicted_classes == j),]
    xySub <- xy[which(predicted_classes == j),]
    idx <- data.frame(sub_data[,1])
    names(idx) <- 'index'
    sub_data <- sub_data[,c(2:ncol(sub_data))]

    ## calculate distance between points)
    dist <- dist(sub_data, method = "euclidean")

    ## calculate clusters based on distance
    hc1 <- hclust(as.dist(dist), method="complete")
    d <- distance_threshold
    cluster <- cutree(hc1, h=d)
    idx$cluster <- cluster

    ## sort; start processing the cluster with most points
    cluster_sorted_by_size <- data.frame(abs(sort(-table(cluster))))
    total_number_points <- nrow(sub_data)
    points_covered <- 0

    for(i in 1:nrow(cluster_sorted_by_size)){

      if(nrow(cluster_sorted_by_size) == 1){

        ## get cluster
        cluster_number <- 1
        points_in_cluster <- as.numeric(cluster_sorted_by_size)
        cluster_counter <- cluster_counter + 1

        ## get data in cluster
        data_index <- which(idx$cluster == cluster_number)
        cluster_data <- sub_data[data_index,]
        xy_cluster <- xy_sub[data_index,]


      } else {

        ## get cluster
        cluster_number <- cluster_sorted_by_size[i,]$cluster
        points_in_cluster <- cluster_sorted_by_size[i,]$Freq
        cluster_counter <- cluster_counter + 1

        ## get data in cluster
        data_index <- which(idx$cluster == cluster_number)
        cluster_data <- sub_data[data_index,]
        xy_cluster <- xySub[data_index,]
      }

      if(points_in_cluster > 1){

        ## find the point which is the closest to the cluster center
        centroid <- colMeans(cluster_data) # center of cluster

        ## find data closest to cluster centre
        distance_matrix <- fields::rdist(t(centroid), cluster_data) # calculate distance between all points in cluster and centroid
        minimum_distance_id <- which(distance_matrix == min(distance_matrix))
        data_index_cluster_centre <- as.numeric(rownames(cluster_data[minimum_distance_id,]))

      } else {

        ## if there is only one observation, the data itself is the centre of cluster
        data_index_cluster_centre <- rownames(cluster_data)
        centroid <- colMeans(cluster_data)

      }

      ## combine all clustered data
      df <- data.frame(cluster_number = cluster_counter,
                       points_in_cluster = points_in_cluster,
                       data_index_cluster_centre = data_index_cluster_centre,
                       t(centroid))

      ## assign cluster_number to original data
      df2 <- data.frame(cluster_number = cluster_counter,
                        xy_cluster,
                        cluster_data)

      if(i == 1){
        points_covered_df <- df
        dff2 <- df2
      } else{
        points_covered_df <- rbind(points_covered_df, df)
        dff2 <- rbind(dff2, df2)
      }
    }

    cluster_df <- rbind(cluster_df, points_covered_df)
    not_clustered_df <- rbind(not_clustered_df, dff2)
  }

  # add pixel index to not clustered data
  not_clustered_df <- add_column(not_clustered_df, index=rownames(not_clustered_df), .after = 1)

  ## write clustered dataframes
  write.csv(cluster_df, file=paste0(working_directory, '/', aoi_name, '/clustered/', aoi_name, ' -- clusters_distThreshold=', distance_threshold, '.csv'), row.names=F)
  write.csv(not_clustered_df, file=paste0(working_directory, '/', aoi_name, '/clustered/', aoi_name, ' -- nonClustered_distThreshold=', distance_threshold, '.csv'), row.names=F)
  write.csv(data.frame(calib_barcode_xy, calib), file=paste0(working_directory, '/', aoi_name, '/clustered/', aoi_name, ' -- scaledCalibPoints=', distance_threshold, '.csv'), row.names=F)

  # print status
  print(paste0('Clustered covariable data is saved here: ', working_directory, '/', aoi_name, '/clustered'))
}
