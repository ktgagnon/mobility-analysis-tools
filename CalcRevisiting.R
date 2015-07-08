CalcRevisiting <- function(x, y, radius, start.index=1, end.index=1) {
  # Quantifies the amount of revisiting and loitering behavior in the x-y coordinates. 
  # A data point is considered to reflect loitering OR revisiting behavior when it's distance to a centroid is less than the value of radius.
  # Of the data points being considered as loitering OR revisiting, those that are consecutive in time to the centroid point are loitering and all others are revisiting.
  # In other words, if the person left an "area" (area defined as region around centroid with radius equal to radius input arg.), and then re-entered this area, it is revisiting.
  # If the person stayed within the "area" and never left then these data points are considered loitering.
  # Args: 
  #     x and y: vectors of equal length.
  #     radius: distance a data point in x-y must be within in order to be considered as a revisitng or loitering point.
  #     start.index: an integer between 1 and the length of x-y vectors indicating where to begin quantifying revisiting and loitering.
  #     end.index: an integer between 1 and the length of x-y vectors indicating where to stop quantifying revisiting and loitering.
  # Returns: 'revisit.stats' is an N x 12 matrix. The columns correspond to the following values or calculations:
  #           1: radius argument used. 
  #           2: current data point in x-y vector revisiting and loitering is calculated around.
  #           3 and 4: x and y value of current data point.
  #           5: The total number of data points that are considered 'revisiting' data points.

  # Check that start.index is in bounds.
  if (start.index > length(x)){
    stop("start.index argument exceeds length of x and y vectors. start.index must be a value between 1 and the length of the x vector.")
  }

  # Initialize data matrix and var end.location.
  revisit.stats <- matrix(0,length(x),12)
  end.location <- end.index
  
  for (start.location in start.index:length(x)) {

    # Finds euclidean distance from start.location to each element up to end.location.
    # Returns list with $distances and $pos.vec, indicating euclidean distances and position in x-y coordinates, respectively.
    distance.data <- DistancesFrom(x, y, start.location, end.location)

    # Identifies distances in distance.data that are less than specified radius.
    # Returns list with $distances and $pos.vec, indicating euclidean distances less than radius and the position in the distances vector, respectively.
    distances.in.radius <- InRadius(distance.data$distances, radius)

    # Finds the beginning and end of a sequence within a vector.
    # The use here is finding sequences in positions that landed within the radius.
    seq.indices <- FindSequences(distances.in.radius$vec.pos)

    # Revisiting and Loitering Summary Data For Each Data Point
    # First sequence will be considered instances of "loitering" within the radius.
    loitering.ind <- seq.indices[1, ]
    loit.dist <- distances.in.radius$distances[loitering.ind]
    num.loit.pts <- loitering.ind[length(loitering.ind)]
    density.loit.pts <- num.loit.pts / radius
    avg.loit.dist <- mean(radius - loit.dist)
    sd.loit.dist <- sd(radius - loit.dist)

    if (nrow(seq.indices) > 1){
      # Analyze revisiting
      revisiting.ind <- seq.indices[2, 1] : seq.indices[nrow(seq.indices), 2]
      revisit.dist <- distances.in.radius$distances[revisiting.ind]
      num.revisit.pts <- sum(diff(seq.indices[,2]))
      density.revisit.pts <- num.revisit.pts / radius
      avg.revisit.dist <- mean(radius - revisit.dist)
      sd.revisit.dist <- sd(radius - revisit.dist)
    } else {
      # Fill in missing data for revisiting
      revisiting.ind <- 0
      num.revisit.pts <- 0
      density.revisit.pts <- 0
      revisit.dist <- 0
      avg.revisit.dist <- 0
      sd.revisit.dist <- 0
    } 
    
    revisit.stats[start.location,1] <- radius
    revisit.stats[start.location,2] <- start.location
    revisit.stats[start.location,3] <- x[start.location]
    revisit.stats[start.location,4] <- y[start.location]
    revisit.stats[start.location,5] <- num.revisit.pts
    revisit.stats[start.location,6] <-  density.revisit.pts
    revisit.stats[start.location,7] <- avg.revisit.dist
    revisit.stats[start.location,8] <- sd.revisit.dist
    revisit.stats[start.location,9] <-  num.loit.pts
    revisit.stats[start.location,10] <- density.loit.pts
    revisit.stats[start.location,11] <- avg.loit.dist
    revisit.stats[start.location,12] <- sd.loit.dist
  }
  
  revisit.stats <- as.data.frame(revisit.stats)
  colnames(revisit.stats) <- c("Radius","Centroid","X","Y","NumRevisitPts","DensityRevisit",
                             "AvgDistRevisit","SdDistRevisit","NumLoitPts","DensityLoiter",
                             "AvgDistLoit", "SdDistLoit")
  return(revisit.stats)
}