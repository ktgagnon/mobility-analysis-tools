DistancesFrom <- function(x, y, start.location, end.location){  
# Calculates euclidean distance between start.location and all other locations in x-y trajectory up to end.location.
# Args: 
#     x and y: Vectors of equal length describing a trajectory through space.
#     start.location: An integer specifying where in x-y vectors to begin distance calculation.
#     end.location: An interger specifying where in x-y vectors to end distance calculation.
# Returns: A list where the first member indicates the euclidean distance between start.location in x-y and all x-y points up to (and including) end.location.
#     The second member of the list indicates the position in x-y vector corresponding to the distance calculation.
  
  x <- x 
  y <- y  
  dist <- matrix(0,length(start.location:end.location),1)
  
  if (length(x) != length(y)){
    stop('Length of input vectors are not equal.')        
  }

  if (start.location > length(x)){
    stop('start.location exceeds length of input vector. 
      start.location must be an integer between 1 and the length of input vector')
  }

    dist <- sqrt((x[(start.location):end.location] - x[start.location]) ^ 2 +  (y[(start.location):end.location] - y[start.location]) ^ 2)

  return(list(distances=dist, vec.pos=start.location:end.location))
}