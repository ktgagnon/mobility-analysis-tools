InRadius <- function(distances, radius) {
# Identifies all elements of distances that are within the specified radius.
# Args: 
#     distances: A vector with length greater than 0 indicating the distance from two different locations.
#     radius: An integer specifying the radius an element in distances must be less than to retain.
# Returns: A list where the first member indicates the value for each element in distances that was less than the radius.
#     The second member of the list indicates the position in distances that was less than the radius specified.
  
  if (radius <= 0) {
    stop('radius argument must be larger than 0')
  }
  if (length(distances) <= 0) {
    stop('distances argument must be a vector with length greater than 0')
  }
  
  radius.in <- which(distances < radius)
  dist.radius.in <- distances[radius.in]

  return(list(distances=dist.radius.in, vec.pos=radius.in))
} 