FindSequences <- function(positions){
# Finds consecutive sequences in vector 'positions'. This is useful for finding streaks of consistent behaviors or trends in a vector.
# Args: positions is a vector.
# Returns: seq.index is a N x 2 matrix where the first column corresponds to the beginning index for each sequence found, and the second column the end index for each sequence.
#           Each new sequence found is allocated it's own row in the matrix.            
  
  if (any(diff(positions)>1)) {
  seq.start <- c(1, which(diff(positions)>1) + 1)
  seq.end <- c(which(diff(positions)>1), length(positions))
  } else {
    seq.start <- 1
    seq.end <- length(positions)
  }
    sequence.pos <- cbind(seq.start,seq.end)

  return(seq.index=sequence.pos)
}