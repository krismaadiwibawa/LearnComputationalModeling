# The function that implements the mathematical core of the GCM, calculating
# the predicted probability of a specific category response (e.g., Category A).

GCMpred <- function(probe, exemplars, c, w) {
  
  # probe = A vector representing the new stimulus to be categorized
  #   (e.g., the features of a single face). This is the new object whose
  #   category is unknown.
  
  # exemplars = The memory of past category members.
  #   A list of matrices. exemplars[[1]] (row 1) holds the memory traces
  #   for Category A; exemplars[[2]] (row 2) holds traces for Category B.
  
  # c = A single, positive scalar called the scaling parameter. It controls
  #   the steepness of the similarity function (how quickly similarity drops
  #   as distance increases).
  
  # w = A vector of attention weights (one weight for each stimulus
  #   dimension/feature). It controls the relative importance of each feature
  #   (dimension) during comparison.
  
  # calculate likelihod of N_A `A' responses out of N given parameter c
  # 'stim' is a single vector representing the stimulus to be categorised
  # 'exemplars' is a list of exemplars; the first list item is the 'A' exemplars
  #   in memory, and the second list item is the `B` exemplars in memory
  #   each list item is a matrix in which the rows correspond to individual
  #   exemplars
  # 'c' is the scaling parameter, and 'w' is a vector giving weighting for each
  #   stimulus dimension (the columns in 'stim' and 'exemplars')
  
  # note: for a large number of categories we could use lapply to loop across
  # the categories in the list 'exemplars'
  
  dist <- list()
  for (ex in exemplars) {
    dist[[length(dist)+1]] <- apply(as.array(ex), 1, function(x) sqrt(sum(w*(x-probe)^2)))
  }
  
  # This loop calculates the psychological distance between the probe and
  #   every single exemplar in memory. This uses the weighted Euclidean
  #   distance formula. This loop calculates the distances of every exemplar
  #   (5 exemplar for category A; 5 exemplar for category B) with each probe.
  #   
  #   The '1' in apply() means "loop by row".
  #
  #   w is multiplied by the squared difference between the probe and the
  #   exemplar, ensuring that highly attended dimensions (w is large)
  #   contribute more to the distance.
  #
  # The result is a list (dist) where dist[[1]] is a vector of distances to
  #   all A exemplars, and dist[[2]] is a vector of distances to all B exemplars.
  
  sumsim <- lapply(dist, function(a) sum(exp(-c*a)))
  
  # The 'lapply' loop converts the distance (a dissimilarity measure) into
  #   psychological similarity using a negative exponential function.
  #
  # a is the vector of distances for one category.
  #
  # sum(exp(-c*a)) calculates the summed similarity (or category activation)
  #   for that category. The parameter c (scaling parameter) modulates how
  #   quickly similarity falls off with distance.
  #
  # The result is a list (sumsim) where sumsim[[1]] is the total
  #   similarity/activation for Category A, and sumsim[[2]] is the total for
  #   Category B.
  
  r_prob <- unlist(sumsim)/sum(unlist(sumsim))
  
  # The final step converts the category activations into response probabilities
  #   using the ratio rule (specifically, Luce's choice rule)
  #   Mathematically: P(A) = Activation_A / (Activation_A + Activation_B)
  #                        = Summed(Similarity Probe,Exemplar_A) /
  #                          (Summed(Similarity Probe,Exemplar_A) +
  #                          Summed(Similarity Probe,Exemplar_B))
  
}
