# This implements a version of the General Context Model (GCM) for
#   categorization, specifically modified to include perceptual or decision
#   noise and a response bias.

GCMprednoisy <- function(probe, exemplars, c, w, sigma, b){
  
  # sigma = The standard deviation (SD) of noise. The stochasticity parameter
  #         that controls the amount of Gaussian noise in the decision process.
  
  # b = The response bias parameter.
  #     Represents a preference for one category over the other, independent of
  #     the stimulus evidence.
  
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
  for (ex in exemplars){
    dist[[length(dist)+1]] <- apply(as.array(ex), 1, 
                                    function(x) sqrt(sum(w*(x-probe)^2)))
  }
  
  sumsim <- unlist(lapply(dist, function(a) sum(exp(-c*a))))
  
  # this only works for 2 categories
  # we also simplify Nosofsky model in only applying noise at the end
  
  r_prob <- c(0,0)
  r_prob[1] <- pnorm(sumsim[1]-sumsim[2]-b,sd=sigma)
  # This line calculates the probability of choosing Category A (r_prob[1])
  #   using the cumulative distribution function of the Normal distribution
  #   (pnorm).
  #
  # The probability of choosing A is the probability that the noisy decision
  #   evidence exceeds a threshold.
  #
  # −b: A constant bias. If b>0, the net evidence must be higher to choose A
  #     (i.e., it biases the response toward B). If b<0, it biases the response
  #     toward A.
  r_prob[2] <- 1 - r_prob[1]
  return(r_prob)
  

  
}