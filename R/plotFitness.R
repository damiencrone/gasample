#' Function to plot the fitness of the final GA result, relative to the randomly
#' generated suggestions.
#' 
#' @param stim_sample a list containing the final items and a GA object
#' @param sample_size the number of items to sample
#' @param distance_mat a population inter-item distance matrix
#' @param fitness the fitness function passed to ga()
#' @param lower_tri_funciton the specific function to assess fitness of the 
#'   lower triangle of the sample distance matrix
#' @export
plotFitness = function (stim_sample, sample_size, distance_mat, fitness = fitnessFunction,
                        lower_tri_function = sumHeight) {
  
  suggestion_quality = apply(
    X = stim_sample$ga_output@suggestions,
    MARGIN = 1,
    FUN = fitness,
    sample_size = sample_size,
    distance_mat = distance_mat,
    lower_tri_function = lower_tri_function
  )
  
  final_fitness = stim_sample$ga_output@fitnessValue
  
  plot(
    x = density(suggestion_quality, adjust = 1/2),
    xlim = range(c(suggestion_quality, final_fitness)),
    xlab = "Fitness",
    main = "Comparison of initial random samples to final GA result"
  )
  
  points(x = final_fitness, y = 0, pch = 19, col = "red")
  
}
