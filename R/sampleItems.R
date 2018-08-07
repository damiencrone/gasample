#' Function to sample items with a genetic algorithm maximizing some measure of 
#' fitness (e.g., everage inter-item dissimilarity) as measured by an inter-item
#' distance matrix.
#' 
#' @param distance_mat a population inter-item distance matrix
#' @param sample_size the number of items to sample
#' @param fitness a fitness function to pass to ga()
#' @param lower_tri_funciton the specific function to assess fitness of the 
#'   lower triangle of the sample distance matrix
#' @param n_suggestions number of suggestions to initialise the GA with
#' @param maxiter maximum number of iterations to run GA
#' @param required_items character vector of any items required to be included
#'   in the solution
#' @param seed seed for GA
#' @return a list containing the final items and a GA object
#' @export
sampleItems = function (distance_mat, sample_size, fitness = fitnessFunction,
                        lower_tri_funciton = mean, n_suggestions = 500,
                        maxiter = 1e6, required_items = NULL, seed = NULL) {
  
  require(GA)
  
  items = rownames(distance_mat)
  
  suggestion_mat = generateSuggestions(
    items = items,
    sample_size = sample_size,
    n_suggestions = n_suggestions,
    required_items = required_items
  )
  
  ga_output = ga(
    
    type = "binary", 
    fitness = fitness,
    nBits = nrow(distance_mat),
    maxiter = 1e6, # Maximum number of generations 
    run = 200,     # Stop if the best-so-far fitness
                   # hasn't improved for 'run' generations 
    popSize = nrow(suggestion_mat), 
    seed = seed,
    suggestions = suggestion_mat,
    
    # Additional fitness function inputs
    sample_size        = sample_size,
    distance_mat       = distance_mat,
    lower_tri_function = lower_tri_funciton,
    required_items     = required_items
    
  )
  
  stim_sample = list(
    final = items[as.logical(ga_output@solution)],
    ga_output = ga_output
  )
  
  return(stim_sample)
  
}