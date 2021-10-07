shuffle <- function(input_vector, seed=100) {

  N <- length(input_vector)
  HALF <- as.integer(N/2)
  set.seed(seed)
  odr <- sample(N, N)
  input_vector <- input_vector[odr]

  for(i in 1:HALF) {
    tmp <- input_vector[i]
    input_vector[i] <- input_vector[i+HALF]
    input_vector[i+HALF] <- tmp
  }
  return(input_vector)
}
