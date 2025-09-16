#' Euclidean Algorithm to find Greatest Common Divisor (GCD)
#'
#' @param a A positive integer.
#' @param b A positive integer.
#' @return The greatest common divisor of `a` and `b`.
#' @description This function implements the Euclidean algorithm to find the GCD of two numbers.
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
euclidean <- function(a, b){
  # Check if both parameters are numeric scalars
  if (!is.numeric(a) || length(a) != 1 || !is.numeric(b) || length(b) != 1){
    stop("Error! Parameters must be numeric scalars")
  }
  # Check if both parameters are integers
  if (a != round(a) || b != round(b)){
    stop("Error! Parameters must be integers")
  }
  # The absolute value of the argument
  x <- abs(a)
  y <- abs(b)
  # The parameters have a situation where zero exists
  if (a == 0 && b == 0){
    return(0)
  }
  if (a == 0){
    return(b)
  }
  if (b == 0){
    return(a)
  }
  # Euclidean algorithm under normal conditions
  while (b != 0){
    rem <- a %% b
    a <- b
    b <- rem
  }
  # Get the greatest common divisor
  return(a)
}

euclidean(123612, 13892347912)
euclidean(100, 1000)

