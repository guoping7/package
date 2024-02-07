addFirstTwo <- function(vec) {
  # Print the class of the first two elements
  print(class(vec[1]))
  print(class(vec[2]))

  # Add the first two elements if they are numeric
  if (is.numeric(vec[1]) && is.numeric(vec[2])) {
    return(vec[1] + vec[2])
  } else {
    return(NA)  # Return NA if the first two elements are not numeric
  }
}
