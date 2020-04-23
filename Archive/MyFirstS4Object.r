Agent <- setClass(
  # Set the name for the class
  "Agent",
  # Define the slots
  slots = c(
    location = "numeric",
    velocity   = "numeric",
    active   = "logical"
  ),
  # Set the default values for the slots. (optional)
  prototype=list(
    location = c(0.0,0.0),
    active   = TRUE,
    velocity = c(0.0,0.0)
  ),
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>100.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  }
)

Calc <-  setClass(
  
  "Calc",
  slots = c( results = "numeric"),
  prototype = list(results = 0)
)


setMethod("Add",
          "Calc",
          function(a,b, object) {
           object@results <- a +b 
          }
)

obj <- new("Calc", results = 0)
Add(obj)
obj@results

