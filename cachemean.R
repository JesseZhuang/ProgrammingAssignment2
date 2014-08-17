cachemean <- function(x, ...) {
   m <- x$getmean()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   ## no matter cache mean exist or not code below will be
   ## executed anyway since these codes are not in if
   
   # this calls function x$get 
   # which returns x(input of makeVector function)
   data <- x$get()
   m <- mean(data, ...)
   x$setmean(m)
   m
}