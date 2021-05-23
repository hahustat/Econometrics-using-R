# Write a function that returns the nth fibonacci number. Think about
# what we just reviewed with regard to writing recurisive functions.

fib <- function(n){
  if(n == 0 || n == 1){
    return(n)
  } else {
    return(fib(n-2) + fib(n-1))
  }
}