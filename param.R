create_pairs <-function(x){
  n <- length(x)
  m <- matrix(NA,ncol=2,nrow = (n * (n - 1)/2))

  k <- 1
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      m[k,1] <- x[i]
      m[k,2] <- x[j]
      k <- k + 1
    }
  }
  return(m)
}


param <- function(){
  #main parameters as a list
  param <- as.list(read())
  param[["Date"]] <- read()[,1] 
  param[["Names"]] <- colnames(read())[-1]
  param[["WindowSize"]] <- c(50,100,200, 500)
  param[["CorrelLag"]] <- c(0,1)
  param[["Correl"]] <-list() 
  assertthat::assert_that(!schoolmath::is.negative(min(param[["CorrelLag"]])), msg = "Correlation lag cannot be negativ!")
  
  #param[["Pairs"]] <-gtools::permutations(n=number_of_products,r=2,v=param[["Names"]], repeats.allowed = F)
  param[["Pairs"]] <- create_pairs(param[["Names"]])
  return(param)
}
