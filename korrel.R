korrel <-function(first="CL1",second="CL2")
{
  # calc correlation
  # windowsize & correlag derive from another function
  # it makes easier to create labels
  windowsize <- select.param()["windowsize"]
  correllag <- select.param()["correllag"]
  
  
  N <- nrow(read()) # length of time series
  dN <- N - windowsize - correllag # shorter ts length
  # assertthat::validate_that(!schoolmath::is.negative(dN)==T)
  
  
  
  shift <- matrix(rep(0:(dN-1),each=windowsize), byrow = TRUE, ncol=windowsize)
  # shift to select proper windows for correlation calculation
  
  shift_lag <- shift + correllag 
  # shift should be lagged by correllag
  
  w <- matrix(rep(c(1:windowsize), dN), byrow = TRUE, ncol = windowsize, nrow=dN)
  w_0lag <- w + shift
  # first matrix select indexes for the first variable
  w_lag <- w + shift_lag
  # second matrix select indexes for the second variable

  number_of_correlations <- dim(param()[["Pairs"]])[1]
  
  # select time series
  aa <- matrix(param()[[first]][w_0lag], byrow=F, ncol=windowsize)#[1,]
  bb <- matrix(param()[[second]][w_lag], byrow=F, ncol=windowsize)#[1,]
  
  corr_vector <- lineup::corbetw2mat(t(aa),t(bb))
  # calculate correlation between matrix columns by pairs
  # transpose is needed, thus the comparable data are in matrix rows
  
  date <- dplyr::slice(param()[["Date"]],((windowsize+1):dim(param()[["Date"]])[1]))
  mat <- xts::as.xts(ts(corr_vector))

  names(mat)<- paste0(first,"-", second) # naming the correlation vector
  
  
  return(mat)
}