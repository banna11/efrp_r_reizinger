all_correlations <- function(){
  
  solution <-apply(param()[["Pairs"]],1, 
                   FUN=function(x){
                     solution <- c(korrel(first = x[1], second = x[2])) 
                     return(solution)
                   }
  )
  # param()[["Pairs]] includes the all possible pairs we should iterate
  # iteration by rows
  # the korrel function is applied to rows
  
  
  colnames(solution) <- paste0(param()[["Pairs"]][,1],"-",param()[["Pairs"]][,2]) # naming correlation matrix
  #tslength <-length(param()[["Date"]]) - select.param()[["windowsize"]] -  select.param()[["correllag"]] # including dates# ERROR
  #rownames(solution) <- (param()[["Date"]][-((tslength+1):length(param()[["Date"]]))])
  # the corralations are ordered to the last Date of the window
  #it is not obligatory
  
  sdate <- (param()[["Date"]])
  
  sdate <-as.Date(as.vector(as.matrix(sdate)))[(select.param()[["windowsize"]]+select.param()[["correllag"]] + 1):dim(sdate)[1]]
  # unfortunately, sdate is in tibble format wich is not compatible with matrix format, but Date is
  # to convert sdate to date a vector: tibble->matrix->vector->Date
  # the rolling correlation coefficients are assigned to the last day of the window
  # the choose of date determines the vector in sdate
  
  rownames(solution) <- sdate
  solution <- xts::as.xts(ts(solution, start = sdate[1], end = xts::last(sdate)))

  return(solution)
}