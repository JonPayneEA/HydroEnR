# Dim version that can handle 1D vectors
dimC <- function(item) {
  if (is.null(base::dim(item)) ) {
    dims<-length(item)
  } else{
    dims  <- base::dim(item)
  }
  return(dims)
}

headTail <- function(x, n = NULL){
  # print the head and tail together
  if(is.null(n)){
    n <- dimC(x)[1]
  }
  if(n < 0){
    dt <-head(x,n*-1)
  } else {
    dt <- tail(x, n)
  }
  return(dt)
}

# headTail(Buildwas_Analysis$Hydro_year, 10)
