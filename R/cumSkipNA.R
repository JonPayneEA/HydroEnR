#' @title cumsumNA
#'
#' @description Provides a cumulative sum of rain gauge data while excluding NA
#' values.
#'
#' @param x Raw rain gauge data or merged dataset
#' @param ... Additional parameters as required
#'
#' @return
#' @export
#'
#' @examples
#' cumsumNA(rain)
cumsumNA <- function(x, ...) {
  UseMethod('cumsumNA', x)
}

#' @rdname cumsumNA
#' @export
cumsumNA.numeric <- function(x){
  v <- cumsum(ifelse(is.na(x), 0, x)) + x*0
  return(v)
}

#' @rdname cumsumNA
#' @export
cumsumNA.rainAll <- function(x, offset = 0){
  tmp_list <- list()
  for(i in tail(seq_along(x),-1)){
    v <- as.numeric(unlist(x[,i, with = FALSE]))
    v1 <- cumsum(ifelse(is.na(v), 0, v)) + v*0
    tmp_list[[i]] <- data.table(v1)
  }
  tmp_list[[1]] <- x[,1, with = FALSE]
  dt <- as.data.table(tmp_list)
  colnames(dt) <- colnames(x)
  class(dt) <- append(class(dt), 'cumulRain')
  return(dt)
}

#' @rdname cumsumNA
#' @export
cumsumNA.rainAllDaily <- function(x, offset = 0){
  tmp_list <- list()
  for(i in tail(seq_along(x),-1)){
    v <- as.numeric(unlist(x[,i, with = FALSE]))
    v1 <- cumsum(ifelse(is.na(v), 0, v)) + v*0
    tmp_list[[i]] <- data.table(v1)
  }
  tmp_list[[1]] <- x[,1, with = FALSE]
  dt <- as.data.table(tmp_list)
  colnames(dt) <- colnames(x)
  class(dt) <- append(class(dt), 'cumulRain')
  return(dt)
}

#' @rdname cumsumNA
#' @export
cumsumNA.rainAllMonthly <- function(x, offset = 0){
  tmp_list <- list()
  for(i in tail(seq_along(x),-1)){
    v <- as.numeric(unlist(x[,i, with = FALSE]))
    v1 <- cumsum(ifelse(is.na(v), 0, v)) + v*0
    tmp_list[[i]] <- data.table(v1)
  }
  tmp_list[[1]] <- x[,1, with = FALSE]
  dt <- as.data.table(tmp_list)
  colnames(dt) <- colnames(x)
  class(dt) <- append(class(dt), 'cumulRain')
  return(dt)
}

#' @rdname cumsumNA
#' @export
cumsumNA.rainAllAnnual <- function(x, offset = 0){
  tmp_list <- list()
  for(i in tail(seq_along(x),-1)){
    v <- as.numeric(unlist(x[,i, with = FALSE]))
    v1 <- cumsum(ifelse(is.na(v), 0, v)) + v*0
    tmp_list[[i]] <- data.table(v1)
  }
  tmp_list[[1]] <- x[,1, with = FALSE]
  dt <- as.data.table(tmp_list)
  colnames(dt) <- colnames(x)
  class(dt) <- append(class(dt), 'cumulRain')
  return(dt)
}


