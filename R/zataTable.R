# Convert AMAX data from rnrfa into the AMAX class

#' @title zataTable
#'
#' @description Converts AMAX data taken from NRFA from type zoo to data.table.
#' Maintaining consistency in package.
#'
#' @param x data in zoo format
#' @param ... Extra parameters if requirred
#'
#' @return
#' @export
#'
#' @examples
#' rnrfa::get_ts(id = 2001, type = 'amax-flow') %>% zataTable()
zataTable <- function(x, ...) {
  UseMethod('zataTable', x)
}

#' @rdname zataTable
#' @export
zataTable.zoo <- function(x, index.name = "Date") {
  #stopifnot(class(x) == 'zoo')
  xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
  setNames(data.table(attr(x, 'index'), x, row.names=NULL), c(index.name,xn))
}

