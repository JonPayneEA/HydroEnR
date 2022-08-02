# NA aggregate plots - done by year


#' @title plotNA
#'
#' @description Shows a proportions of missing data for each year.
#'
#' @description Further updates will include flow and stage data.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' plotNA(rain)
#'
plotNA <- function(x){
  UseMethod('plotNA', x)
}

#' @rdname plotNA
#' @export
plotNA.rainAll <- function(x){
  cols <- dim(a)[2]
  a1 <- x[ , lapply(.SD,FUN = function(x) sum(!is.na(x))), by = year(DateTime)]
  a1$Missing <- rep('OK', dim(a1)[1])
  a2 <- x[ , lapply(.SD, length), by = year(DateTime)]
  a3 <- data.table(year = a1$year,
                   a2[, 2:7] - a1[, 2:7],
                   Missing = rep('NA', dim(a1)[1]))
  a4 <- rbind(a1, a3)
  NAs_melt <- data.table::melt(a4, id.vars = c('year', 'Missing'))
  p <- ggplot(NAs_melt, aes(fill = Missing, y = value, x = year)) +
    geom_bar(position = 'fill', stat = 'identity') +
    facet_wrap(~variable, nrow = 2)
  return(p)
}

# plotNA.rainAll(rain)

