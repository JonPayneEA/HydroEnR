monthPlot.HydroAggs <- function(x, name = 'Gauge', polar = FALSE, snip = NULL, ...) {
  dt <- x$Monthly
  #dt[, c("Year", "Month") := tstrsplit(Year_Month, " ", fixed=TRUE)]
  dt$Year_Month <- gsub(" ", "-", dt$Year_Month)
  dt$Year_Month <- as.Date(paste(dt$Year_Month,"-01",sep=""))
  dt <- headTail(dt, n = snip)
  p <- ggplot(dt, aes(x = month(Year_Month), y = Monthly_Max, group = year(Year_Month),colour = year(Year_Month))) +
    geom_line(size = 1) +
    xlab("Month") +
    ylab(expression(Flow ~ m^3 ~ s^-1)) +
    ggtitle(paste("Season plot of ", name, sep = "")) +
    labs(colour = 'Year') +
    scale_color_gradient(low = '#D2DE26', high = '#00A33B') +
    scale_x_continuous(breaks = sort(unique(month(dt$Year_Month))), labels = month.abb) +
    theme_light()
  if(polar == TRUE) {
    p <- p + coord_polar()
  }
  return(p)
}

monthPlot <- function(x,...) {
  UseMethod('monthPlot', x)
}
