monthPlot.HydroAggs <- function(x, name = 'Gauge', polar = FALSE, snip = NULL, method = 'max', ...) {
  dt1 <- x$Monthly
  dt1$Year_Month <- gsub(' ', '-', dt1$Year_Month)
  dt1$Year_Month <- as.Date(paste(dt1$Year_Month,'-01',sep=''))
  colnames(dt1) <- c('Year_Month', 'Stat')
  dt1 <- headTail(dt1, n = snip)
  p <- ggplot(dt1, aes(x = month(Year_Month), y = Stat, group = year(Year_Month),colour = year(Year_Month))) +
    geom_line(size = 1) +
    xlab('Month') +
    ggtitle(paste('Season plot of ', method, ' flows at ',  name, sep = '')) +
    labs(colour = 'Year') +
    scale_color_gradient(low = '#D2DE26', high = '#00A33B') +
    scale_x_continuous(breaks = sort(unique(month(dt1$Year_Month))), labels = month.abb) +
    theme_light()
  if('HydroAggssum' %in% class(dt)){
    p <- p + ylab(expression(Flow ~ m^3))
  } else {
    p <- p + ylab(expression(Flow ~ m^3 ~ s^-1))
  }
  if(polar == TRUE) {
    p <- p + coord_polar()
  }
  return(p)
}

monthPlot.FlowLoad <- function(x, name = 'Gauge', polar = FALSE, snip = NULL, method = 'max', ...) {
  dt1 <- monthlyAgg(x, method = method)
  dt1$Year_Month <- gsub(' ', '-', dt1$Year_Month)
  dt1$Year_Month <- as.Date(paste(dt1$Year_Month,'-01',sep=''))
  colnames(dt1) <- c('Year_Month', 'Stat')
  dt <- headTail(dt1, n = snip)
  p <- ggplot(dt1, aes(x = month(Year_Month), y = Stat, group = year(Year_Month),colour = year(Year_Month))) +
    geom_line(size = 1) +
    xlab('Month') +
    ggtitle(paste('Season plot of ', method, ' flows at ',  name, sep = '')) +
    labs(colour = 'Year') +
    scale_color_gradient(low = '#D2DE26', high = '#00A33B') +
    scale_x_continuous(breaks = sort(unique(month(dt1$Year_Month))), labels = month.abb) +
    theme_light()
  if('HydroAggssum' %in% class(Buildwas_Analysis)){
    p <- p + ylab(expression(Flow ~ m^3))
  } else {
    p <- p + ylab(expression(Flow ~ m^3 ~ s^-1))
  }
  if(polar == TRUE) {
    p <- p + coord_polar()
  }
  return(p)
}

monthPlot <- function(x,...) {
  UseMethod('monthPlot', x)
}

# Examples
# monthplot(Buildwas_Analysis, method = 'sum', name = 'Buildwas', polar = FALSE)
# monthplot(Buildwas_Analysis, name = 'Buildwas', polar = TRUE)
# monthPlot(Buildwas)
