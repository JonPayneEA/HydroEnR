#' @title HydroYearDay
#'
#' @description Creates fields for hydrological year and hydrological day.
#'
#' @description Speed is slow, needs conversion to C++ in future iterrations.
#'
#' @param d Array of dates of class Date
#' @param hy_cal Set to 'oct_us_gb' (USA and UK), but can also be 'sep_br'
#' (Brazil), 'apr_cl' (Chille).
#'
#' @return
#' @export
#'
#' @examples
#' HydroYearrDay(Buildwas$GaugeData$DateTime, hy_cal = 'oct_us_gb')
HydroYearDay<-function(d,hy_cal = 'oct_us_gb'){

  # note: this function includes get_hydro_year and should be used instead

  # input variables:
  # d: array of dates of class Date
  # hy_cal: hydrological year calendar, current options are 'oct_us_gb', 'sep_br' and 'apr_cl'

  if(class(d)!='Date'){stop('d should be of class Date - use as.Date')}

  m<-as.numeric(month(d)) # extract month
  y<-as.numeric(year(d)) # extract year
  HydrologicalYear <-y                         # create array for hydrological year

  if(hy_cal=='oct_us_gb'){      # USA and Great Britian

    HydrologicalYear[m>=10]<-(HydrologicalYear[m>=10]+1)    # hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    start_hy<-as.Date(paste0(HydrologicalYear-1,'-10-01'))

  } else if(hy_cal=='sep_br'){  # Brazil

    HydrologicalYear[m>=9]<-(HydrologicalYear[m>=9]+1)      # hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    start_hy<-as.Date(paste0(HydrologicalYear-1,'-09-01'))

  } else if(hy_cal=='apr_cl'){  # Chile

    HydrologicalYear[m<=3]<-(HydrologicalYear[m<=3]-1)      # hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    start_hy<-as.Date(paste0(HydrologicalYear,'-04-01'))

  } else {

    stop(paste0('Unkown hydrological year calendar:',hy_cal))

  }

  day_of_hy<-as.numeric(d-start_hy+1) # days since the beginning of the hydro year

  if(any(day_of_hy<1|day_of_hy>366)){

    stop('Error when computing day of hydro year')

  }

  return(data.frame(HydrologicalYear,day_of_hy))

}

