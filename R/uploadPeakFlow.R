library(XML)

#' @title Upload peak flow data from a local source
#'
#' @param filepath file path to Peak Flow data folder
#'
#' @return
#' @export
#'
#' @examples
#' #uploadPeakFlow(filepath = 'This/is/not/a/file.path')
uploadPeakFlow <- function(filepath){
  files <- list.files(path = filepath,
                    pattern = "\\.xml$",
                    all.files = TRUE,
                    full.names = TRUE,
                    recursive = TRUE)[-1]
  lst <- list()
  for(i in seq_along(files)){
    result <- xmlParse(file = files[i])
    a <- xmlToList(result)
    lst[[i]] <- data.table(ID = as.numeric(a$GaugingStation$.attrs[3]),
                           NAME = a$GaugingStation$.attrs[2],
                           EASTING = as.numeric(a$CatchmentDescriptors$.attrs[3]),
                           NORTHING = as.numeric(a$CatchmentDescriptors$.attrs[4]),
                           FEHPOOL = a$GaugingStation$Usages[2]$Usage[2],
                           FEHQMED = a$GaugingStation$Usages$Usage[2],
                           ALTBAR = as.numeric(a$CatchmentDescriptors$altbar),
                           ASPBAR  = as.numeric(a$CatchmentDescriptors$aspbar),
                           ASPVAR = as.numeric(a$CatchmentDescriptors$aspvar),
                           AREA = as.numeric(a$CatchmentDescriptors$area),
                           BFIHOST  = as.numeric(a$CatchmentDescriptors$bfihost),
                           BFIHOST19 = as.numeric(a$CatchmentDescriptors$bfihost19),
                           DPLBAR = as.numeric(a$CatchmentDescriptors$dplbar),
                           DPSBAR = as.numeric(a$CatchmentDescriptors$dpsbar),
                           FARL = as.numeric(a$CatchmentDescriptors$farl),
                           FPDBAR = as.numeric(a$CatchmentDescriptors$fpdbar),
                           FPEXT = as.numeric(a$CatchmentDescriptors$fpext),
                           FPLOC = as.numeric(a$CatchmentDescriptors$fploc),
                           LDP = as.numeric(a$CatchmentDescriptors$ldp),
                           PROPWET = as.numeric(a$CatchmentDescriptors$propwet),
                           RMED1H = as.numeric(a$CatchmentDescriptors$rmed_1h),
                           RMED1D = as.numeric(a$CatchmentDescriptors$rmed_1d),
                           RMED2D = as.numeric(a$CatchmentDescriptors$rmed_2d),
                           SAAR = as.numeric(a$CatchmentDescriptors$saar),
                           SAAR4170 = as.numeric(a$CatchmentDescriptors$saar4170),
                           SPRHOST = as.numeric(a$CatchmentDescriptors$sprhost),
                           URBEXT1990 = as.numeric(a$CatchmentDescriptors$urbext1990),
                           URBEXT2000 = as.numeric(a$CatchmentDescriptors$urbext2000),
                           URBCON1990 = as.numeric(a$CatchmentDescriptors$urbconc1990),
                           URBCON2000 = as.numeric(a$CatchmentDescriptors$urbconc2000),
                           URBLOC1990 = as.numeric(a$CatchmentDescriptors$urbloc1990),
                           URBLOC2000 = as.numeric(a$CatchmentDescriptors$urbloc200)
    )
  }
  dt <- rbindlist(lst)
  return(dt)
}
#dt <- uploadPeakFlow(filepath = 'C:/Users/jpayne05/Downloads/NRFAPeakFlow_v11')
