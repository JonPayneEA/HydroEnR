#' @title Fetch CDs
#'
#' @param CD can be numeric ID relating to peak flows database or a file path to catchment descriptor file .cd2, .cd3, or .xml
#' @param peakCDs Peak flow database, optional
#'
#' @return
#' @export
#'
#' @examples
#' #fetchCDs(2001, peakFlowDatabase)
fetchCDs <- function(CD, peakCDs){
  if(is.numeric(CD)){
    position <- which(peakCDs$ID == CD)
    if(length(position) == 0){
      stop('Catchment ID does not match registry of peak flows dataset')
      }
    cd <- new("CD", ID = peakCDs$ID[position],
             NAME = peakCDs$NAME[position],
             EASTING = peakCDs$EASTING[position],
             NORTHING = peakCDs$NORTHING[position],
             FEHPOOL = peakCDs$FEHPOOL[position],
             FEHQMED = peakCDs$FEHQMED[position],
             ALTBAR = peakCDs$ALTBAR[position],
             ASPBAR  = peakCDs$ASPBAR[position],
             ASPVAR = peakCDs$ASPVAR[position],
             AREA = peakCDs$AREA[position],
             BFIHOST  = peakCDs$BFIHOST[position],
             BFIHOST19 = peakCDs$BFIHOST[position],
             DPLBAR = peakCDs$DPLBAR[position],
             DPSBAR = peakCDs$DPSBAR[position],
             FARL = peakCDs$FARL[position],
             FPDBAR = peakCDs$FPDBAR[position],
             FPEXT = peakCDs$FPEXT[position],
             FPLOC = peakCDs$FPLOC[position],
             LDP = peakCDs$LDP[position],
             PROPWET = peakCDs$PROPWET[position],
             SAAR = peakCDs$SAAR[position],
             SAAR4170 = peakCDs$SAAR4170[position],
             SPRHOST = peakCDs$SPRHOST[position],
             URBEXT1990 = peakCDs$URBEXT1990[position],
             URBEXT2000 = peakCDs$URBEXT2000[position],
             URBCON1990 = peakCDs$URBCON1990[position],
             URBCON2000 = peakCDs$URBCON2000[position],
             URBLOC1990 = peakCDs$URBLOC1990[position],
             URBLOC2000 = peakCDs$URBLOC2000[position]
    )
  }
  # XML, CD3, CD2, CSV
  # if(is.character(CD)){
  #   type <-tools::file_ext(CD)
  # }
  # if(type == 'csv'){
  #
  # }
  return(cd)
}

