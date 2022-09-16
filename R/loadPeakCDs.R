#' @title Load CD data from NRFA
#'
#' @param local defaults as NULL. To load a local version of the peak flow database and a file link.
#'
#' @return
#' @export
#'
#' @examples
#' loadPeakCDs() %>%
#'   head()
loadPeakCDs <- function(local = NULL){
  if(is.null(local)){
    df <- rnrfa::catalogue()
    dt <- data.table(ID = df$id,
                     NAME = df$name,
                     EASTING = df$easting,
                     NORTHING = df$northing,
                     LAT = df$latitude,
                     LONG = df$longitude,
                     FEHPOOL = df$`feh-pooling`,
                     FEHQMED = df$`feh-qmed`,
                     FEHNO = df$`feh-neither`,
                     REJECTED = df$`peak-flow-rejected-amax-years`,
                     QMED = df$qmed,
                     ALTBAR = df$altbar,
                     ASPBAR  = df$aspbar,
                     ASPVAR = df$aspvar,
                     AREA = df$`catchment-area`,
                     BFIHOST  = df$bfihost,
                     BFIHOST19 = df$bfihost,
                     CENTROID = NULL,
                     DDF = NULL,
                     DPLBAR = df$dplbar,
                     DPSBAR = df$dpsbar,
                     FARL = df$farl,
                     FPDBAR = NULL,
                     FPEXT = df$`mean-flood-plain-extent`,
                     FPLOC = NULL,
                     LDP = df$ldp,
                     PROPWET = df$propwet,
                     SAAR = df$`saar-1961-1990`,
                     SAAR4170 = df$`saar-1941-1970`,
                     SPRHOST = df$sprhost,
                     URBEXT1990 = df$`urbext-1990`,
                     URBEXT2000 = df$`urbext-2000`,
                     URBCON1990 = df$`urbconc-1990`,
                     URBCON2000 = df$`urbconc-2000`,
                     URBLOC1990 = df$`urbloc-1990`,
                     URBLOC2000 = df$`urbloc-2000`
    )
  } else {
    print('Reminder to add local functionality')
    stop('Contact JP regarding functionality')
  }
  return(dt)
}

