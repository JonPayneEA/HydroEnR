#' @title S4 class, \code{"CD"},  for catchment descriptor files
#'
#' @slot ID numeric.
#' @slot NAME character.
#' @slot EASTING numeric.
#' @slot NORTHING numeric.
#' @slot FEHPOOL character.
#' @slot FEHQMED character.
#' @slot ALTBAR numeric.
#' @slot ASPBAR numeric.
#' @slot ASPVAR numeric.
#' @slot AREA numeric.
#' @slot BFIHOST numeric.
#' @slot BFIHOST19 numeric.
#' @slot CENTROID numeric.
#' @slot DDF numeric.
#' @slot DPLBAR numeric.
#' @slot DPSBAR numeric.
#' @slot FARL numeric.
#' @slot FPDBAR numeric.
#' @slot FPEXT numeric.
#' @slot FPLOC numeric.
#' @slot LDP numeric.
#' @slot PROPWET numeric.
#' @slot RMED1H numeric.
#' @slot RMED1D numeric.
#' @slot RMED2D numeric.
#' @slot SAAR numeric.
#' @slot SAAR4170 numeric.
#' @slot SPRHOST numeric.
#' @slot URBEXT1990 numeric.
#' @slot URBEXT2000 numeric.
#' @slot URBCON1990 numeric.
#' @slot URBCON2000 numeric.
#' @slot URBLOC1990 numeric.
#' @slot URBLOC2000 numeric.
#'
#' @return
#' @export
#'
setClass("CD", slots=list(ID = 'numeric',
                          NAME = 'character',
                          EASTING = 'numeric',
                          NORTHING = 'numeric',
                          FEHPOOL = 'character',
                          FEHQMED = 'character',
                          ALTBAR = 'numeric',
                          ASPBAR  = 'numeric',
                          ASPVAR = 'numeric',
                          AREA = 'numeric',
                          BFIHOST  = 'numeric',
                          BFIHOST19 = 'numeric',
                          CENTROID = 'numeric',
                          DDF = 'numeric',
                          DPLBAR = 'numeric',
                          DPSBAR = 'numeric',
                          FARL = 'numeric',
                          FPDBAR = 'numeric',
                          FPEXT = 'numeric',
                          FPLOC = 'numeric',
                          LDP = 'numeric',
                          PROPWET = 'numeric',
                          RMED1H = 'numeric',
                          RMED1D = 'numeric',
                          RMED2D = 'numeric',
                          SAAR = 'numeric',
                          SAAR4170 = 'numeric',
                          SPRHOST = 'numeric',
                          URBEXT1990 = 'numeric',
                          URBEXT2000 = 'numeric',
                          URBCON1990 = 'numeric',
                          URBCON2000 = 'numeric',
                          URBLOC1990 = 'numeric',
                          URBLOC2000 = 'numeric'
                          )
         )
