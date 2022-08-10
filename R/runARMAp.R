#' @title Run ARMAp shiny dashboard
#'
#' @description This function opens the ARMAp shiny tool.
#'
#' @export
runARMAp <- function() {
  appDir <- system.file("shiny-examples", "ARMAp.R", package = "HydroEnR")
  if(appDir == ""){
    stop("Could not find example directory. Try re-installing `HydroEnR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
