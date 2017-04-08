#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "ValUSunSSN")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ValUSunSSN`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
