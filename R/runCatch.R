#' run App
#'
#' runs the app
#'
#' @export


runCatch <- function() {
  
  shiny::shinyApp(ui,server)
  
}