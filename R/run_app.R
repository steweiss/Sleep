#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  ...
) {
  with_golem_options(
    app = rmarkdown::run("D:/Users/royde/Nextcloud/Studium/Mathematik/Bachelorarbeit/Sleep/R//Dashboard.Rmd", shiny_args = list(port = 3838, host = "0.0.0.0"))
      
      
    #   shinyApp(
    #   ui = app_ui,
    #   server = app_server,
    #   onStart = onStart,
    #   options = options, 
    #   enableBookmarking = enableBookmarking
    # )
    , 
    golem_opts = list(...)
  )
}
