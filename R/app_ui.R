#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


library(shiny)
library(knitr)
# rmdfiles <- c("D:/Users/royde/Nextcloud/Studium/Mathematik/Bachelorarbeit/Sleep/R/Dashboard.Rmd")
# sapply(rmdfiles, knit, quiet = T)
# rmarkdown::render(
#   input = "D:/Users/royde/Nextcloud/Studium/Mathematik/Bachelorarbeit/Sleep/R/Dashboard.Rmd",runtime="shiny")



app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      rmarkdown::run("D:/Users/royde/Nextcloud/Studium/Mathematik/Bachelorarbeit/Sleep/R/Dashboard.Rmd", shiny_args = list(port = 3838, host = "0.0.0.0"))
      
      # includeHTML("D:/Users/royde/Nextcloud/Studium/Mathematik/Bachelorarbeit/Sleep/R/Dashboard.html")
      # withMathJax(includeMarkdown("Dashboard.md")
                  )
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Sleep'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

