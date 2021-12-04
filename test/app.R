#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    helpText
    checkboxGroupInput("variable", "Variables to show:",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
    tableOutput("data")
)

server <- function(input, output, session) {
    output$data <- renderTable({
        mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
}

shinyApp(ui, server)
