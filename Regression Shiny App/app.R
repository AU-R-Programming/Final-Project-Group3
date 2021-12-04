library(shiny)

devtools::install_github('AU-R-Programming/Final-Project-Group3/Group3Package/FinalProjPackage')
library(FinalProjPackage)

ui <- fluidPage(

    titlePanel("Linear Regression Report Generator"),

    sidebarLayout(
        sidebarPanel(
            ## Taking in an n-length vector from the user for x and y
            textInput("xVector", "Enter X Vector (comma delimited)", 
                      "1,2,5,2,8,2,3,4"),
            textInput("yVector", "Enter Y Vector of equal length
                       (comma delimited)", "0,2,4,6,10,1,3,2"),
            actionButton("start", "Press to Generate")
        ),
        
        mainPanel(
         tabsetPanel(id = "Tabs",
           tabPanel("Beta Coefficients", textOutput("betaC")),
           tabPanel("Beta C-I", textOutput("betaCI")),
           tabPanel("R Squared", textOutput("rSq")),
           tabPanel("Mallow's Cp", textOutput("Cp")),
           tabPanel("Plots", plotOutput("packagePlots"))
         )
        )
    )
)



server <- function(input, output) {
    
    # Found this code that turns textInput into a numeric vector
    # Reference: https://stackoverflow.com/questions/34902765/vector-input-in-shiny-r-and-then-use-it/34921859
    # x <- as.numeric(unlist(strsplit(input$xVector,",")))
    # y <- as.numeric(unlist(strsplit(input$yVector, ",")))
    
    
    # Each tab will need to recalculate everytime the button is pressed
    # so each output gets an eventReactive function 
    
    start <- eventReactive (input$start, {
        x <- as.numeric(unlist(strsplit(input$xVector,",")))
        y <- as.numeric(unlist(strsplit(input$yVector, ",")))
        coeff_beta(y, x)
        })
    
    start2 <- eventReactive (input$start, {
        x <- as.numeric(unlist(strsplit(input$xVector,",")))
        y <- as.numeric(unlist(strsplit(input$yVector, ",")))
        coeff_ci(y, x)
    })
    
    start3 <- eventReactive (input$start, {
        x <- as.numeric(unlist(strsplit(input$xVector,",")))
        y <- as.numeric(unlist(strsplit(input$yVector, ",")))
        coeff_rsquared(y, x)
    })
    
    start4 <- eventReactive (input$start, {
        x <- as.numeric(unlist(strsplit(input$xVector,",")))
        y <- as.numeric(unlist(strsplit(input$yVector, ",")))
        coeff_mallows_Cp(y, x)
    })
    
    start5 <- eventReactive (input$start, {
        x <- as.numeric(unlist(strsplit(input$xVector,",")))
        y <- as.numeric(unlist(strsplit(input$yVector, ",")))
        coeff_plot(y, x)
    })
    
    
    output$betaC <- renderText({
            (start())
    })
    
    output$betaCI <- renderText({
        (start2())
    })
    
    output$rSq <- renderText({
        (start3())
    })
    
    output$Cp <- renderText({
        (start4())
    })
    
    output$packagePlots <- renderPlot({
        (start5())
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
