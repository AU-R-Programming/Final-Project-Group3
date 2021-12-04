library(shiny)

# devtools::install_github('AU-R-Programming/Final-Project-Group3/Group3Package/FinalProjPackage')
library(FinalProjPackage)

ui <- fluidPage(

    titlePanel("Interactive Optimized Linear Regression Using 'mtcars' Set"),

    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("independentVariables",
                               "Select Independent Variables:",
            c("Cylinders" = "cyl", "Horsepower" = "hp",
              "Rear Axle Ratio" = "drat", "Weight" = "wt", "1/4 Mile Time" = "qsec",
              "# of Forward Gears" = "gear", "# of Carburetors" = "carb")),
            
            actionButton("generate", "Generate Output")
        ),

        mainPanel(
           plotOutput("packagePlots"),
           br(),
           textOutput("beta_hat"),
           br(),
           textOutput("beta_CI"), 
           br(), 
           textOutput("f_test"),
           br(),
           textOutput("r_sq"),
           br(),
           textOutput("C_p")
           
        )
    )
)


server <- function(input, output) {
    
    generate <- eventReactive(input$generate, {
        coeff_plot(as.matrix(mtcars[, input$independentVariables]), as.vector(mtcars[, 1]))
    })
    
    generate2 <- eventReactive(input$generate, {
        coeff_beta(as.matrix(mtcars[, input$independentVariables]), as.vector(mtcars[, 1]))
    })
    
    generate3 <- eventReactive(input$generate, {
        coeff_ci(as.matrix(mtcars[, input$independentvariables]), as.vector(mtcars[, 1]))
    })
    
    generate4 <- eventReactive(input$generate, {
        coeff_ftest(as.matrix(mtcars[, input$independentVariables]), as.vector(mtcars[, 1]))
    })
    
    generate5 <- eventReactive(input$generate, {
        coeff_rsquared(as.matrix(mtcars[, input$independentVariables]), as.vector(mtcars[, 1]))
    })
    
    generate6 <- eventReactive(input$generate, {
        coeff_mallows_Cp(as.matrix(mtcars[, input$independentVariables]), as.vector(mtcars[, 1]))
    })
    
    output$packagePlots <- renderPlot({
        generate()
        }
    )
    
    output$beta_hat<- renderText({
        generate2()
    }
    )
    
    output$beta_ci <- renderText({
        generate3()
    }
    )
    
    output$f_test <- renderText({
        generate4()
    }
    )
    
    output$r_sq <- renderText({
        generate5()
    }
    )
    
    output$c_p <- renderText({
        generate6()
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
