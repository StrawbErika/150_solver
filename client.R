library(shiny)
source("./server.R")

ui = fluidPage(
  
  navbarPage("CMSC 150 - Basilio",
    navbarMenu("Generic Solvers",
      tabPanel(div("Polynomial Regression", style="color: red"),
        titlePanel(div("Polynomial Regression", style="color:red")),

        sidebarPanel(
          fileInput(
            "fileInput", 
            "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          # Choose to display all data
          checkboxInput("dispAll", "Display All", FALSE),
          checkboxInput("sorted", "Sorted by x", FALSE),
          
          sliderInput(
            "integer", 
            label = "Degree",
            min = 1, 
            max = 10,
            step = 1,
            pre="n = ",
            value=0
          )
        ),
      
        mainPanel(
          tableOutput("fileContents")
        )
      ),
    
    
      # ================= QSI ================= #
      tabPanel(div("Quadratic Spline Interpolation", style="color: blue"),
        titlePanel(div("Quadratic Spline Interpolation", style="color: blue")),
          sidebarPanel(
            
          )
      )
    ),
  
      
      
      
    tabPanel("Simplex Solver",
      sidebarPanel(
        
      )
    )
    

    
  )
)

shinyApp(ui, server)
