library(shiny)
# library(shinyjs)
source("./server.R")

# TODO: Sort by y
ui = fluidPage(
  useShinyjs(),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #f44e42}")),
  tags$head(tags$style("#answerFunctionPR{color:#f44e42; font-size:12px; font-style:italic; 
overflow-y:scroll; max-height: 100px; background: ghostwhite;}")),
  
  navbarPage("CMSC 150 - Basilio",
    navbarMenu("Generic Solvers",
      tabPanel(div("Polynomial Regression", style="color: #f44e42"),
        titlePanel(div("Polynomial Regression", style="color:#f44e42")),

        sidebarPanel(
          fileInput(
            "fileInputPR", 
            "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          # Choose to display all data
          checkboxInput("dispAllPR", "Display All", FALSE),
          checkboxInput("sortedXPR", "Sort by x", FALSE),
          checkboxInput("headerCheckPR", "File has header", FALSE),
          br(),
          sliderInput(
            "degreeNPR", 
            label = "Degree",
            min = 1, 
            max = 10,
            step = 1,
            pre="n = ",
            value = 0
          ),
          actionButton("solveButtonPR", "Generate Function", style="background-color:#f44e42; color:white"),
          hr(),
          hidden(
            div(id="funcLabel", "Function: ")
          ),
          verbatimTextOutput("answerFunctionPR"),
          hidden(
            numericInput("xInputPR", "Solve for X", 0),
            actionButton("solveXPR", "SOLVE", style="background-color:#f44e42; color:white")
          ),
          hr(),
          hidden(
            div(id="ansLabel", "f(x): ")
          ),
          verbatimTextOutput("answerGivenX")

        ),
      
        mainPanel(
          tableOutput("fileContentsPR")
        )
      ),
    
    
      # ================= QSI ================= #
      tabPanel(div("Quadratic Spline Interpolation", style="color: #429ef4"),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #429ef4}")),
        titlePanel(div("Quadratic Spline Interpolation", style="color: #429ef4")),
        
        sidebarPanel(
          fileInput(
            "fileInputQSI", 
            "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          checkboxInput("dispAllQSI", "Display All", FALSE),
          checkboxInput("sortedXQSI", "Sort by x", FALSE),
          checkboxInput("headerCheckQSI", "File has header", FALSE),
          br(),
          actionButton("solveButtonQSI", "Generate Functions", style="background-color:#429ef4; color:white"),
          hr(),
          hidden(
            numericInput("xInputQSI", "Solve for X", 0),
            actionButton("solveXQSI", "SOLVE", style="background-color:#429ef4; color:white")
          ),
          hr(),
          hidden(
            div(id="ansLabelQSI", "f(x): ")
          ),
          verbatimTextOutput("answerGivenXQSI")
        ),
      
        mainPanel(
          tableOutput("fileContentsQSI"),
          tableOutput("generatedFunctions")
        )
      )
    ),
    
    # =============== SIMPLEX SOLVER =============== #
    tabPanel("Simplex Solver"
    
      
    )
  )
)

shinyApp(ui, server)
