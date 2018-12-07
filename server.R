source("./controllers.R")

server = function(input, output, session) {

  matrixOutput = NULL
  
  output$fileContents = renderTable({

    req(input$fileInput)

    tryCatch(
      { df = read.csv(input$fileInput$datapath, header=TRUE) },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # Translate to matrix
    matrixOutput = as.matrix(df, nrow = 1, ncol = 1)
    print("wat")
    updateSliderInput(session, 
      "integer", 
      label = "Degree",
      min = 1, 
      max = length(matrixOutput[, 1]) - 1,
      step = 1,
      value=0
    )
    
    if(input$sorted) {
      matrixOutput = matrixOutput[order(matrixOutput[,1]), ]
    }
    
    if(!input$dispAll)  {
      # print(matrixOutput)
      return(head(matrixOutput))
    } else {
      return(matrixOutput)
    }
  })
  
  
}


