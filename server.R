source("./controllers.R")




server = function(input, output, session) {


  fileToMatrix = reactive({
  
    if (is.null(input$fileInput)) return(NULL)
    fileToRead = input$fileInput
    df = read.csv(input$fileInput$datapath, header=TRUE)
    return(as.matrix(df, nrow = 1, ncol = 1))
    
  })
  
  
  output$fileContentsPR = renderTable({

    if (is.null(fileToMatrix())) return(NULL)
    
    matrixOutput = fileToMatrix()
    updateSliderInput(session, 
      "degreeN", 
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
  

  
  answerPR = eventReactive(input$solveButton, {
    
    if (is.null(fileToMatrix())) return(NULL)
    matrixHandler = fileToMatrix()
    result = PolynomialRegression(matrixHandler[,1], matrixHandler[,2], input$degreeN)
    return(result)
    
  })
  
  output$answerFunctionPR = renderText({
    answerPR()
  })
  
}


