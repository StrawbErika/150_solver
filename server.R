source("./controllers.R")

library(shinyjs)


server = function(input, output, session) {

  useShinyjs()
  
  fileToMatrixPR = reactive({
    if (is.null(input$fileInputPR)) return(NULL)
    fileToRead = input$fileInputPR
    df = read.csv(input$fileInputPR$datapath, header=TRUE)
    return(as.matrix(df, nrow = 1, ncol = 1))
  })
  
  output$fileContentsPR = renderTable({
    if (is.null(fileToMatrixPR())) return(NULL)
    matrixOutput = fileToMatrixPR()
    updateSliderInput(session, 
      "degreeNPR", 
      label = "Degree",
      min = 1, 
      max = length(matrixOutput[, 1]) - 1,
      step = 1,
      value=0
    )
    if(input$sortedXPR) {
      matrixOutput = matrixOutput[order(matrixOutput[,1]), ]
    }
    if(!input$dispAllPR)  {
      return(head(matrixOutput))
    } else {
      return(matrixOutput)
    }
  })
  
  getFunction = eventReactive(input$solveButtonPR, {

    if (is.null(fileToMatrixPR())) return(NULL)
    matrixHandler = fileToMatrixPR()
    result = PolynomialRegression(matrixHandler[,1], matrixHandler[,2], input$degreeNPR)
    show("funcLabel")
    show("xInputPR")
    show("solveXPR")
    return(result)

  })
  
  getFunctionText = reactive({
    
    if(is.null(getFunction())) return(NULL)
    functionText = getFunction()$textForm
    return(functionText)
    
  })
    
  output$answerFunctionPR = renderText({
    getFunctionText()
  })

  
  solveForX = reactive({
    
    if(is.null(getFunction())) return(NULL)
    return(getFunction()$functionForm(input$xInputPR))
    
  })
  
  output$answerGivenX = eventReactive(input$solveXPR, {
    show("ansLabel")
    solveForX()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  fileToMatrixQSI = reactive({
    if (is.null(input$fileInputQSI)) return(NULL)
    fileToRead = input$fileInputQSI
    df = read.csv(input$fileInputQSI$datapath, header=TRUE)
    return(as.matrix(df, nrow = 1, ncol = 1))
  })
  
  output$fileContentsQSI = renderTable({
    if (is.null(fileToMatrixQSI())) return(NULL)
    matrixOutput = fileToMatrixQSI()
    updateSliderInput(session, 
      "degreeNQSI", 
      label = "Order",
      min = 1, 
      max = length(matrixOutput[, 1]) - 1,
      step = 1,
      value=0
    )
    if(input$sortedXQSI) {
      matrixOutput = matrixOutput[order(matrixOutput[,1]), ]
    }
    if(!input$dispAllQSI)  {
      return(head(matrixOutput))
    } else {
      return(matrixOutput)
    }
  })
  
  
  
}


