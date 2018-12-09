source("./controllers.R")

library(shinyjs)


server = function(input, output, session) {

  useShinyjs()
  
  fileToMatrixPR = reactive({
    if (is.null(input$fileInputPR)) return(NULL)
    fileToRead = input$fileInputPR
    df = read.csv(input$fileInputPR$datapath, header=input$headerCheckPR, sep=",", quote="")
    return(as.matrix(df, nrow = 1, ncol = 1))
  })
  
  output$fileContentsPR = renderTable({
    if (is.null(fileToMatrixPR())) return(NULL)
    vectorNames = c("x", "y")
    matrixOutput = fileToMatrixPR()
    updateSliderInput(session, 
      "degreeNPR", 
      label = "Degree",
      min = 1, 
      max = length(matrixOutput[, 1]) - 1,
      step = 1,
      value=0
    )
    
    colnames(matrixOutput) = vectorNames
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

  output$answerGivenX = eventReactive(input$solveXPR, {
    if (is.null(fileToMatrixPR())) return(NULL)
    show("ansLabel")
    round(PRSolver(fileToMatrixPR()[,1], fileToMatrixPR()[,2], input$degreeNPR, input$xInputPR), digits=4)
  })
  

  
  
  
  fileToMatrixQSI = reactive({
    if (is.null(input$fileInputQSI)) return(NULL)
    fileToRead = input$fileInputQSI
    df = read.csv(fileToRead$datapath, header=input$headerCheckQSI, sep=",", quote="")
    toReturn = as.matrix(df, nrow = 1, ncol = 1)
    return(toReturn)
  })
  
  output$fileContentsQSI = renderTable({
    if (is.null(fileToMatrixQSI())) return(NULL)
    vectorNames = c("x", "y")
    matrixOutput = fileToMatrixQSI()
    colnames(matrixOutput) = vectorNames
    if(input$sortedXQSI) {
      matrixOutput = matrixOutput[order(matrixOutput[,1]), ]
    }
    if(!input$dispAllQSI)  {
      return(head(matrixOutput))
    } else {
      return(matrixOutput)
    }
  })
  
  generateFunctionsQSI = eventReactive(input$solveButtonQSI, {
  
    if (is.null(fileToMatrixQSI())) return(NULL)
    matrixHandler = fileToMatrixQSI()
    resultQSI = QSI(matrixHandler[,1], matrixHandler[,2])
    show("xInputQSI")
    show("solveXQSI")
    return(resultQSI$functionSet)
    
  })
  
  output$generatedFunctions = renderTable({
    toReturn = generateFunctionsQSI()
    vectorNames = c("Interval", "Function", "Range")
    colnames(toReturn) = vectorNames
    return(toReturn)
  })
  
  output$answerGivenXQSI = eventReactive(input$solveXQSI, {
    if (is.null(fileToMatrixQSI())) return(NULL)
    show("ansLabelQSI")
    round(QSISolver(fileToMatrixQSI()[,1], fileToMatrixQSI()[,2], input$xInputQSI), digits=4)
  })
  
}


